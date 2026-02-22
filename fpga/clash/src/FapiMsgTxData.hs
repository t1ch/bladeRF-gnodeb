{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

-- | TX_DATA.request (0x84) body parser and transport block capture.
--
--   Progressively parses the TX_DATA.request body dword-by-dword.
--   For inline payloads (TLV tag=0 or tag=3), captures the transport
--   block data into the TB buffer for downstream RF processing.
--
--   CRC is computed incrementally on Transport Block data as it streams
--   through the TLV parser, delegating to the NewRadioCRC module.
--   This is the correct location for CRC since the actual TB payload
--   is carried in TX_DATA.request (not in DL_TTI.request, which only
--   contains scheduling metadata).
--
--   SCF-222 Table 3.4.6-1 body layout:
--
--     Body DW0: [SFN(16) | Slot(16)]
--     Body DW1: [controlLength(16) | nPDUs(16)]
--     Per PDU:
--       [pduLength(32)]
--       [pduIndex(16) | cwIndex(8) | pad(8)]
--       [numTLV(32)]
--       [tag(16) | pad(16)]
--       [length(32)]          ← TB size in bytes
--       [value dwords...]     ← inline TB payload (tag 0/3)
--
--   Response: SLOT.indication acknowledging receipt.

module FapiMsgTxData
  ( parseTxDataDword
  , processTxDataResponse
  ) where

import Clash.Prelude
import GNodeBFAPITypes
import NewRadioCRC (resetNrCrc, selectCrcType, updateNrCrc, finalizeNrCrc)

-- =============================================================================
-- Progressive Body Parser
-- =============================================================================
--
-- CRC checking is woven into the TLV data parsing path: when the parser
-- identifies dwords belonging to the Transport Block data region of an
-- inline TLV payload, each dword is fed through the NewRadioCRC parallel
-- CRC engine. The CRC is fully computed by the time the last TB dword is
-- consumed — no extra latency.

parseTxDataDword :: TxDataParseState -> BitVector 32 -> Unsigned 16
                 -> TxDataParseState
parseTxDataDword st dw _bodyDwIdx =
  case tpPhase st of
    BP_HEADER ->
      case tpBodyDwIdx st of
        -- Body DW0: [SFN(16) | Slot(16)]
        0 -> let sfn  = slice d31 d16 dw
                 slot = slice d15 d0  dw
                 tdi  = (tpInfo st) { tdSfn = sfn, tdSlot = slot }
             in st { tpInfo = tdi, tpBodyDwIdx = 1 }

        -- Body DW1: [controlLength(16) | nPDUs(16)]
        1 -> let nPdus = slice d15 d0 dw
                 tdi   = (tpInfo st) { tdNumPdus = nPdus }
             in if nPdus == 0
                  then st { tpInfo = tdi, tpPhase = BP_DONE }
                  else st { tpInfo      = tdi
                          , tpPhase     = BP_PDU_HEADER
                          , tpCurPdu    = 0
                          , tpBodyDwIdx = 2
                          }
        _ -> st

    -- Per-PDU: reading pduLength(32)
    BP_PDU_HEADER ->
      let pduLenBytes = unpack dw :: Unsigned 32
          -- Reset CRC accumulators at the start of each new PDU
          crcReset = resetNrCrc (tpCrcState st)
      in st { tpPduRemainDw  = truncateB ((pduLenBytes + 3) `div` 4) - 1
            , tpPhase        = BP_PDU_BODY
            , tpCurPduDwRead = 0
            , tpCrcState     = crcReset
            }

    BP_PDU_BODY ->
      let dwRead = tpCurPduDwRead st
          tdi    = tpInfo st
          pduNum = tpCurPdu st
      in case dwRead of
        -- PDU body DW0: [pduIndex(16) | cwIndex(8) | pad(8)]
        0 -> let pduIndex = slice d31 d16 dw
                 cwIndex  = slice d15 d8  dw
                 tp = nullTxDataPdu
                   { txpValid    = 1
                   , txpPduIndex = pduIndex
                   , txpCwIndex  = cwIndex
                   , txpTbOffset = tpTbWriteIdx st
                   }
                 newPdus = replace pduNum tp (tdPdus tdi)
             in st { tpInfo         = tdi { tdPdus = newPdus }
                   , tpCurPduDwRead = 1
                   , tpPduRemainDw  = tpPduRemainDw st - 1
                   }

        -- PDU body DW1: [numTLV(32)]
        1 -> st { tpCurPduDwRead = 2
                , tpPduRemainDw  = tpPduRemainDw st - 1
                }

        -- PDU body DW2: [tag(16) | pad(16)]
        2 -> let tag = slice d31 d16 dw
             in st { tpTlvTag       = tag
                   , tpCurPduDwRead = 3
                   , tpPduRemainDw  = tpPduRemainDw st - 1
                   , tpPhase        = BP_TLV_HEADER
                   }

        _ -> st

    -- TLV: reading length(32)
    -- This is the TB size in bytes — use it to select the CRC type
    -- (CRC-24A for TB > 3824 bits / 478 bytes, CRC-16 otherwise).
    BP_TLV_HEADER ->
      let tbLenBytes = dw
          tbLenDw = ((unpack tbLenBytes :: Unsigned 32) + 3) `div` 4
          clampedLen = if tbLenDw > resize maxTbDwords
                         then maxTbDwords
                         else truncateB tbLenDw
          tdi    = tpInfo st
          pduNum = tpCurPdu st
          curTp  = tdPdus tdi !! pduNum
          updTp  = curTp { txpTbLenBytes = tbLenBytes }
          newPdus = replace pduNum updTp (tdPdus tdi)

          -- Select CRC type based on TB size per 3GPP TS 38.212
          crcType = selectCrcType tbLenBytes
          crc'    = (tpCrcState st) { ncCrcType = crcType }

      in st { tpInfo        = tdi { tdPdus = newPdus }
            , tpTlvLenDw    = clampedLen
            , tpTlvDwRead   = 0
            , tpPduRemainDw = tpPduRemainDw st - 1
            , tpCrcState    = crc'
            , tpPhase       = if clampedLen == 0
                                then BP_PDU_HEADER
                                else BP_TLV_DATA
            }

    -- TLV value: capture TB payload dwords and feed through CRC engine
    BP_TLV_DATA ->
      let tag  = tpTlvTag st
          wIdx = tpTbWriteIdx st
          isInline = tag == 0 || tag == 3

          tbBuf  = tpTbBuffer st
          newBuf = if isInline && wIdx < maxTbDwords
                     then tbBuf { tbData = replace wIdx dw (tbData tbBuf) }
                     else tbBuf
          newWIdx = if isInline && wIdx < maxTbDwords
                      then wIdx + 1
                      else wIdx

          tlvRead = tpTlvDwRead st + 1
          tlvDone = tlvRead >= tpTlvLenDw st

          -- Feed each inline TB data dword through the CRC engine.
          -- Pure MSB-first — no phase tracking or bit reversal needed.
          crc' = if isInline
                   then updateNrCrc (tpCrcState st) dw
                   else tpCrcState st

          pduRemain = tpPduRemainDw st - 1
          pduNum    = tpCurPdu st
          nPdus     = unpack (tdNumPdus (tpInfo st)) :: Unsigned 16

      in if tlvDone
           then
             -- TLV complete: finalize CRC, write it into the TB buffer
             -- after the last data dword, and update PDU info.
             -- CRC is returned as a 32-bit dword with zero-padded MSBs.
             -- (3GPP TS 38.212 Sec 7.2.1)
             let crcValue = finalizeNrCrc crc'

                 -- Append CRC dword into TB buffer at current write index
                 crcWIdx  = newWIdx
                 bufWithCrc = if crcWIdx < maxTbDwords
                                then newBuf { tbData = replace crcWIdx crcValue
                                                         (tbData newBuf) }
                                else newBuf
                 wIdxAfterCrc = if crcWIdx < maxTbDwords
                                  then crcWIdx + 1
                                  else crcWIdx

                 tdi       = tpInfo st
                 curTp     = tdPdus tdi !! pduNum
                 updTp     = curTp { txpCrc = crcValue }
                 newPdus   = replace pduNum updTp (tdPdus tdi)

                 nextPdu = pduNum + 1
                 finalBuf = bufWithCrc
                   { tbLenDwords = wIdxAfterCrc
                   , tbPduIndex  = txpPduIndex (tdPdus (tpInfo st) !! pduNum)
                   , tbSfn       = tdSfn (tpInfo st)
                   , tbSlot      = tdSlot (tpInfo st)
                   , tbCrc       = crcValue
                   , tbReady     = 1
                   }
             in if nextPdu >= nPdus
                  then st { tpTbBuffer    = finalBuf
                          , tpTbWriteIdx  = wIdxAfterCrc
                          , tpTlvDwRead   = tlvRead
                          , tpPduRemainDw = pduRemain
                          , tpCrcState    = crc'
                          , tpInfo        = tdi { tdPdus = newPdus }
                          , tpPhase       = BP_DONE
                          }
                  else st { tpTbBuffer    = bufWithCrc
                          , tpTbWriteIdx  = wIdxAfterCrc
                          , tpTlvDwRead   = tlvRead
                          , tpPduRemainDw = pduRemain
                          , tpCrcState    = crc'
                          , tpInfo        = tdi { tdPdus = newPdus }
                          , tpCurPdu      = nextPdu
                          , tpPhase       = BP_PDU_HEADER
                          }
           else st { tpTbBuffer    = newBuf
                   , tpTbWriteIdx  = newWIdx
                   , tpTlvDwRead   = tlvRead
                   , tpPduRemainDw = pduRemain
                   , tpCrcState    = crc'
                   }

    BP_DONE -> st
    _       -> st

-- =============================================================================
-- Response Builder
-- =============================================================================

processTxDataResponse :: PhyState -> FapiParsedReq -> FapiRespPayload
processTxDataResponse curPhy req =
  let handle  = prHandle  req
      phyId   = prPhyId   req
      bodyDw0 = prBodyDw0 req
      sfn     = slice d31 d16 bodyDw0
      slot    = slice d15 d0  bodyDw0
  in case curPhy of
    PHY_RUNNING -> FapiRespPayload
      { rpValid    = 1
      , rpMsgType  = fapiMsgTypeId FAPI_SLOT_INDICATION
      , rpHandle   = handle
      , rpPhyId    = phyId
      , rpMsgLen   = 8
      , rpErrCode  = fapiErrorCodeVal FAPI_MSG_OK
      , rpPhyState = phyStateToVal curPhy
      , rpSfn      = sfn
      , rpSlot     = slot
      }
    _ -> FapiRespPayload
      { rpValid    = 1
      , rpMsgType  = fapiMsgTypeId FAPI_ERROR_INDICATION
      , rpHandle   = handle
      , rpPhyId    = phyId
      , rpMsgLen   = 4
      , rpErrCode  = fapiErrorCodeVal FAPI_MSG_INVALID_STATE
      , rpPhyState = phyStateToVal curPhy
      , rpSfn      = 0
      , rpSlot     = 0
      }
