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
--
--   Code Block Segmentation (CBS) parameters are computed combinationally
--   in BP_TLV_HEADER from the TB size.  CRC-24B accumulation for each CB
--   runs in parallel with TB CRC accumulation during BP_TLV_DATA — no
--   separate post-processing state machine is needed.
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
import NewRadioCRC (resetNrCrc, selectCrcType, updateNrCrc, finalizeNrCrc,
                    rangeCrc24BStep, updateCbCrcWithTbCrc)
import NrCbSegment (computeCbParams)

-- =============================================================================
-- Progressive Body Parser
-- =============================================================================
--
-- CRC checking is woven into the TLV data parsing path: when the parser
-- identifies dwords belonging to the Transport Block data region of an
-- inline TLV payload, each dword is fed through the NewRadioCRC parallel
-- CRC engine.  The CRC is fully computed by the time the last TB dword is
-- consumed — no extra latency.
--
-- CBS: in BP_TLV_HEADER, computeCbParams derives the base graph, number of
-- code blocks C, and per-CB payload dword count kDw from tbLenBytes.
-- In BP_TLV_DATA, a CRC-24B accumulator tracks each CB in parallel with the
-- TB CRC.  When a CB boundary is crossed (cbDwNext >= kDw), the CB CRC is
-- finalized and the CB is marked ready.  The last CB is finalized on tlvDone.

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
    -- This is the TB size in bytes — use it to select the CRC type and
    -- compute CBS parameters (base graph, C, kDw) combinationally.
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

          -- Select TB CRC type per 3GPP TS 38.212
          crcType = selectCrcType tbLenBytes
          crc'    = (tpCrcState st) { ncCrcType = crcType }

          -- Compute CBS parameters combinationally from TB size
          (_, cbC, cbKDwCb0, cbSplitCb0, cbKDwCb1, cbSplitCb1, cbKBits, cbFiller) =
              computeCbParams tbLenBytes
          cbCrcInit = nullNrCrcState { ncCrcType = NR_CRC24B }

      in st { tpInfo         = tdi { tdPdus = newPdus }
            , tpTlvLenDw     = clampedLen
            , tpTlvDwRead    = 0
            , tpPduRemainDw  = tpPduRemainDw st - 1
            , tpCrcState     = crc'
            , tpPhase        = if clampedLen == 0
                                 then BP_PDU_HEADER
                                 else BP_TLV_DATA
            -- CBS
            , tpCbNumCbs     = cbC
            , tpCbPayDw      = cbKDwCb1
            , tpCbPayBits    = cbKBits
            , tpCbSplitBit   = cbSplitCb1
            , tpCbPayDwCb0   = cbKDwCb0
            , tpCbSplitBitCb0 = cbSplitCb0
            , tpCbFillerBits = cbFiller
            , tpCbIsFirst    = True
            , tpCbDwInBlock  = 0
            , tpCbCrcState   = cbCrcInit
            }

    -- TLV value: capture TB payload dwords and feed through CRC engines.
    -- TB CRC (CRC-24A or CRC-16) and per-CB CRC-24B run in parallel.
    BP_TLV_DATA ->
      let tag      = tpTlvTag st
          wIdx     = tpTbWriteIdx st
          isInline = tag == 0 || tag == 3

          -- TB buffer write
          tbBuf  = tpTbBuffer st
          newBuf = if isInline && wIdx < maxTbDwords
                     then tbBuf { tbData = replace wIdx dw (tbData tbBuf) }
                     else tbBuf
          newWIdx = if isInline && wIdx < maxTbDwords
                      then wIdx + 1
                      else wIdx

          tlvRead = tpTlvDwRead st + 1
          tlvDone = tlvRead >= tpTlvLenDw st

          -- TB CRC update
          crc' = if isInline
                   then updateNrCrc (tpCrcState st) dw
                   else tpCrcState st

          pduRemain = tpPduRemainDw st - 1
          pduNum    = tpCurPdu st
          nPdus     = unpack (tdNumPdus (tpInfo st)) :: Unsigned 16

          -- -------------------------------------------------------------------
          -- CB tracking (parallel to TB CRC)
          -- -------------------------------------------------------------------

          cbDwPos  = tpCbDwInBlock st
          cbDwNext = cbDwPos + 1

          -- Select boundary parameters based on whether CB 0 has been completed
          curPayDw = if tpCbIsFirst st then tpCbPayDwCb0 st else tpCbPayDw st
          cbSplit  = if tpCbIsFirst st then tpCbSplitBitCb0 st else tpCbSplitBit st

          -- CB boundary detection
          cbBoundaryDw = cbDwNext >= curPayDw && tpCbNumCbs st > 1
          cbNeedsSplit = cbBoundaryDw && cbSplit > 0

          -- CB CRC-24B update: depends on whether this is a split dword
          cbCrc' = if not isInline || tpCbNumCbs st <= 1
                     then tpCbCrcState st
                     else if cbNeedsSplit
                       -- Split: only process first `splitBit` bits for current CB
                       then (tpCbCrcState st)
                              { ncCrc24BReg = rangeCrc24BStep
                                  (ncCrc24BReg (tpCbCrcState st)) 0 (resize cbSplit) dw }
                       else updateNrCrc (tpCbCrcState st) dw   -- full 32-bit step

          -- For split dwords: start next CB with remaining bits
          cbCrcNext = if cbNeedsSplit
                        then nullNrCrcState
                               { ncCrcType   = NR_CRC24B
                               , ncCrc24BReg = rangeCrc24BStep
                                   0 (resize cbSplit) 32 dw }
                        else freshCbCrc   -- dword-aligned boundary: fresh start

          -- Finalized CRC-24B value (used for both intermediate and last CB)
          cbCrcVal = finalizeNrCrc cbCrc'

          -- Fresh CRC-24B accumulator for the next CB
          freshCbCrc = nullNrCrcState { ncCrcType = NR_CRC24B }

      in if tlvDone
           then
             -- TLV complete: finalize TB CRC, write it into the TB buffer,
             -- and finalize the last (or only) CB.
             let crcValue = finalizeNrCrc crc'

                 -- For C > 1: feed the raw TB CRC register (exact width)
                 -- into the last CB CRC-24B accumulator.  Spec §5.2.2:
                 -- b = [TB payload | TB CRC]; the last CB's slice covers TB CRC.
                 cbCrc'' = if tpCbNumCbs st > 1
                             then updateCbCrcWithTbCrc crc' cbCrc'
                             else cbCrc'
                 lastCbCrcVal = finalizeNrCrc cbCrc''

                 -- Buffer tail layout:
                 --   C > 1: [TB CRC dword] [last CB CRC-24B]  (TB CRC is part of b)
                 --   C = 1: handled below via bufWithCrc
                 (tbBufPreCrc, crcWIdx) =
                   if tpCbNumCbs st > 1
                     then let p0  = newWIdx
                              buf0 = if p0 < maxTbBufDwords
                                       then newBuf { tbData = replace p0 crcValue (tbData newBuf) }
                                       else newBuf
                              p1  = if p0 < maxTbBufDwords then p0 + 1 else p0
                              buf1 = if p1 < maxTbBufDwords
                                       then buf0 { tbData = replace p1 lastCbCrcVal (tbData buf0) }
                                       else buf0
                              p2  = if p1 < maxTbBufDwords then p1 + 1 else p1
                          in (buf1, p2)
                     else (newBuf, newWIdx)

                 -- C = 1: write TB CRC now; C > 1: already written above
                 bufWithCrc  = if tpCbNumCbs st > 1
                                 then tbBufPreCrc
                                 else if crcWIdx < maxTbBufDwords
                                        then tbBufPreCrc { tbData = replace crcWIdx crcValue
                                                                     (tbData tbBufPreCrc) }
                                        else tbBufPreCrc
                 wIdxAfterCrc = if tpCbNumCbs st > 1
                                  then crcWIdx
                                  else if crcWIdx < maxTbBufDwords
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
                          , tpCbDwInBlock = cbDwNext
                          , tpCbCrcState  = cbCrc'
                          }
                  else st { tpTbBuffer    = bufWithCrc
                          , tpTbWriteIdx  = wIdxAfterCrc
                          , tpTlvDwRead   = tlvRead
                          , tpPduRemainDw = pduRemain
                          , tpCrcState    = crc'
                          , tpInfo        = tdi { tdPdus = newPdus }
                          , tpCurPdu      = nextPdu
                          , tpPhase       = BP_PDU_HEADER
                          , tpCbDwInBlock = cbDwNext
                          , tpCbCrcState  = cbCrc'
                          }

           else if cbBoundaryDw
                  then
                    -- Intermediate CB complete: write CB CRC-24B into TB buffer,
                    -- reset for next CB (using cbCrcNext which may contain
                    -- remaining split-dword bits).
                    let cbCrcWIdx      = newWIdx
                        tbBufWithCbCrc = if cbCrcWIdx < maxTbBufDwords
                                           then newBuf { tbData = replace cbCrcWIdx cbCrcVal (tbData newBuf) }
                                           else newBuf
                        wIdxAfterCbCrc = if cbCrcWIdx < maxTbBufDwords
                                           then cbCrcWIdx + 1
                                           else cbCrcWIdx
                    in st { tpTbBuffer    = tbBufWithCbCrc
                          , tpTbWriteIdx  = wIdxAfterCbCrc
                          , tpTlvDwRead   = tlvRead
                          , tpPduRemainDw = pduRemain
                          , tpCrcState    = crc'
                          , tpCbDwInBlock = 0
                          , tpCbCrcState  = cbCrcNext
                          , tpCbIsFirst   = False
                          }
                  else
                    -- Normal dword: continue building current CB.
                    st { tpTbBuffer    = newBuf
                       , tpTbWriteIdx  = newWIdx
                       , tpTlvDwRead   = tlvRead
                       , tpPduRemainDw = pduRemain
                       , tpCrcState    = crc'
                       , tpCbDwInBlock = cbDwNext
                       , tpCbCrcState  = cbCrc'
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
