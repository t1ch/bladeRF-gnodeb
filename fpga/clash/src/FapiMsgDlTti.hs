{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

-- | DL_TTI.request (0x80) body parser and DL_TTI.response builder.
--
--   Progressively parses the DL_TTI.request body dword-by-dword,
--   extracting PDSCH scheduling info for downstream TB correlation.
--
--   NOTE: CRC computation on Transport Block data is handled in
--   FapiMsgTxData, where the actual TB payload is received via
--   TX_DATA.request.  DL_TTI.request only carries scheduling
--   metadata (BWP, RNTI, TB size) — not the TB data itself.
--
--   SCF-222 Table 3.4.2-1 body layout (simplified for PoC):
--
--     Body DW0: [SFN(16) | Slot(16)]
--     Body DW1: [nPDUs(16) | nDlTypes(8) | pad(8)]
--     (skip numPDUsOfEachType array + numGroups + rate-match structs)
--     Per PDU:
--       [pduType(16) | pduSize(16)]
--       PDSCH body DW0: [bwpSize(16) | bwpStart(16)]
--       PDSCH body DW1: [scs(8) | cp(8) | pduIndex(16)]
--       PDSCH body DW2: [rnti(16) | pad(16)]
--       PDSCH body DW3: [tbSizeBytes(32)]
--
--   Response: DL_TTI.response (0x8A) per SCF-222 Table 3.4.2b-1:
--     Body DW0: [SFN(16) | Slot(16)]
--     Body DW1: [nPDUs(8) | nPdsch(8) | errCode(8) | phyState(8)]

module FapiMsgDlTti
  ( parseDlTtiDword
  , processDlTtiResponse
  , buildDlTtiCdcBody
  ) where

import Clash.Prelude
import GNodeBFAPITypes
import FapiDlPduPdsch  (parsePdschBodyDw)
import FapiDlPduPdcch  (parsePdcchBodyDw)
import FapiDlPduSsb    (parseSsbBodyDw)
import FapiDlPduCsiRs  (parseCsiRsBodyDw)

-- =============================================================================
-- Progressive Body Parser
-- =============================================================================
--
-- Called once per body dword.  Reads from and writes to DlTtiParseState.
-- The main FSM calls this for each data_valid dword when the message
-- type is DL_TTI.request.
--
-- This parser extracts scheduling metadata only.  TB CRC checking is
-- performed in FapiMsgTxData where the Transport Block payload arrives.

parseDlTtiDword :: DlTtiParseState -> BitVector 32 -> Unsigned 16
                -> DlTtiParseState
parseDlTtiDword st dw _bodyDwIdx =
  case dpPhase st of
    BP_HEADER ->
      case dpBodyDwIdx st of
        -- Body DW0: [SFN(16) | Slot(16)]
        0 -> let sfn  = slice d31 d16 dw
                 slot = slice d15 d0  dw
                 dti  = (dpInfo st) { dtSfn = sfn, dtSlot = slot }
             in st { dpInfo = dti, dpBodyDwIdx = 1 }

        -- Body DW1: [nPDUs(16) | nDlTypes(8) | pad(8)]
        1 -> let nPdus = slice d31 d16 dw
                 dti   = (dpInfo st) { dtNumPdus = nPdus }
                 -- PoC: skip 2 dwords (numPDUsOfEachType + numGroups)
                 skipDw = 2 :: Unsigned 16
             in st { dpInfo      = dti
                   , dpPhase     = BP_SKIP_FIXED
                   , dpSkipRemain = skipDw
                   , dpBodyDwIdx = 2
                   }
        _ -> st

    BP_SKIP_FIXED ->
      let remain = dpSkipRemain st
      in if remain <= 1
           then let nPdus = unpack (dtNumPdus (dpInfo st)) :: Unsigned 16
                in if nPdus == 0
                     then st { dpPhase = BP_DONE }
                     else st { dpPhase     = BP_PDU_HEADER
                             , dpSkipRemain = 0
                             , dpCurPduIdx = 0
                             }
           else st { dpSkipRemain = remain - 1 }

    BP_PDU_HEADER ->
      -- [pduType(16) | pduSize(16)]
      let pduType = slice d31 d16 dw
          pduSize = slice d15 d0  dw
          bodySizeBytes = let s = unpack pduSize :: Unsigned 16
                          in if s > 4 then s - 4 else 0
          bodyDwords = (bodySizeBytes + 3) `div` 4
      in st { dpCurPduType   = pduType
            , dpCurPduSizeDw = bodyDwords
            , dpCurPduDwRead = 0
            , dpPhase        = if bodyDwords == 0
                                 then BP_PDU_HEADER
                                 else BP_PDU_BODY
            }

    BP_PDU_BODY ->
      let dwRead  = dpCurPduDwRead st + 1
          pduType = dlPduTypeFromId (dpCurPduType st)
          dti     = dpInfo st

          -- Dispatch to per-PDU-type body parser.
          -- Each parser receives the current accumulated info record,
          -- the intra-PDU dword index, and the dword value.
          -- Counters (dtNumPdsch etc.) are used as write indices into the
          -- respective Vec and are only incremented in the finalize block below.
          st' = case pduType of
            DL_PDU_PDSCH ->
              let nIdx   = dtNumPdsch dti
                  pi'    = parsePdschBodyDw (dtPdsch dti !! nIdx)
                                            (dpCurPduDwRead st) dw
              in st { dpInfo = dti { dtPdsch = replace nIdx pi' (dtPdsch dti) } }

            DL_PDU_PDCCH ->
              let nIdx   = dtNumPdcch dti
                  pc'    = parsePdcchBodyDw (dtPdcch dti !! nIdx)
                                            (dpCurPduDwRead st) dw
              in st { dpInfo = dti { dtPdcch = replace nIdx pc' (dtPdcch dti) } }

            DL_PDU_SSB ->
              let nIdx   = dtNumSsb dti
                  sb'    = parseSsbBodyDw (dtSsb dti !! nIdx)
                                          (dpCurPduDwRead st) dw
              in st { dpInfo = dti { dtSsb = replace nIdx sb' (dtSsb dti) } }

            DL_PDU_CSI_RS ->
              let nIdx   = dtNumCsiRs dti
                  cr'    = parseCsiRsBodyDw (dtCsiRs dti !! nIdx)
                                            (dpCurPduDwRead st) dw
              in st { dpInfo = dti { dtCsiRs = replace nIdx cr' (dtCsiRs dti) } }

            _ -> st  -- PRS, OCNG, RIM_RS, RB_AGG: skip gracefully

          -- Check if PDU body is fully consumed
          allRead = dwRead >= dpCurPduSizeDw st
          pduIdx  = dpCurPduIdx st + 1
          nPdus   = unpack (dtNumPdus (dpInfo st')) :: Unsigned 16

          -- When a PDU is fully consumed, advance its type-specific counter.
          finalizePdsch  = allRead && pduType == DL_PDU_PDSCH
          finalizePdcch  = allRead && pduType == DL_PDU_PDCCH
          finalizeSsb    = allRead && pduType == DL_PDU_SSB
          finalizeCsiRs  = allRead && pduType == DL_PDU_CSI_RS

          dti' = dpInfo st'
          dtiOut = dti'
            { dtNumPdsch = if finalizePdsch && dtNumPdsch dti' < maxPdschPerSlot
                             then dtNumPdsch dti' + 1 else dtNumPdsch dti'
            , dtNumPdcch = if finalizePdcch && dtNumPdcch dti' < maxPdcchPerSlot
                             then dtNumPdcch dti' + 1 else dtNumPdcch dti'
            , dtNumSsb   = if finalizeSsb   && dtNumSsb   dti' < maxSsbPerSlot
                             then dtNumSsb   dti' + 1 else dtNumSsb   dti'
            , dtNumCsiRs = if finalizeCsiRs && dtNumCsiRs dti' < maxCsiRsPerSlot
                             then dtNumCsiRs dti' + 1 else dtNumCsiRs dti'
            }

      in if allRead
           then if pduIdx >= nPdus
                  then st' { dpCurPduDwRead = dwRead
                           , dpPhase        = BP_DONE
                           , dpInfo         = dtiOut
                           }
                  else st' { dpCurPduDwRead = 0
                           , dpCurPduIdx    = pduIdx
                           , dpPhase        = BP_PDU_HEADER
                           , dpInfo         = dtiOut
                           }
           else st' { dpCurPduDwRead = dwRead }

    BP_DONE -> st
    _       -> st

-- =============================================================================
-- Response Builder
-- =============================================================================

processDlTtiResponse :: PhyState -> FapiParsedReq -> DlTtiInfo
                     -> FapiRespPayload
processDlTtiResponse curPhy req dlTti =
  let handle = prHandle req
      phyId  = prPhyId  req
  in case curPhy of
    PHY_RUNNING -> FapiRespPayload
      { rpValid    = 1
      , rpMsgType  = fapiMsgTypeId FAPI_DL_TTI_RESPONSE
      , rpHandle   = handle
      , rpPhyId    = phyId
      , rpMsgLen   = 8
      , rpErrCode  = fapiErrorCodeVal FAPI_MSG_OK
      , rpPhyState = phyStateToVal curPhy
      , rpSfn      = dtSfn dlTti
      , rpSlot     = dtSlot dlTti
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

-- =============================================================================
-- CDC Body Builder
-- =============================================================================
--
-- For DL_TTI.response, body DW1 includes PDU counts:
--   [nPDUs(8) | nPdsch(8) | errCode(8) | phyState(8)]

buildDlTtiCdcBody :: FapiRespPayload -> DlTtiInfo
                  -> (BitVector 32, BitVector 32)
buildDlTtiCdcBody rp dlTti =
  let bdw0 = rpSfn rp ++# rpSlot rp
      nPdusField  = resize (dtNumPdus dlTti) :: BitVector 8
      nPdschField = pack (resize (dtNumPdsch dlTti) :: Unsigned 8) :: BitVector 8
      bdw1 = nPdusField ++# nPdschField ++# rpErrCode rp ++# rpPhyState rp
  in (bdw0, bdw1)
