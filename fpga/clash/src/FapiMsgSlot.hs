{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

-- | Generic slot-level message processing.
--
--   Handles UL_TTI.request, UL_DCI.request, and any other slot-level
--   message that doesn't require deep body parsing.  These are
--   acknowledged with a SLOT.indication containing the echoed SFN/slot.
--
--   No body parsing is performed — only the first body dword
--   (SFN/slot, captured by the main FSM) is used.

module FapiMsgSlot
  ( processSlotMsg
  ) where

import Clash.Prelude
import GNodeBFAPITypes

-- =============================================================================
-- Slot Message Processing
-- =============================================================================
--
-- Pure combinational.  Requires PHY to be in RUNNING state.

processSlotMsg :: PhyState -> FapiParsedReq -> FapiRespPayload
processSlotMsg curPhy req =
  let handle  = prHandle  req
      phyId   = prPhyId   req
      bodyDw0 = prBodyDw0 req
      sfn     = slice d31 d16 bodyDw0
      slot    = slice d15 d0  bodyDw0

      slotOkResp = FapiRespPayload
        { rpValid    = 1
        , rpMsgType  = fapiMsgTypeId FAPI_SLOT_INDICATION
        , rpHandle   = handle
        , rpPhyId    = phyId
        , rpMsgLen   = 8    -- sfn(2) + slot(2) + err(1) + phy(1) + pad(2)
        , rpErrCode  = fapiErrorCodeVal FAPI_MSG_OK
        , rpPhyState = phyStateToVal curPhy
        , rpSfn      = sfn
        , rpSlot     = slot
        }

      errResp = FapiRespPayload
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

  in case curPhy of
    PHY_RUNNING -> slotOkResp
    _           -> errResp
