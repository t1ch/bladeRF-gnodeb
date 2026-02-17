{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

-- | P5 Configuration message processing.
--
--   Handles PARAM, CONFIG, START, STOP requests.
--   These messages have no body to parse (or a trivially small body);
--   processing is purely combinational based on the current PHY state
--   and the message type from the common header.
--
--   SCF-222 §3.3:
--     PARAM.request  (0x00) → PARAM.response  (0x01)  |  ERROR.indication
--     CONFIG.request (0x02) → CONFIG.response (0x03)  |  ERROR.indication
--     START.request  (0x04) → ERROR.indication (OK)    (transitions to RUNNING)
--     STOP.request   (0x05) → STOP.indication (0x06)  |  ERROR.indication

module FapiMsgConfig
  ( processConfigMsg
  , nextPhyState
  ) where

import Clash.Prelude ()
import GNodeBFAPITypes

-- =============================================================================
-- Config Request Processing
-- =============================================================================
--
-- Pure combinational: given PHY state + parsed header, produce response.
-- Returns (response_payload, new_phy_state).

processConfigMsg :: PhyState -> FapiParsedReq -> (FapiRespPayload, PhyState)
processConfigMsg curPhy req =
  let handle = prHandle req
      phyId  = prPhyId  req
      msgType = fapiMsgTypeFromId (prMsgType req)

      -- Error response template
      cfgErrResp newPhy = FapiRespPayload
        { rpValid    = 1
        , rpMsgType  = fapiMsgTypeId FAPI_ERROR_INDICATION
        , rpHandle   = handle
        , rpPhyId    = phyId
        , rpMsgLen   = 4   -- err(1) + phy(1) + reserved(2)
        , rpErrCode  = fapiErrorCodeVal FAPI_MSG_INVALID_STATE
        , rpPhyState = phyStateToVal newPhy
        , rpSfn      = 0
        , rpSlot     = 0
        }

      -- OK response template
      cfgOkResp rspType newPhy = FapiRespPayload
        { rpValid    = 1
        , rpMsgType  = fapiMsgTypeId rspType
        , rpHandle   = handle
        , rpPhyId    = phyId
        , rpMsgLen   = 4
        , rpErrCode  = fapiErrorCodeVal FAPI_MSG_OK
        , rpPhyState = phyStateToVal newPhy
        , rpSfn      = 0
        , rpSlot     = 0
        }

  in case msgType of
    FAPI_PARAM_REQUEST -> case curPhy of
      PHY_IDLE       -> (cfgOkResp FAPI_PARAM_RESPONSE PHY_IDLE,       PHY_IDLE)
      PHY_CONFIGURED -> (cfgOkResp FAPI_PARAM_RESPONSE PHY_CONFIGURED, PHY_CONFIGURED)
      PHY_RUNNING    -> (cfgErrResp PHY_RUNNING,                       PHY_RUNNING)

    FAPI_CONFIG_REQUEST -> case curPhy of
      PHY_IDLE       -> (cfgOkResp FAPI_CONFIG_RESPONSE PHY_CONFIGURED, PHY_CONFIGURED)
      PHY_CONFIGURED -> (cfgOkResp FAPI_CONFIG_RESPONSE PHY_CONFIGURED, PHY_CONFIGURED)
      PHY_RUNNING    -> (cfgErrResp PHY_RUNNING,                        PHY_RUNNING)

    FAPI_START_REQUEST -> case curPhy of
      PHY_CONFIGURED ->
        let resp = (cfgOkResp FAPI_ERROR_INDICATION PHY_RUNNING)
                     { rpErrCode = fapiErrorCodeVal FAPI_MSG_OK }
        in (resp, PHY_RUNNING)
      _ -> (cfgErrResp curPhy, curPhy)

    FAPI_STOP_REQUEST -> case curPhy of
      PHY_RUNNING -> (cfgOkResp FAPI_STOP_INDICATION PHY_IDLE, PHY_IDLE)
      _           -> (cfgErrResp curPhy,                        curPhy)

    _ -> (cfgErrResp curPhy, curPhy)

-- =============================================================================
-- PHY State Transition (reusable by all modules)
-- =============================================================================

nextPhyState :: PhyState -> FapiMsgType -> PhyState
nextPhyState curPhy msgType = case msgType of
  FAPI_CONFIG_REQUEST -> case curPhy of
    PHY_IDLE       -> PHY_CONFIGURED
    PHY_CONFIGURED -> PHY_CONFIGURED
    _              -> curPhy
  FAPI_START_REQUEST -> case curPhy of
    PHY_CONFIGURED -> PHY_RUNNING
    _              -> curPhy
  FAPI_STOP_REQUEST -> case curPhy of
    PHY_RUNNING    -> PHY_IDLE
    _              -> curPhy
  _ -> curPhy
