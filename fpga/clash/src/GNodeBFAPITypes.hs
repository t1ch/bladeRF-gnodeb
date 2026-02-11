{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module GNodeBFAPITypes
  ( -- * FAPI Message Type IDs (SCF222 P5)
    FapiMsgType(..)
  , fapiMsgTypeId
  , fapiMsgTypeFromId

    -- * PHY State Machine (SCF222 Section 3.2)
  , PhyState(..)
  , phyStateToVal

    -- * FAPI Response
  , FapiResponse(..)
  , nullFapiResponse

    -- * Error Codes
  , FapiErrorCode(..)
  , fapiErrorCodeVal

    -- * Packet Control (bladeRF packet_control_t)
  , PacketControl(..)
  , nullPacketControl
  ) where

import Clash.Prelude

-- =============================================================================
-- FAPI P5 Message Types (SCF222 Table 3-5)
-- =============================================================================

data FapiMsgType
  = FAPI_PARAM_REQUEST
  | FAPI_PARAM_RESPONSE
  | FAPI_CONFIG_REQUEST
  | FAPI_CONFIG_RESPONSE
  | FAPI_START_REQUEST
  | FAPI_STOP_REQUEST
  | FAPI_STOP_INDICATION
  | FAPI_ERROR_INDICATION
  | FAPI_MSG_UNKNOWN
  deriving (Show, Eq, Generic, NFDataX)

fapiMsgTypeId :: FapiMsgType -> BitVector 8
fapiMsgTypeId FAPI_PARAM_REQUEST    = 0x00
fapiMsgTypeId FAPI_PARAM_RESPONSE   = 0x01
fapiMsgTypeId FAPI_CONFIG_REQUEST   = 0x02
fapiMsgTypeId FAPI_CONFIG_RESPONSE  = 0x03
fapiMsgTypeId FAPI_START_REQUEST    = 0x04
fapiMsgTypeId FAPI_STOP_REQUEST     = 0x05
fapiMsgTypeId FAPI_STOP_INDICATION  = 0x06
fapiMsgTypeId FAPI_ERROR_INDICATION = 0x07
fapiMsgTypeId FAPI_MSG_UNKNOWN      = 0xFF

fapiMsgTypeFromId :: BitVector 8 -> FapiMsgType
fapiMsgTypeFromId 0x00 = FAPI_PARAM_REQUEST
fapiMsgTypeFromId 0x01 = FAPI_PARAM_RESPONSE
fapiMsgTypeFromId 0x02 = FAPI_CONFIG_REQUEST
fapiMsgTypeFromId 0x03 = FAPI_CONFIG_RESPONSE
fapiMsgTypeFromId 0x04 = FAPI_START_REQUEST
fapiMsgTypeFromId 0x05 = FAPI_STOP_REQUEST
fapiMsgTypeFromId 0x06 = FAPI_STOP_INDICATION
fapiMsgTypeFromId 0x07 = FAPI_ERROR_INDICATION
fapiMsgTypeFromId _    = FAPI_MSG_UNKNOWN

-- =============================================================================
-- PHY State Machine (SCF222 Section 3.2)
-- =============================================================================

data PhyState
  = PHY_IDLE
  | PHY_CONFIGURED
  | PHY_RUNNING
  deriving (Show, Eq, Generic, NFDataX)

phyStateToVal :: PhyState -> BitVector 8
phyStateToVal PHY_IDLE       = 0x00
phyStateToVal PHY_CONFIGURED = 0x01
phyStateToVal PHY_RUNNING    = 0x02

-- =============================================================================
-- FAPI Response
-- =============================================================================

data FapiResponse = FapiResponse
  { frValid    :: Bit
  , frMsgType  :: BitVector 8
  , frErrCode  :: BitVector 8
  , frPhyState :: BitVector 8
  } deriving (Show, Eq, Generic, NFDataX)

nullFapiResponse :: FapiResponse
nullFapiResponse = FapiResponse
  { frValid    = 0
  , frMsgType  = 0xFF
  , frErrCode  = 0
  , frPhyState = 0
  }

-- =============================================================================
-- Error Codes (SCF222 Table 3-6)
-- =============================================================================

data FapiErrorCode
  = FAPI_MSG_OK
  | FAPI_MSG_INVALID_STATE
  | FAPI_MSG_INVALID_CONFIG
  | FAPI_SFN_OUT_OF_SYNC
  | FAPI_MSG_SLOT_ERR
  | FAPI_MSG_BCH_MISSING
  | FAPI_MSG_INVALID_SFN
  deriving (Show, Eq, Generic, NFDataX)

fapiErrorCodeVal :: FapiErrorCode -> BitVector 8
fapiErrorCodeVal FAPI_MSG_OK             = 0x00
fapiErrorCodeVal FAPI_MSG_INVALID_STATE  = 0x01
fapiErrorCodeVal FAPI_MSG_INVALID_CONFIG = 0x02
fapiErrorCodeVal FAPI_SFN_OUT_OF_SYNC   = 0x03
fapiErrorCodeVal FAPI_MSG_SLOT_ERR      = 0x04
fapiErrorCodeVal FAPI_MSG_BCH_MISSING   = 0x05
fapiErrorCodeVal FAPI_MSG_INVALID_SFN   = 0x06

-- =============================================================================
-- Shared Packet Control Type (matches nuand bladeRF packet_control_t)
-- =============================================================================

data PacketControl = PacketControl
  { pkt_sop    :: Bit
  , pkt_eop    :: Bit
  , data_valid :: Bit
  , pktData    :: BitVector 32
  } deriving (Show, Eq, Generic, NFDataX)

nullPacketControl :: PacketControl
nullPacketControl = PacketControl
  { pkt_sop    = 0
  , pkt_eop    = 0
  , data_valid = 0
  , pktData    = 0
  }
