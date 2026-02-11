{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module GNodeBTX
  ( -- * Types
    FsmTx(..)
  , StateTx(..)
  , TxOutput(..)
    -- * State initialization
  , nullTxState
    -- * State machine logic
  , processP5
  , txStateComb
  , txMealy
  , calcLeds
  ) where

import Clash.Prelude
import GNodeBFAPITypes

-- =============================================================================
-- TX Types (Host → FPGA)
-- =============================================================================

data FsmTx
  = TX_IDLE           -- ^ Waiting for packet available
  | TX_WAIT_FOR_SOP   -- ^ Signalling ready, waiting for SOP
  | TX_DRAIN_PAYLOAD  -- ^ Consuming remaining payload words until EOP
  | TX_ERROR          -- ^ Error
  deriving (Show, Eq, Generic, NFDataX)

-- | TX state — carries PHY state and latest computed response.
-- No handshake with RX: the response is held until the next packet overwrites it.
data StateTx = StateTx
  { txFsm            :: FsmTx
  , txReadyForPacket :: Bit
  , txReadNextWord   :: Bit
  , txReadDwords     :: Unsigned 16
  , txPhyState       :: PhyState
  , txFapiResp       :: FapiResponse   -- ^ Held until next packet overwrites
  } deriving (Show, Eq, Generic, NFDataX)

data TxOutput = TxOutput
  { tx_packet_ready :: Bit
  , txResponse      :: FapiResponse
  , leds            :: BitVector 3
  } deriving (Show, Eq, Generic, NFDataX)

-- =============================================================================
-- State Initialization
-- =============================================================================

nullTxState :: StateTx
nullTxState = StateTx
  { txFsm            = TX_IDLE
  , txReadyForPacket = 0
  , txReadNextWord   = 0
  , txReadDwords     = 0
  , txPhyState       = PHY_IDLE
  , txFapiResp       = nullFapiResponse
  }

-- =============================================================================
-- P5 Protocol Logic — pure function
-- =============================================================================

processP5
  :: PhyState
  -> BitVector 8
  -> (PhyState, FapiResponse)
processP5 curPhy msgId =
  let
    msgType   = fapiMsgTypeFromId msgId
    curPhyVal = phyStateToVal curPhy

    errResp = FapiResponse
      { frValid    = 1
      , frMsgType  = fapiMsgTypeId FAPI_ERROR_INDICATION
      , frErrCode  = fapiErrorCodeVal FAPI_MSG_INVALID_STATE
      , frPhyState = curPhyVal
      }

    okResp rspType newPhy = FapiResponse
      { frValid    = 1
      , frMsgType  = fapiMsgTypeId rspType
      , frErrCode  = fapiErrorCodeVal FAPI_MSG_OK
      , frPhyState = phyStateToVal newPhy
      }
  in
    case msgType of
      FAPI_PARAM_REQUEST ->
        case curPhy of
          PHY_IDLE       -> (PHY_IDLE,       okResp FAPI_PARAM_RESPONSE PHY_IDLE)
          PHY_CONFIGURED -> (PHY_CONFIGURED, okResp FAPI_PARAM_RESPONSE PHY_CONFIGURED)
          PHY_RUNNING    -> (PHY_RUNNING,    errResp)

      FAPI_CONFIG_REQUEST ->
        case curPhy of
          PHY_IDLE       -> (PHY_CONFIGURED, okResp FAPI_CONFIG_RESPONSE PHY_CONFIGURED)
          PHY_CONFIGURED -> (PHY_CONFIGURED, okResp FAPI_CONFIG_RESPONSE PHY_CONFIGURED)
          PHY_RUNNING    -> (PHY_RUNNING,    errResp)

      FAPI_START_REQUEST ->
        case curPhy of
          PHY_CONFIGURED -> (PHY_RUNNING,
                             (okResp FAPI_START_REQUEST PHY_RUNNING)
                               { frMsgType = 0x04 })
          _              -> (curPhy, errResp)

      FAPI_STOP_REQUEST ->
        case curPhy of
          PHY_RUNNING -> (PHY_IDLE, okResp FAPI_STOP_INDICATION PHY_IDLE)
          _           -> (curPhy,   errResp)

      _ -> (curPhy, errResp)

-- =============================================================================
-- TX Combinational Logic
-- =============================================================================

-- | TX FSM — no handshake with RX.
-- On SOP: process FAPI, latch response with frValid=1, go back to IDLE.
-- Response stays valid until overwritten by next packet.
txStateComb
  :: StateTx
  -> Bit              -- ^ tx_packet_empty
  -> PacketControl    -- ^ tx_packet_control
  -> StateTx
txStateComb current@StateTx{..} tx_packet_empty tx_pkt_ctrl =
  let
    future = current { txReadNextWord = 0 }
  in
    case txFsm of
      TX_IDLE ->
        if tx_packet_empty == 0
          then future { txFsm = TX_WAIT_FOR_SOP }
          else future

      TX_WAIT_FOR_SOP ->
        let withReady = future { txReadyForPacket = 1 }
        in
          if pkt_sop tx_pkt_ctrl == 1 && data_valid tx_pkt_ctrl == 1
            then
              let
                headerWord = pktData tx_pkt_ctrl
                msgTypeId  = slice d31 d24 headerWord
                (newPhy, resp) = processP5 txPhyState msgTypeId
              in
                if pkt_eop tx_pkt_ctrl == 1
                  then
                    -- Single-word packet: process and immediately back to IDLE
                    withReady
                      { txReadyForPacket = 0
                      , txFsm            = TX_IDLE
                      , txReadDwords     = 1
                      , txPhyState       = newPhy
                      , txFapiResp       = resp
                      }
                  else
                    -- Multi-word: drain remaining payload
                    withReady
                      { txReadyForPacket = 0
                      , txFsm            = TX_DRAIN_PAYLOAD
                      , txReadDwords     = 1
                      , txPhyState       = newPhy
                      , txFapiResp       = resp
                      }
            else withReady

      TX_DRAIN_PAYLOAD ->
        let withRead = future { txReadNextWord = 1 }
        in
          if data_valid tx_pkt_ctrl == 1
            then
              let updated = withRead { txReadDwords = txReadDwords + 1 }
              in
                if pkt_eop tx_pkt_ctrl == 1
                  then updated
                    { txFsm          = TX_IDLE
                    , txReadNextWord = 0
                    }
                  else updated
            else withRead

      TX_ERROR ->
        future { txFsm = TX_IDLE }

-- =============================================================================
-- LED output
-- =============================================================================

calcLeds :: FsmTx -> BitVector 3
calcLeds fsm = complement $ case fsm of
  TX_IDLE          -> 0b111
  TX_WAIT_FOR_SOP  -> 0b001
  TX_DRAIN_PAYLOAD -> 0b010
  TX_ERROR         -> 0b100

-- =============================================================================
-- Mealy Machine — no respConsumed input
-- =============================================================================

txMealy
  :: StateTx
  -> (Bit, PacketControl)         -- ^ (tx_packet_empty, pkt_ctrl)
  -> (StateTx, TxOutput)
txMealy current (tx_packet_empty, tx_packet_control) =
  let
    future = txStateComb current tx_packet_empty tx_packet_control

    tx_ready = if txReadyForPacket current == 1 || txReadNextWord current == 1
                 then 1 else 0

    output = TxOutput
      { tx_packet_ready = tx_ready
      , txResponse      = txFapiResp current   -- registered
      , leds            = calcLeds (txFsm current)
      }
  in
    (future, output)
