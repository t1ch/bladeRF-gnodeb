{-# LANGUAGE RecordWildCards #-}

module GNodeBTX
  ( -- * Types
    FsmTx(..)
  , PacketControl(..)
  , StateTx(..)
  , TxOutput(..)
    -- * State initialization
  , nullTxState
    -- * State machine logic
  , txStateComb
  , txMealy
    -- * Output calculation
  , calcLeds
  ) where

import Clash.Prelude

-- =============================================================================
-- TX Types
-- =============================================================================

-- | TX FSM states
data FsmTx
  = IDLE
  | WAIT_FOR_SOP
  | READ_PACKET
  | DONE
  | ERR
  | TEST
  deriving (Show, Eq, Generic, NFDataX)

-- | Packet control type (corresponds to packet_control_t from nuand library)
data PacketControl = PacketControl
  { pkt_sop    :: Bit
  , pkt_eop    :: Bit
  , data_valid :: Bit
  , pktData    :: BitVector 32
  } deriving (Show, Eq, Generic, NFDataX)

-- | TX State record
data StateTx = StateTx
  { fsm              :: FsmTx
  , ready_for_packet :: Bit
  , read_next_word   :: Bit
  , stateData        :: BitVector 32
  , read_dwords      :: Unsigned 32
  } deriving (Show, Eq, Generic, NFDataX)

-- | TX output record
data TxOutput = TxOutput
  { tx_packet_ready :: Bit
  , leds            :: BitVector 3
  } deriving (Show, Eq, Generic, NFDataX)

-- =============================================================================
-- State Initialization
-- =============================================================================

-- | Initial/NULL state for TX state machine
nullTxState :: StateTx
nullTxState = StateTx
  { fsm              = IDLE
  , ready_for_packet = 0
  , read_next_word   = 0
  , stateData        = 0
  , read_dwords      = 0
  }

-- =============================================================================
-- Combinational Logic
-- =============================================================================

-- | Combinational logic for TX state machine
-- Implements the state transitions based on current state and inputs
txStateComb
  :: StateTx              -- ^ Current state
  -> Bit                  -- ^ tx_packet_empty
  -> PacketControl        -- ^ tx_packet_control
  -> StateTx              -- ^ Future state
txStateComb current@StateTx{..} tx_packet_empty tx_packet_control =
  let
    -- Default: keep current state but clear read_next_word
    future = current { read_next_word = 0 }
  in
    case fsm of
      IDLE ->
        if tx_packet_empty == 0
          then future { fsm = WAIT_FOR_SOP }
          else future

      WAIT_FOR_SOP ->
        let withReady = future { ready_for_packet = 1 }
        in
          if pkt_sop tx_packet_control == 1 && data_valid tx_packet_control == 1
            then withReady
              { ready_for_packet = 0
              , fsm              = READ_PACKET
              , read_dwords      = 1
              , stateData        = pktData tx_packet_control
              }
            else withReady

      READ_PACKET ->
        let withReadNext = future { read_next_word = 1 }
        in
          if data_valid tx_packet_control == 1
            then
              let updated = withReadNext
                    { read_dwords = read_dwords + 1
                    , stateData   = pktData tx_packet_control
                    }
              in
                if pkt_eop tx_packet_control == 1
                  then updated { fsm = TEST, read_next_word = 0 }
                  else updated
            else withReadNext

      TEST ->
        if stateData == 0x0000000D
          then future { fsm = TEST }
          else future { fsm = ERR }

      DONE ->
        future { fsm = IDLE }

      ERR ->
        future { fsm = IDLE }

-- =============================================================================
-- Output Calculation
-- =============================================================================

-- | Calculate LEDs output based on FSM state
-- Each state has a unique LED pattern (active low)
calcLeds :: FsmTx -> BitVector 3
calcLeds fsm = complement $ case fsm of
  IDLE         -> 0b111
  WAIT_FOR_SOP -> 0b001
  READ_PACKET  -> 0b010
  DONE         -> 0b011
  ERR          -> 0b100
  TEST         -> 0b000

-- =============================================================================
-- Mealy Machine
-- =============================================================================

-- | TX Mealy machine
-- Combines state transitions and output generation
txMealy
  :: StateTx                      -- ^ Current state
  -> (Bit, PacketControl)         -- ^ Inputs: (tx_packet_empty, tx_packet_control)
  -> (StateTx, TxOutput)          -- ^ (New state, Outputs)
txMealy current (tx_packet_empty, tx_packet_control) =
  let
    -- Calculate next state
    future = txStateComb current tx_packet_empty tx_packet_control

    -- tx_packet_ready is '1' when ready_for_packet or read_next_word is '1'
    tx_ready = if ready_for_packet current == 1 || read_next_word current == 1
                 then 1
                 else 0

    -- Generate outputs based on current state
    output = TxOutput
      { tx_packet_ready = tx_ready
      , leds            = calcLeds (fsm current)
      }
  in
    (future, output)
