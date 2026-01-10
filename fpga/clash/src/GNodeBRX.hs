{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module GNodeBRX
  ( -- * Types
    FsmRx(..)
  , StateRx(..)
  , RxConfig(..)
    -- * Default configuration
  , defaultRxConfig
    -- * State initialization
  , nullRxState
    -- * State machine logic
  , rxStateComb
  , rxMealy
  ) where

import Clash.Prelude
import GNodeBTX (PacketControl(..))

-- =============================================================================
-- RX Types
-- =============================================================================

-- | RX FSM states for packet generation
data FsmRx
  = RX_IDLE
  | RX_HOLDOFF
  | RX_WAITED
  | RX_WRITE
  deriving (Show, Eq, Generic, NFDataX)

-- | RX configuration parameters (generics in VHDL)
data RxConfig = RxConfig
  { packetLen :: Int  -- ^ Length of packet to generate
  , maxLen    :: Int  -- ^ Maximum packet length
  , incr      :: Int  -- ^ Increment value
  , gap       :: Int  -- ^ Gap between packets
  } deriving (Show, Eq, Generic, NFDataX)

-- | Default RX configuration matching VHDL generics
defaultRxConfig :: RxConfig
defaultRxConfig = RxConfig
  { packetLen = 500
  , maxLen    = 1000
  , incr      = 50
  , gap       = 10
  }

-- | RX State record
data StateRx = StateRx
  { state       :: FsmRx
  , hold_count  :: Signed 32      -- ^ Hold counter for gap timing
  , write_count :: Signed 32      -- ^ Write counter for packet length
  , pkt_id      :: Unsigned 16    -- ^ Packet ID counter
  , pkt         :: PacketControl  -- ^ Current packet control output
  } deriving (Show, Eq, Generic, NFDataX)

-- =============================================================================
-- State Initialization
-- =============================================================================

-- | Default packet control value
packetControlDefault :: PacketControl
packetControlDefault = PacketControl
  { pkt_sop    = 0
  , pkt_eop    = 0
  , data_valid = 0
  , pktData    = 0
  }

-- | Initial/NULL state for RX state machine
nullRxState :: StateRx
nullRxState = StateRx
  { state       = RX_IDLE
  , hold_count  = 0
  , write_count = 0
  , pkt_id      = 0
  , pkt         = packetControlDefault
  }

-- =============================================================================
-- Combinational Logic
-- =============================================================================

-- | Combinational logic for RX packet generator state machine
-- Generates packets with configurable length and timing
rxStateComb
  :: RxConfig         -- ^ Configuration parameters
  -> StateRx          -- ^ Current state
  -> Bit              -- ^ rx_enable
  -> Bit              -- ^ rx_packet_enable
  -> Bit              -- ^ rx_packet_ready
  -> StateRx          -- ^ Future state
rxStateComb config current rx_enable rx_packet_enable rx_packet_ready =
  let
    -- Default: keep current state but reset packet control
    future = current { pkt = packetControlDefault }

    -- Extract config parameters
    pktLen = fromIntegral (packetLen config)
    gapVal = fromIntegral (gap config)
  in
    case state current of
      RX_IDLE ->
        let idle_future = future
              { hold_count  = 0
              , write_count = 0
              }
        in
          if rx_enable == 1 && rx_packet_enable == 1
            then idle_future { state = RX_HOLDOFF }
            else idle_future

      RX_HOLDOFF ->
        let holdoff_future = future
              { write_count = pktLen
              , hold_count  = hold_count current + 1
              }
        in
          if hold_count current == gapVal
            then holdoff_future { state = RX_WAITED }
            else holdoff_future

      RX_WAITED ->
        if rx_packet_ready == 1
          then future { state = RX_WRITE }
          else future

      RX_WRITE ->
        let
          current_write = write_count current
          current_pkt_id = pkt_id current

          -- Build packet data: [pkt_id (16 bits) | write_count (16 bits)]
          pkt_id_vec = pack current_pkt_id
          write_count_vec = pack (resize (bitCoerce current_write :: Unsigned 32) :: Unsigned 16)
          data_value = pkt_id_vec ++# write_count_vec

          write_future = future
            { pkt = (pkt future)
                { data_valid = 1
                , pktData    = data_value
                }
            , write_count = current_write - 1
            }
        in
          if current_write == pktLen
            then -- Start of packet
              write_future
                { pkt = (pkt write_future) { pkt_sop = 1 }
                }
          else if current_write == 1
            then -- End of packet
              let
                new_pkt_id = if current_pkt_id > 65000
                              then 0
                              else current_pkt_id + 1
              in
                write_future
                  { pkt = (pkt write_future) { pkt_eop = 1 }
                  , state = RX_IDLE
                  , pkt_id = new_pkt_id
                  }
          else -- Middle of packet
            write_future

-- =============================================================================
-- Mealy Machine
-- =============================================================================

-- | RX Mealy machine for packet generation
-- Takes configuration, current state, and inputs, produces next state and output
rxMealy
  :: RxConfig                          -- ^ Configuration parameters
  -> StateRx                           -- ^ Current state
  -> (Bit, Bit, Bit)                   -- ^ Inputs: (rx_enable, rx_packet_enable, rx_packet_ready)
  -> (StateRx, PacketControl)          -- ^ (New state, Output packet control)
rxMealy config current (rx_enable, rx_packet_enable, rx_packet_ready) =
  let
    -- Calculate next state
    future = rxStateComb config current rx_enable rx_packet_enable rx_packet_ready

    -- Output is the packet control from current state (registered output)
    output = pkt current
  in
    (future, output)
