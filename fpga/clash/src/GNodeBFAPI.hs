{-# language FlexibleContexts #-}
{-# language MultiParamTypeClasses #-}
{-# language NumericUnderscores #-}

module GNodeBFAPI where

import Clash.Prelude
import GHC.Generics (Generic)

type TxDom = System
type RxDom = System

-- Simple two-state machine for payload-only reception
data RxState =
    Idle
  | ReceivingPayload
  deriving (Generic, NFDataX, Eq, Show, Enum)

{-# ANN topEntity
  ( Synthesize
      { t_name = "gnodeb_fapi_top"
      , t_inputs =
          [PortName  "rx_clock"
          , PortName "rx_reset"
          , PortName "rx_enable"

          , PortName "rx_packet_enable"
          , PortName "rx_packet_ready"

          , PortName "tx_clock"
          , PortName "tx_reset"
          , PortName "tx_enable"

          , PortName "tx_pkt_sop"         -- Start of packet
          , PortName "tx_pkt_eop"         -- End of packet
          , PortName "tx_pkt_data"            -- 32-bit data word
          , PortName "tx_pkt_data_valid"      -- Data valid signal
          , PortName "tx_packet_empty" -- Packet queue empty (0 = has data)
          ]
      , t_output =
          PortProduct ""
            [PortName "rx_pkt_sop"         -- Start of packet
            , PortName "rx_pkt_eop"         -- End of packet
            , PortName "rx_pkt_data"            -- 32-bit data word
            , PortName "rx_pkt_data_valid"      -- Data valid signal

            , PortName "tx_packet_ready"    -- Ready to receive
            , PortName "leds"            -- Status LEDs (3 bits, active low)
            ]
      }
  ) #-}

topEntity ::
  -- RX domain inputs
  Clock RxDom ->                  -- rx_clock
  Reset RxDom ->                  -- rx_reset
  Signal RxDom Bit ->             -- rx_enable

  Signal RxDom Bit ->             -- rx_packet_enable
  Signal RxDom Bit ->             -- rx_packet_ready

  -- TX domain inputs
  Clock TxDom ->                  -- tx_clock
  Reset TxDom ->                  -- tx_reset
  Signal TxDom Bit ->             -- tx_enable

  -- TX packet inputs
  Signal TxDom Bit ->             -- tx_pkt_sop
  Signal TxDom Bit ->             -- tx_pkt_eop
  Signal TxDom (BitVector 32) ->  -- tx_pkt_data
  Signal TxDom Bit ->             -- tx_pkt_data_valid
  Signal TxDom Bit ->             -- tx_packet_empty

  -- Outputs
  (Signal RxDom Bit               -- rx_pkt_sop
  , Signal RxDom Bit              -- rx_pkt_eop
  , Signal RxDom (BitVector 32)   -- rx_pkt_data
  , Signal RxDom Bit              -- rx_data_valid
  , Signal TxDom Bit              -- tx_packet_ready
  , Signal TxDom (BitVector 3)    -- tx_leds (active low)
  )
topEntity rxClk rxRst rxEna rx_pkt_ena rx_pkt_ready txClk txRst txEna sop eop dat valid empty =
  (rx_sop, rx_eop, rx_dat, rx_valid, ready, leds)
 where
  -- Use TX enable signal to create Enable
  ena = toEnable (bitToBool <$> txEna)

  -- Simple activity counter for visual feedback
  counter = withClockResetEnable txClk txRst ena $
    register (0 :: Unsigned 24) (counter + 1)

  -- Bundle inputs for state machine
  inputs = bundle (sop, eop, valid)

  -- State machine - payload only, no header processing
  (state, wordCount) = unbundle $
    withClockResetEnable txClk txRst ena $
      moore stateMachine outputFunction (Idle, 0 :: Unsigned 16) inputs

  -- State transition function
  stateMachine ::
    (RxState, Unsigned 16) ->     -- Current state and word count
    (Bit, Bit, Bit) ->             -- SOP, EOP, Valid inputs
    (RxState, Unsigned 16)         -- Next state and word count

  stateMachine (Idle, _) (sopBit, eopBit, validBit) =
    -- Start receiving when SOP arrives with valid data
    if sopBit == 1 && validBit == 1 then
      if eopBit == 1 then
        -- Single-word packet, stay in Idle but count it
        (Idle, 1)
      else
        -- Multi-word packet, start receiving
        (ReceivingPayload, 1)
    else
      -- Wait for start of packet
      (Idle, 0)

  stateMachine (ReceivingPayload, count) (sopBit, eopBit, validBit) =
    if validBit == 1 then
      if eopBit == 1 then
        -- End of packet, return to Idle
        (Idle, 0)
      else
        -- Continue receiving, increment count
        (ReceivingPayload, count + 1)
    else
      -- Wait for valid data
      (ReceivingPayload, count)

  -- Output function (just passes state through)
  outputFunction :: (RxState, Unsigned 16) -> (RxState, Unsigned 16)
  outputFunction = id

  -- Pass through the TX inputs as RX outputs (loopback for testing)
  -- In a real implementation, these would come from actual RX logic
  rx_sop = sop
  rx_eop = eop
  rx_dat = dat
  rx_valid = valid

  -- Ready signal: always ready to receive in this simple implementation
  -- Set to 0 only during actual data reception to provide backpressure if needed
  ready = mux (valid .==. pure 1 .&&. state .==. pure ReceivingPayload)
              (pure 0)  -- Not ready during active reception
              (pure 1)  -- Ready otherwise

  -- LED status (active low: 0 = ON, 1 = OFF)
  -- Convert Vec 3 Bit to BitVector 3
  leds = pack <$> ledVec
    where
      ledVec = mkLeds <$> state <*> counter <*> wordCount <*> valid

      mkLeds :: RxState -> Unsigned 24 -> Unsigned 16 -> Bit -> Vec 3 Bit
      mkLeds currentState cnt words validBit =
        let heartbeat = if testBit cnt 23 then 0 else 1   -- ~6Hz @ 50MHz
            activity = if testBit cnt 19 then 0 else 1     -- ~95Hz @ 50MHz
            wordParity = if testBit words 0 then 0 else 1  -- Toggle per word
        in case currentState of
             Idle ->
               -- LED0: Heartbeat (system alive)
               -- LED1: OFF
               -- LED2: OFF
               heartbeat :> 1 :> 1 :> Nil

             ReceivingPayload ->
               -- LED0: ON solid (receiving)
               -- LED1: Activity indicator (fast blink)
               -- LED2: Word counter parity
               0 :> activity :> wordParity :> Nil
