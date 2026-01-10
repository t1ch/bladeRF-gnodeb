{-# LANGUAGE RecordWildCards #-}

module GNodeBFAPI where

import Clash.Prelude
import GNodeBTX
import GNodeBRX

-- =============================================================================
-- Top Entity
-- =============================================================================

-- | Top entity with explicit port names matching the synthesize annotation
{-# ANN topEntity
  ( Synthesize
      { t_name = "gnodeb_fapi_top"
      , t_inputs =
          [ PortName "rx_clock"
          , PortName "rx_reset"
          , PortName "rx_enable"
          , PortName "rx_packet_enable"
          , PortName "rx_packet_ready"
          , PortName "tx_clock"
          , PortName "tx_reset"
          , PortName "tx_enable"
          , PortName "tx_pkt_sop"
          , PortName "tx_pkt_eop"
          , PortName "tx_pkt_data"
          , PortName "tx_pkt_data_valid"
          , PortName "tx_packet_empty"
          ]
      , t_output =
          PortProduct ""
            [ PortName "rx_pkt_sop"
            , PortName "rx_pkt_eop"
            , PortName "rx_pkt_data"
            , PortName "rx_pkt_data_valid"
            , PortName "tx_packet_ready"
            , PortName "leds"
            ]
      }
  ) #-}
topEntity
  :: Clock System
  -> Reset System
  -> Signal System Bit          -- ^ rx_enable
  -> Signal System Bit          -- ^ rx_packet_enable
  -> Signal System Bit          -- ^ rx_packet_ready
  -> Clock System               -- ^ tx_clock
  -> Reset System               -- ^ tx_reset
  -> Signal System Bit          -- ^ tx_enable
  -> Signal System Bit          -- ^ tx_pkt_sop
  -> Signal System Bit          -- ^ tx_pkt_eop
  -> Signal System (BitVector 32)  -- ^ tx_pkt_data
  -> Signal System Bit          -- ^ tx_pkt_data_valid
  -> Signal System Bit          -- ^ tx_packet_empty
  -> ( Signal System Bit        -- ^ rx_pkt_sop
     , Signal System Bit        -- ^ rx_pkt_eop
     , Signal System (BitVector 32)  -- ^ rx_pkt_data
     , Signal System Bit        -- ^ rx_pkt_data_valid
     , Signal System Bit        -- ^ tx_packet_ready
     , Signal System (BitVector 3)   -- ^ leds
     )
topEntity rxClk rxRst rxEnBit rxPktEn rxPktReady
          txClk txRst txEnBit
          txPktSop txPktEop txPktData txPktDataValid
          txPktEmpty =
  let
    -- =========================================================================
    -- RX Side (Packet Generator)
    -- =========================================================================

    -- Convert Bit signal to Enable for RX
    rxEn = toEnable (fmap bitToBool rxEnBit)

    -- Combine RX inputs for the Mealy machine
    rxInputs = bundle (rxEnBit, rxPktEn, rxPktReady)

    -- Instantiate the RX Mealy machine with explicit clock/reset/enable
    rxPacketCtrl = withClockResetEnable rxClk rxRst rxEn $
                     mealy (rxMealy defaultRxConfig) nullRxState rxInputs

    -- Extract RX outputs from packet control
    rxPktSop       = pkt_sop <$> rxPacketCtrl
    rxPktEop       = pkt_eop <$> rxPacketCtrl
    rxPktData      = pktData <$> rxPacketCtrl
    rxPktDataValid = data_valid <$> rxPacketCtrl

    -- =========================================================================
    -- TX Side (Packet Consumer)
    -- =========================================================================

    -- Convert Bit signal to Enable for TX
    txEn = toEnable (fmap bitToBool txEnBit)

    -- Construct packet control signal from individual inputs
    txPacketControl = PacketControl <$> txPktSop
                                    <*> txPktEop
                                    <*> txPktDataValid
                                    <*> txPktData

    -- Combine inputs for the TX Mealy machine
    txInputs = bundle (txPktEmpty, txPacketControl)

    -- Instantiate the TX Mealy machine with explicit clock/reset/enable
    txOutput = withClockResetEnable txClk txRst txEn $
                 mealy txMealy nullTxState txInputs

    -- Extract TX outputs
    txReady = tx_packet_ready <$> txOutput
    txLeds  = leds <$> txOutput
  in
    (rxPktSop, rxPktEop, rxPktData, rxPktDataValid, txReady, txLeds)
