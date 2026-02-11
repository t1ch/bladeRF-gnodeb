{-# LANGUAGE RecordWildCards #-}

module GNodeBFAPI where

import Clash.Prelude
import GNodeBFAPITypes
import GNodeBTX
import GNodeBRX

-- =============================================================================
-- Top Entity: BladeRF xA9 FAPI P5 Processor
-- =============================================================================
--
-- Architecture (2 decoupled Mealy machines, no feedback loop):
--
--   Host (libbladeRF, BLADERF_FORMAT_PACKET_META)
--     │
--     ▼  tx_pkt_sop / tx_pkt_data / tx_pkt_eop
--   ┌────────────────────────────────────────────────────────────┐
--   │  TX Side  (GNodeBTX)                                      │
--   │  • Receives bladeRF packets from host                     │
--   │  • On SOP: processP5() → PHY state transition + response  │
--   │  • Response held with frValid=1 until next packet          │
--   │  • No handshake / no waiting for RX                       │
--   └──────────┬─────────────────────────────────────────────────┘
--              │ FapiResponse (frValid=1, msg_type, err_code, phy_state)
--              │  (unidirectional, no feedback)
--              ▼
--   ┌──────────────────────────────────────────────────────────┐
--   │  RX Side  (GNodeBRX)                                     │
--   │  • Waits for frValid=1, latches, sets sentFlag           │
--   │  • Serialises 2-word packet: SOP header + EOP sentinel   │
--   │  • Same FSM shape as original working PoC                │
--   │  • sentFlag prevents re-sending same response            │
--   └──────────┬───────────────────────────────────────────────┘
--              │ rx_pkt_sop / rx_pkt_data / rx_pkt_eop
--              ▼
--   Host (libbladeRF)
--

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
  -> Signal System Bit                     -- ^ rx_enable
  -> Signal System Bit                     -- ^ rx_packet_enable
  -> Signal System Bit                     -- ^ rx_packet_ready
  -> Clock System                          -- ^ tx_clock
  -> Reset System                          -- ^ tx_reset
  -> Signal System Bit                     -- ^ tx_enable
  -> Signal System Bit                     -- ^ tx_pkt_sop
  -> Signal System Bit                     -- ^ tx_pkt_eop
  -> Signal System (BitVector 32)          -- ^ tx_pkt_data
  -> Signal System Bit                     -- ^ tx_pkt_data_valid
  -> Signal System Bit                     -- ^ tx_packet_empty
  -> ( Signal System Bit                   -- ^ rx_pkt_sop
     , Signal System Bit                   -- ^ rx_pkt_eop
     , Signal System (BitVector 32)        -- ^ rx_pkt_data
     , Signal System Bit                   -- ^ rx_pkt_data_valid
     , Signal System Bit                   -- ^ tx_packet_ready
     , Signal System (BitVector 3)         -- ^ leds
     )
topEntity rxClk rxRst rxEnBit rxPktEn rxPktReady
          txClk txRst txEnBit
          txPktSop txPktEop txPktData txPktDataValid
          txPktEmpty =
  let
    -- =========================================================================
    -- TX Side: packet consumer + inline FAPI P5 processor
    -- =========================================================================

    txEn = toEnable (fmap bitToBool txEnBit)

    txPacketControl = PacketControl <$> txPktSop
                                    <*> txPktEop
                                    <*> txPktDataValid
                                    <*> txPktData

    txInputs = bundle (txPktEmpty, txPacketControl)

    txOutput = withClockResetEnable txClk txRst txEn $
                 mealy txMealy nullTxState txInputs

    txReady      = tx_packet_ready <$> txOutput
    txLeds       = leds            <$> txOutput
    fapiResponse = txResponse      <$> txOutput

    -- =========================================================================
    -- RX Side: FAPI response → bladeRF packet serialiser
    -- =========================================================================

    rxEn = toEnable (fmap bitToBool rxEnBit)

    rxInputs = bundle (rxEnBit, rxPktEn, rxPktReady, fapiResponse)

    -- RX mealy returns plain PacketControl (same as original working PoC)
    rxPacketCtrl = withClockResetEnable rxClk rxRst rxEn $
                     mealy (rxMealy defaultRxConfig) nullRxState rxInputs

    rxPktSop       = pkt_sop    <$> rxPacketCtrl
    rxPktEop       = pkt_eop    <$> rxPacketCtrl
    rxPktData      = pktData    <$> rxPacketCtrl
    rxPktDataValid = data_valid <$> rxPacketCtrl

  in
    (rxPktSop, rxPktEop, rxPktData, rxPktDataValid, txReady, txLeds)
