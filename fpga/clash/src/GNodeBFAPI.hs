{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module GNodeBFAPI where

import Clash.Explicit.Prelude
import GNodeBFAPITypes
import GNodeBTX
import GNodeBRX

-- =============================================================================
-- Clock Domains
-- =============================================================================

createDomain vSystem{vName="DomTx", vPeriod=hzToPeriod 80e6}
createDomain vSystem{vName="DomRx", vPeriod=hzToPeriod 80e6}

-- =============================================================================
-- Top Entity
-- =============================================================================

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
            , PortName "tb_ready"
            , PortName "tb_len_dwords"
            , PortName "tb_pdu_index"
            ]
      }
  ) #-}
topEntity
  :: Clock DomRx
  -> Reset DomRx
  -> Signal DomRx Bit
  -> Signal DomRx Bit
  -> Signal DomRx Bit
  -> Clock DomTx
  -> Reset DomTx
  -> Signal DomTx Bit
  -> Signal DomTx Bit
  -> Signal DomTx Bit
  -> Signal DomTx (BitVector 32)
  -> Signal DomTx Bit
  -> Signal DomTx Bit
  -> ( Signal DomRx Bit
     , Signal DomRx Bit
     , Signal DomRx (BitVector 32)
     , Signal DomRx Bit
     , Signal DomTx Bit
     , Signal DomTx (BitVector 3)
     , Signal DomTx Bit
     , Signal DomTx (BitVector 16)
     , Signal DomTx (BitVector 16)
     )
topEntity rxClk rxRst rxEnBit rxPktEn rxPktReady
          txClk txRst txEnBit
          txPktSop txPktEop txPktData txPktDataValid
          txPktEmpty =
  let
    txEn = toEnable (fmap bitToBool txEnBit)

    txPacketControl = PacketControl <$> txPktSop
                                    <*> txPktEop
                                    <*> txPktDataValid
                                    <*> txPktData

    fifoFullBit = boolToBit <$> fifoFull
    txInputs = bundle (txPktEmpty, txPacketControl, fifoFullBit)
    txOutput = mealy txClk txRst txEn txMealy nullTxState txInputs

    txReady       = tx_packet_ready <$> txOutput
    txLeds        = leds            <$> txOutput
    txFifoWr      = txFifoWrite     <$> txOutput
    tbReadyOut    = txoTbReady      <$> txOutput
    tbLenDwOut    = pack . txoTbLenDwords <$> txOutput
    tbPduIndexOut = txoTbPduIndex   <$> txOutput

    rxEn = toEnable (fmap bitToBool rxEnBit)

    (fifoData, fifoEmpty, fifoFull) =
      asyncFIFOSynchronizer
        d4 txClk rxClk txRst rxRst txEn rxEn fifoReadReq txFifoWr

    rxInputs = bundle (rxEnBit, rxPktEn, rxPktReady, fifoData, fifoEmpty)
    rxRawOutput = mealy rxClk rxRst rxEn rxMealy nullRxState rxInputs
    rxPacketCtrl = fst <$> rxRawOutput
    fifoReadReq  = snd <$> rxRawOutput

    rxPktSopOut  = pkt_sop    <$> rxPacketCtrl
    rxPktEopOut  = pkt_eop    <$> rxPacketCtrl
    rxPktDataOut = pktData    <$> rxPacketCtrl
    rxPktDvOut   = data_valid <$> rxPacketCtrl

  in
    ( rxPktSopOut, rxPktEopOut, rxPktDataOut, rxPktDvOut
    , txReady, txLeds
    , tbReadyOut, tbLenDwOut, tbPduIndexOut
    )
