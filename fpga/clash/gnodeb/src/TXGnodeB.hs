{-# LANGUAGE BlockArguments #-}
module TXGnodeB (tx_gnodeb) where
import Clash.Prelude

data TXState = TXReset | TXIdle | TXReceivingStart | TXReceiving | TXReceivingDone
  deriving (Show, Generic, NFDataX, Eq)
data CRCState = CRCStarting | CRCCalculating | CRCDone
  deriving (Show, Generic, NFDataX, Eq)


data Packet_control_t = Packet_control_t
  { pkt_core_id :: BitVector 8
    ,pkt_flags  :: BitVector 8
    ,pkt_sop    :: BitVector 1
    ,pkt_eop    :: BitVector 1
    ,dataIn     :: BitVector 32
    ,data_valid :: BitVector 1
  }



txNextState (txPacketReady,txLeds,txState,txCRC,txTransportBlock) (txPacketControl,txReset,txPacketEmpty) 
  |bitToBool txReset = (txPacketReady,txLeds,TXIdle,txCRC,txTransportBlock)
  |otherwise =
    case txState of
      TXReset -> (txPacketReady,txLeds,TXIdle,txCRC,txTransportBlock)
      TXIdle  -> TXReceivingStart
      TXReceiving -> TXReceivingDone
      TXReceivingDone -> TXIdle




--gnb_top_int :: Clock System -> Reset System -> Signal System Packet_control_t -> (Signal System Bit, Signal System (BitVector 3))
--gnb_top_int tx_clock tx_reset tx_packet_control = [...]
--{-# NOINLINE gnb_top_int #-}

-- {-# ANN tx_gnodeb
--   (Synthesize
--     { t_name     = "tx_gnodeb"
--     , t_inputs   = [ PortName "tx_clock", PortName "tx_reset"
--                    , PortName "tx_packet_control", PortName "tx_packet_empty" ]
--     , t_output   = PortProduct ""
--          [ PortName "tx_packet_ready"
--          , PortName "tx_leds"
--          ]
--     })#-}


--gnb_top :: Clock System -> Reset System -> Signal System Packet_control_t ->
--           Signal System Bit -> (Signal System Bit, Signal System (BitVector 3))

--gnb_top txClock txReset txPacketControl txPacketEmpty  = (txPacketReady,txLeds)
--where Clash.Explicit.Moore.moore txClock txReset txPacketEmpty txNextState (0b1,0b111) (txPacketControl,TXReset)
tx_gnodeb = 1
