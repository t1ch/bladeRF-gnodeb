{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module GNodeBRX
  ( FsmRx(..), StateRx(..)
  , nullRxState
  , rxMealy
  ) where

import Clash.Prelude
import GNodeBFAPITypes

-- =============================================================================
-- RX Types
-- =============================================================================

data FsmRx
  = RX_IDLE
  | RX_SETTLE_DW1_A
  | RX_SETTLE_DW1_B
  | RX_READ_HDR_DW1
  | RX_SETTLE_BODY_A
  | RX_SETTLE_BODY_B
  | RX_READ_BODY
  | RX_WAIT_READY
  | RX_WRITE
  deriving (Show, Eq, Generic, NFDataX)

data StateRx = StateRx
  { rxFsmState   :: FsmRx
  , rxWriteCount :: Signed 32
  , rxPktId      :: Unsigned 16
  , rxPkt        :: PacketControl
  , rxHdrDw0     :: BitVector 32
  , rxHdrDw1     :: BitVector 32
  , rxBodyBuf    :: Vec 14 (BitVector 32)
  , rxBodyTotal  :: Unsigned 16
  , rxBodyRead   :: Unsigned 16
  , rxFifoRead   :: Bool
  } deriving (Show, Eq, Generic, NFDataX)

maxRxBodyDwords :: Unsigned 16
maxRxBodyDwords = 14

nullRxState :: StateRx
nullRxState = StateRx
  { rxFsmState   = RX_IDLE
  , rxWriteCount = 0
  , rxPktId      = 0
  , rxPkt        = nullPacketControl
  , rxHdrDw0     = 0
  , rxHdrDw1     = 0
  , rxBodyBuf    = repeat 0
  , rxBodyTotal  = 0
  , rxBodyRead   = 0
  , rxFifoRead   = False
  }

-- =============================================================================
-- Combinational Logic
-- =============================================================================

rxStateComb
  :: StateRx
  -> Bit -> Bit -> Bit
  -> BitVector 32
  -> Bool
  -> StateRx
rxStateComb current rx_enable rx_packet_enable rx_packet_ready
            fifoData fifoEmpty =
  let
    quiet = current { rxPkt = nullPacketControl, rxFifoRead = False }
  in
    case rxFsmState current of

      RX_IDLE ->
        if rx_enable == 1 && rx_packet_enable == 1 && not fifoEmpty
          then quiet
            { rxFsmState  = RX_SETTLE_DW1_A
            , rxHdrDw0    = fifoData
            , rxFifoRead  = True
            , rxBodyRead  = 0
            , rxBodyBuf   = repeat 0
            }
          else quiet

      RX_SETTLE_DW1_A ->
        quiet { rxFsmState = RX_SETTLE_DW1_B }

      RX_SETTLE_DW1_B ->
        quiet { rxFsmState = RX_READ_HDR_DW1 }

      RX_READ_HDR_DW1 ->
        if not fifoEmpty
          then
            let hdw1 = fifoData
                (_, msgLen) = parseHeaderDw1 hdw1
                nBody = bodyLenToDwords msgLen
                nBodyClamped = if nBody > maxRxBodyDwords
                                 then maxRxBodyDwords else nBody
            in if nBodyClamped == 0
                 then quiet
                   { rxFsmState  = RX_WAIT_READY
                   , rxHdrDw1    = hdw1
                   , rxBodyTotal = 0
                   , rxFifoRead  = True
                   , rxWriteCount = 4
                   }
                 else quiet
                   { rxFsmState  = RX_SETTLE_BODY_A
                   , rxHdrDw1    = hdw1
                   , rxBodyTotal = nBodyClamped
                   , rxFifoRead  = True
                   }
          else quiet

      RX_SETTLE_BODY_A ->
        quiet { rxFsmState = RX_SETTLE_BODY_B }

      RX_SETTLE_BODY_B ->
        quiet { rxFsmState = RX_READ_BODY }

      RX_READ_BODY ->
        if not fifoEmpty
          then
            let idx  = rxBodyRead current
                word = fifoData
                newBuf  = replace idx word (rxBodyBuf current)
                newRead = idx + 1
                allDone = newRead >= rxBodyTotal current
                nBody   = rxBodyTotal current
                wireDwords = fromIntegral (2 + nBody + 2) :: Signed 32
            in if allDone
                 then quiet
                   { rxFsmState  = RX_WAIT_READY
                   , rxBodyBuf   = newBuf
                   , rxBodyRead  = newRead
                   , rxFifoRead  = True
                   , rxWriteCount = wireDwords
                   }
                 else quiet
                   { rxFsmState  = RX_SETTLE_BODY_A
                   , rxBodyBuf   = newBuf
                   , rxBodyRead  = newRead
                   , rxFifoRead  = True
                   }
          else quiet

      RX_WAIT_READY ->
        if rx_packet_ready == 1
          then quiet { rxFsmState = RX_WRITE }
          else quiet

      RX_WRITE ->
        let
          curWrite  = rxWriteCount current
          nBody     = rxBodyTotal current
          wireTotal = fromIntegral (2 + nBody + 2) :: Signed 32
          pos       = wireTotal - curWrite

          wordData
            | pos == 0           = rxHdrDw0 current
            | pos == 1           = rxHdrDw1 current
            | pos >= 2 && pos < fromIntegral (2 + nBody)
                                 = let bodyIdx = fromIntegral (pos - 2) :: Unsigned 16
                                   in rxBodyBuf current !! bodyIdx
            | curWrite == 1      = 0x0000000D
            | otherwise          = 0x00000000

          basePkt = nullPacketControl
            { data_valid = 1
            , pktData    = wordData
            }

          write_future = quiet
            { rxPkt        = basePkt
            , rxWriteCount = curWrite - 1
            }

        in if curWrite == wireTotal
             then write_future
               { rxPkt = basePkt { pkt_sop = 1 } }
           else if curWrite == 1
             then let newId = if rxPktId current > 65000 then 0
                              else rxPktId current + 1
                  in write_future
                    { rxPkt      = basePkt { pkt_eop = 1 }
                    , rxFsmState = RX_IDLE
                    , rxPktId    = newId
                    }
           else write_future

-- =============================================================================
-- Mealy Machine
-- =============================================================================

rxMealy
  :: StateRx
  -> (Bit, Bit, Bit, BitVector 32, Bool)
  -> (StateRx, (PacketControl, Bool))
rxMealy current (rx_en, rx_pkt_en, rx_pkt_rdy, fData, fEmpty) =
  let future = rxStateComb current rx_en rx_pkt_en rx_pkt_rdy fData fEmpty
      output = (rxPkt current, rxFifoRead current)
  in (future, output)
