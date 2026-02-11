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
import GNodeBFAPITypes

-- =============================================================================
-- RX Types (FPGA → Host response serialiser)
-- =============================================================================

data FsmRx
  = RX_IDLE          -- ^ Waiting for frValid
  | RX_HOLDOFF       -- ^ Inter-packet gap
  | RX_WAITED        -- ^ Waiting for rx_packet_ready
  | RX_WRITE         -- ^ Writing packet words (SOP then EOP)
  deriving (Show, Eq, Generic, NFDataX)

data RxConfig = RxConfig
  { rxGap :: Int
  } deriving (Show, Eq, Generic, NFDataX)

defaultRxConfig :: RxConfig
defaultRxConfig = RxConfig { rxGap = 10 }

-- | RX state record — uses the SAME shape as the original working PoC.
-- Output is PacketControl directly (not a tuple).
data StateRx = StateRx
  { rxState     :: FsmRx
  , rxHoldCount :: Signed 32
  , rxWriteCount :: Signed 32     -- ^ Counts down: 2 = SOP, 1 = EOP
  , rxPktId     :: Unsigned 16
  , rxPkt       :: PacketControl  -- ^ Current output packet control
  , rxLatched   :: FapiResponse   -- ^ Latched response being serialised
  , rxSentFlag  :: Bit            -- ^ '1' after we latch; cleared when frValid drops
  } deriving (Show, Eq, Generic, NFDataX)

-- =============================================================================
-- State Initialization
-- =============================================================================

nullRxState :: StateRx
nullRxState = StateRx
  { rxState      = RX_IDLE
  , rxHoldCount  = 0
  , rxWriteCount = 0
  , rxPktId      = 0
  , rxPkt        = nullPacketControl
  , rxLatched    = nullFapiResponse
  , rxSentFlag   = 0
  }

-- =============================================================================
-- Combinational Logic
-- =============================================================================

-- | RX state machine — serialises a FapiResponse into a 2-word bladeRF packet.
-- Follows the EXACT same FSM shape as the original working PoC:
--   IDLE → HOLDOFF → WAITED → WRITE → IDLE
--
-- Output packet:
--   write_count=2 (SOP): [msg_type(8) | err_code(8) | phy_state(8) | pkt_id(8)]
--   write_count=1 (EOP): [0x0000000D] sentinel
--
-- No handshake back to TX. Uses rxSentFlag to avoid re-sending the same response:
--   - When we see frValid=1 and rxSentFlag=0, we latch and set rxSentFlag=1
--   - rxSentFlag is cleared when frValid drops to 0 (new response can be accepted)
rxStateComb
  :: RxConfig
  -> StateRx
  -> Bit              -- ^ rx_enable
  -> Bit              -- ^ rx_packet_enable
  -> Bit              -- ^ rx_packet_ready
  -> FapiResponse     -- ^ Response from TX side
  -> StateRx
rxStateComb config current rx_enable rx_packet_enable rx_packet_ready fapiResp =
  let
    -- Default: keep state, clear packet control (same pattern as original PoC)
    future = current { rxPkt = nullPacketControl }

    gapVal = fromIntegral (rxGap config)
    pktLen = 2 :: Signed 32   -- 2 words: SOP header + EOP sentinel

    -- Clear sentFlag when the TX side's frValid drops (new response can come)
    futureWithFlagUpdate =
      if frValid fapiResp == 0
        then future { rxSentFlag = 0 }
        else future
  in
    case rxState current of
      RX_IDLE ->
        let idle_future = futureWithFlagUpdate
              { rxHoldCount  = 0
              , rxWriteCount = 0
              }
        in
          if rx_enable == 1 && rx_packet_enable == 1
               && frValid fapiResp == 1
               && rxSentFlag current == 0
            then idle_future
              { rxState    = RX_HOLDOFF
              , rxLatched  = fapiResp
              , rxSentFlag = 1
              }
            else idle_future

      RX_HOLDOFF ->
        let hf = future
              { rxWriteCount = pktLen
              , rxHoldCount  = rxHoldCount current + 1
              }
        in
          if rxHoldCount current == gapVal
            then hf { rxState = RX_WAITED }
            else hf

      RX_WAITED ->
        if rx_packet_ready == 1
          then future { rxState = RX_WRITE }
          else future

      RX_WRITE ->
        let
          curWrite = rxWriteCount current
          resp     = rxLatched current

          -- Build the header word: [msg_type(8)|err_code(8)|phy_state(8)|pkt_id(8)]
          pkt_id_vec = pack (resize (rxPktId current) :: Unsigned 16)
          headerWord = (frMsgType resp)
                   ++# (frErrCode resp)
                   ++# (frPhyState resp)
                   ++# (slice d7 d0 pkt_id_vec)

          write_future = future
            { rxPkt = (rxPkt future)
                { data_valid = 1
                }
            , rxWriteCount = curWrite - 1
            }
        in
          if curWrite == pktLen
            then -- First word: SOP
              write_future
                { rxPkt = (rxPkt write_future)
                    { pkt_sop = 1
                    , pktData = headerWord
                    }
                }
          else if curWrite == 1
            then -- Last word: EOP
              let newId = if rxPktId current > 65000 then 0
                          else rxPktId current + 1
              in write_future
                { rxPkt = (rxPkt write_future)
                    { pkt_eop = 1
                    , pktData = 0x0000000D
                    }
                , rxState = RX_IDLE
                , rxPktId = newId
                }
          else -- Middle words (not used for 2-word packets, but safe)
            write_future
              { rxPkt = (rxPkt write_future) { pktData = 0 }
              }

-- =============================================================================
-- Mealy Machine — returns PacketControl directly, same as original PoC
-- =============================================================================

rxMealy
  :: RxConfig
  -> StateRx
  -> (Bit, Bit, Bit, FapiResponse)
  -> (StateRx, PacketControl)        -- ^ Plain PacketControl, no tuple
rxMealy config current (rx_enable, rx_pkt_enable, rx_pkt_ready, fapiResp) =
  let
    future = rxStateComb config current rx_enable rx_pkt_enable rx_pkt_ready fapiResp
    output = rxPkt current   -- Registered output, same as original PoC
  in
    (future, output)
