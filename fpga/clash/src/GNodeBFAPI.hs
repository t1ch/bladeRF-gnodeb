{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE TypeFamilies #-}

module GNodeBFAPI where

import Clash.Explicit.Prelude
import Clash.Sized.Vector (Vec, (!!), replace)
import GHC.Generics (Generic)
import Data.Bits ((.|.), shiftL, shiftR, (.&.))
import Clash.Sized.BitVector (BitVector)
import Clash.Explicit.Synchronizer (asyncFIFOSynchronizer)

-- Clock domains for TX (host->FPGA) and RX (FPGA->host)
-- Use createDomain to properly define domains
createDomain vSystem{vName="TxDom", vPeriod=hzToPeriod 80_000_000}
createDomain vSystem{vName="RxDom", vPeriod=hzToPeriod 80_000_000}

-- =============================================================================
-- BLADERF_FORMAT_PACKET_META Structure (matches wlan_top.vhd and C code)
-- =============================================================================

-- TX Header (from host to FPGA) - 16 bytes / 4 DWORDs
data BladeRFTxHeader = BladeRFTxHeader
  { txhLen        :: BitVector 16  -- Payload length in bytes
  , txhModulation :: BitVector 8   -- Modulation (repurposed for FAPI msg type)
  , txhBandwidth  :: BitVector 8   -- Bandwidth (repurposed for FAPI flags)
  , txhCookie     :: BitVector 32  -- Transaction cookie
  , txhRsvd       :: BitVector 32  -- Reserved
  } deriving (Generic, NFDataX, Show, Eq, BitPack)

-- RX Header (from FPGA to host) - 16 bytes / 4 DWORDs
data BladeRFRxHeader = BladeRFRxHeader
  { rxhType       :: BitVector 16  -- 1=Packet, 2=ACK, 3=Missing ACK
  , rxhLen        :: BitVector 16  -- Payload length (for Type=1)
  , rxhRsvd2      :: BitVector 32  -- Reserved/Cookie (for Type!=1)
  , rxhModulation :: BitVector 8   -- Modulation/Status
  , rxhBandwidth  :: BitVector 8   -- Bandwidth
  , rxhRsvd3      :: BitVector 16  -- Reserved
  } deriving (Generic, NFDataX, Show, Eq, BitPack)

-- =============================================================================
-- FAPI Message Structures (Based on SCF-222 5G FAPI PHY API Spec)
-- =============================================================================

-- FAPI Message Header (as per spec section 3.3.1)
-- Typical format: num_msg (8 bits) | handle (8 bits) | msg_id (16 bits)
data FapiMessageHeader = FapiMessageHeader
  { fapiNumMsg :: BitVector 8   -- Number of messages in payload
  , fapiHandle :: BitVector 8   -- Transaction handle
  , fapiMsgId  :: BitVector 16  -- Message identifier
  } deriving (Generic, NFDataX, Show, Eq, BitPack)

-- FAPI P5 Message IDs (Control Plane)
paramRequest, paramResponse, configRequest, configResponse :: BitVector 16
startRequest, stopRequest, stopIndication :: BitVector 16
errorIndication, resetRequest, resetIndication :: BitVector 16

paramRequest     = 0x0000
paramResponse    = 0x0001
configRequest    = 0x0002
configResponse   = 0x0003
startRequest     = 0x0004
stopRequest      = 0x0005
stopIndication   = 0x0006
errorIndication  = 0x0007
resetRequest     = 0x0008
resetIndication  = 0x0009

-- FAPI P7 Message IDs (Data Plane)
slotIndication, dlTtiRequest, ulTtiRequest :: BitVector 16
ulDciRequest, txDataRequest, rxDataIndication :: BitVector 16
crcIndication, uciIndication, srsIndication :: BitVector 16
rachiIndication, csiRsIndication :: BitVector 16

slotIndication    = 0x0082
dlTtiRequest      = 0x0080
ulTtiRequest      = 0x0081
ulDciRequest      = 0x0083
txDataRequest     = 0x0084
rxDataIndication  = 0x0085
crcIndication     = 0x0086
uciIndication     = 0x0087
srsIndication     = 0x0088
rachiIndication   = 0x0089
csiRsIndication   = 0x008A

-- FAPI Error Codes
msgOk, msgInvalidState, msgInvalidConfig :: BitVector 8
msgOk             = 0x00
msgInvalidState   = 0x01
msgInvalidConfig  = 0x02

-- =============================================================================
-- PHY State Machine (Based on FAPI State Transitions)
-- =============================================================================

data PhyState
  = PhyIdle         -- Initial state
  | PhyConfigured   -- After CONFIG.request
  | PhyRunning      -- After START.request
  deriving (Generic, NFDataX, Eq, Show, BitPack)

-- =============================================================================
-- TX FSM States (Receiving from host)
-- =============================================================================

data TxFsmState
  = TX_Idle
  | TX_ReadHeader       -- Reading 4 DWORDs of BladeRF header
  | TX_ReadPayload      -- Reading FAPI message payload
  | TX_ProcessMessage   -- Process complete FAPI message
  deriving (Generic, NFDataX, Eq, Show)

-- =============================================================================
-- RX FSM States (Sending to host)
-- =============================================================================

data RxFsmState
  = RX_Idle
  | RX_WriteHeader      -- Writing BladeRF header
  | RX_WritePayload     -- Writing FAPI response payload
  deriving (Generic, NFDataX, Eq, Show)

-- =============================================================================
-- TX State Record
-- =============================================================================

type MaxPayloadDWords = 256  -- 1KB max payload

data TxState = TxState
  { txFsmState    :: TxFsmState
  , txPhyState    :: PhyState
  , txHeader      :: BladeRFTxHeader
  , txPayload     :: Vec MaxPayloadDWords (BitVector 32)
  , txWordCount   :: Unsigned 8
  , txReadyFlag   :: Bit
  , txTriggerMsg  :: Maybe (PhyState, FapiMessageHeader, BitVector 32) -- (newState, msgHdr, msgBody)
  } deriving (Generic, NFDataX)

nullTxState :: TxState
nullTxState = TxState
  { txFsmState    = TX_Idle
  , txPhyState    = PhyIdle
  , txHeader      = BladeRFTxHeader 0 0 0 0 0
  , txPayload     = repeat 0
  , txWordCount   = 0
  , txReadyFlag   = 1
  , txTriggerMsg  = Nothing
  }

-- =============================================================================
-- RX State Record
-- =============================================================================

type MaxRxDWords = 256

data RxState = RxState
  { rxFsmState    :: RxFsmState
  , rxHeader      :: BladeRFRxHeader
  , rxPayload     :: Vec MaxRxDWords (BitVector 32)
  , rxWordCount   :: Unsigned 8
  , rxTotalWords  :: Unsigned 8
  } deriving (Generic, NFDataX)

nullRxState :: RxState
nullRxState = RxState
  { rxFsmState    = RX_Idle
  , rxHeader      = BladeRFRxHeader 0 0 0 0 0 0
  , rxPayload     = repeat 0
  , rxWordCount   = 0
  , rxTotalWords  = 0
  }

-- =============================================================================
-- TX FSM Logic (Parsing incoming BladeRF packets with FAPI messages)
-- =============================================================================

txFsm :: TxState -> (Bit, Bit, BitVector 32, Bit) -> (TxState, Bit)
txFsm st (sop, eop, dataIn, valid) =
  case txFsmState st of
    TX_Idle ->
      if sop == 1 && valid == 1
        then
          let hdrBits = pack (txHeader st) :: BitVector 128
              -- Store first DWORD in lower 32 bits
              newHdrBits = (hdrBits .&. 0xFFFFFFFF_FFFFFFFF_FFFFFFFF_00000000) .|.
                           (resize dataIn :: BitVector 128)
              newHdr = unpack newHdrBits :: BladeRFTxHeader
          in (st { txFsmState = TX_ReadHeader
                 , txWordCount = 1
                 , txHeader = newHdr
                 , txReadyFlag = 0
                 }, 0)
        else (st { txReadyFlag = 1 }, 0)

    TX_ReadHeader ->
      let cnt = txWordCount st
          hdrBits = pack (txHeader st) :: BitVector 128

          -- Build header across 4 DWORDs
          newHdrBits = case cnt of
            1 -> (hdrBits .&. 0xFFFFFFFF_00000000_00000000_00000000) .|.
                 ((resize dataIn :: BitVector 128) `shiftL` 64)
            2 -> (hdrBits .&. 0xFFFFFFFF_FFFFFFFF_00000000_00000000) .|.
                 ((resize dataIn :: BitVector 128) `shiftL` 32)
            3 -> (hdrBits .&. 0xFFFFFFFF_FFFFFFFF_FFFFFFFF_00000000) .|.
                 (resize dataIn :: BitVector 128)
            _ -> hdrBits

          newHdr = unpack newHdrBits :: BladeRFTxHeader

          -- Calculate payload size in DWORDs
          lenBits = txhLen (txHeader st) :: BitVector 16
          lenUnsigned = bitCoerce lenBits :: Unsigned 16
          payloadDWords = resize (lenUnsigned + 3) `shiftR` 2 :: Unsigned 8

          nextState = if cnt >= 3
                        then if payloadDWords > 0
                               then TX_ReadPayload
                               else TX_ProcessMessage
                        else TX_ReadHeader
      in if valid == 1
           then (st { txFsmState = nextState
                    , txHeader = newHdr
                    , txWordCount = if cnt >= 3 then 0 else cnt + 1
                    }, 0)
           else (st, 0)

    TX_ReadPayload ->
      let cnt = txWordCount st
          payloadDWords = (extend (txhLen (txHeader st)) + 3) `shiftR` 2
          newPayload = replace (bitCoerce cnt :: Index MaxPayloadDWords) dataIn (txPayload st)

          nextState = if cnt + 1 >= resize payloadDWords || eop == 1
                        then TX_ProcessMessage
                        else TX_ReadPayload
      in if valid == 1
           then (st { txFsmState = nextState
                    , txPayload = newPayload
                    , txWordCount = cnt + 1
                    }, 0)
           else (st, 0)

    TX_ProcessMessage ->
      let fapiHdr = unpack (txPayload st !! 0) :: FapiMessageHeader
          msgBody = txPayload st !! 1  -- First body DWORD
          msgId = fapiMsgId fapiHdr
          currentPhy = txPhyState st

          -- State transition based on message type
          nextPhy = case currentPhy of
            PhyIdle ->
              if msgId == configRequest then PhyConfigured else currentPhy
            PhyConfigured ->
              if msgId == startRequest then PhyRunning
              else if msgId == resetRequest then PhyIdle
              else currentPhy
            PhyRunning ->
              if msgId == stopRequest then PhyConfigured
              else if msgId == resetRequest then PhyIdle
              else currentPhy

          trigger = Just (nextPhy, fapiHdr, msgBody)

      in (st { txFsmState = TX_Idle
             , txPhyState = nextPhy
             , txWordCount = 0
             , txReadyFlag = 1
             , txTriggerMsg = trigger
             }, 1)  -- Trigger valid

-- =============================================================================
-- RX FSM Logic (Sending BladeRF packets with FAPI responses to host)
-- =============================================================================

rxFsm :: RxState -> (Bit, Maybe (PhyState, FapiMessageHeader, BitVector 32))
      -> (RxState, (Bit, Bit, BitVector 32, Bit))
rxFsm st (ready, trigger) =
  case rxFsmState st of
    RX_Idle ->
      case trigger of
        Just (phyState, msgHdr, msgBody) ->
          let (respHdr, respPayload, respLen) = buildFapiResponse phyState msgHdr msgBody
              -- Build BladeRF RX header
              bladeHdr = BladeRFRxHeader
                { rxhType = 1  -- Packet type
                , rxhLen = bitCoerce (resize respLen :: Unsigned 16) * 4  -- Convert DWORDs to bytes
                , rxhRsvd2 = 0
                , rxhModulation = fapiMsgId msgHdr .&. 0xFF  -- Store msg ID
                , rxhBandwidth = 0
                , rxhRsvd3 = 0
                }
              -- Pack header into 4 DWORDs
              hdrBits = pack bladeHdr :: BitVector 128
              hdr0 = resize (hdrBits `shiftR` 96) :: BitVector 32
              hdr1 = resize (hdrBits `shiftR` 64) :: BitVector 32
              hdr2 = resize (hdrBits `shiftR` 32) :: BitVector 32
              hdr3 = resize hdrBits :: BitVector 32

              fullPayload = hdr0 :> hdr1 :> hdr2 :> hdr3 :> respHdr :> respPayload
          in (st { rxFsmState = RX_WriteHeader
                 , rxHeader = bladeHdr
                 , rxPayload = fullPayload
                 , rxWordCount = 0
                 , rxTotalWords = 4 + resize respLen  -- Header + payload
                 }, (1, 0, hdr0, 1))  -- SOP=1, EOP=0, valid=1

        Nothing ->
          (st, (0, 0, 0, 0))

    RX_WriteHeader ->
      let cnt = rxWordCount st
          dataOut = rxPayload st !! bitCoerce (cnt + 1)
          totalWords = rxTotalWords st

          isLastWord = (cnt + 2) >= totalWords
          nextState = if isLastWord then RX_Idle else RX_WriteHeader

      in if ready == 1 || cnt > 0
           then (st { rxFsmState = nextState
                    , rxWordCount = if isLastWord then 0 else cnt + 1
                    }, (0, if isLastWord then 1 else 0, dataOut, 1))
           else (st, (0, 0, dataOut, 1))

-- =============================================================================
-- FAPI Response Builder
-- =============================================================================

buildFapiResponse :: PhyState -> FapiMessageHeader -> BitVector 32
                  -> (BitVector 32, Vec MaxRxDWords (BitVector 32), Unsigned 8)
buildFapiResponse phyState msgHdr msgBody =
  let msgId = fapiMsgId msgHdr

      -- Build response header
      buildHdr respMsgId = FapiMessageHeader
        { fapiNumMsg = 1
        , fapiHandle = fapiHandle msgHdr
        , fapiMsgId = respMsgId
        }

      -- Single DWORD response (header only in payload)
      singleResp respMsgId =
        (pack (buildHdr respMsgId), repeat 0, 1)

      -- Header + 1 DWORD body response
      bodyResp respMsgId bodyData =
        let hdr = pack (buildHdr respMsgId)
            payload = replace 0 bodyData (repeat 0)
        in (hdr, payload, 2)

      -- Error response
      errorResp errMsgId errCode =
        let hdr = pack (buildHdr errorIndication)
            body = (extend errMsgId `shiftL` 16) .|. (extend errCode `shiftL` 8) .|. extend (fapiHandle msgHdr)
            payload = replace 0 body (repeat 0)
        in (hdr, payload, 2)

  in case phyState of
       PhyIdle -> case msgId of
         _ | msgId == paramRequest  -> bodyResp paramResponse (extend msgOk)
         _ | msgId == configRequest -> bodyResp configResponse (extend msgOk)
         _ | msgId == startRequest  -> errorResp startRequest msgInvalidState
         _ | msgId == stopRequest   -> errorResp stopRequest msgInvalidState
         _ | msgId == resetRequest  -> errorResp resetRequest msgInvalidState
         _ -> (0, repeat 0, 0)

       PhyConfigured -> case msgId of
         _ | msgId == paramRequest  -> bodyResp paramResponse (extend msgOk)
         _ | msgId == configRequest -> bodyResp configResponse (extend msgOk)
         _ | msgId == startRequest  -> bodyResp slotIndication 0  -- Start sending slot indications
         _ | msgId == stopRequest   -> errorResp stopRequest msgInvalidState
         _ | msgId == resetRequest  -> singleResp resetIndication
         _ -> (0, repeat 0, 0)

       PhyRunning -> case msgId of
         _ | msgId == configRequest -> bodyResp configResponse (extend msgOk)
         _ | msgId == startRequest  -> errorResp startRequest msgInvalidState
         _ | msgId == stopRequest   -> singleResp stopIndication
         _ | msgId == resetRequest  -> singleResp resetIndication
         _ | msgId == dlTtiRequest  -> (0, repeat 0, 0)  -- No response needed for DL_TTI
         _ | msgId == ulTtiRequest  -> (0, repeat 0, 0)  -- No response needed for UL_TTI
         _ | msgId == txDataRequest -> (0, repeat 0, 0)  -- No response needed for TX_Data
         _ -> (0, repeat 0, 0)

-- =============================================================================
-- Top Entity
-- =============================================================================

{-# ANN topEntity
  ( Synthesize
      { t_name = "gnodeb_fapi_bladerf"
      , t_inputs =
          [ PortName "tx_clock"
          , PortName "tx_reset"
          , PortName "tx_enable"
          , PortName "tx_sop"
          , PortName "tx_eop"
          , PortName "tx_data"
          , PortName "tx_valid"
          , PortName "rx_clock"
          , PortName "rx_reset"
          , PortName "rx_enable"
          , PortName "rx_ready"
          ]
      , t_output =
          PortProduct ""
            [ PortName "tx_ready"
            , PortName "rx_sop"
            , PortName "rx_eop"
            , PortName "rx_data"
            , PortName "rx_valid"
            , PortName "phy_state_leds"
            ]
      }
  ) #-}

topEntity ::
  Clock TxDom ->
  Reset TxDom ->
  Signal TxDom Bit ->
  Signal TxDom Bit ->
  Signal TxDom Bit ->
  Signal TxDom (BitVector 32) ->
  Signal TxDom Bit ->
  Clock RxDom ->
  Reset RxDom ->
  Signal RxDom Bit ->
  Signal RxDom Bit ->
  ( Signal TxDom Bit
  , Signal RxDom Bit
  , Signal RxDom Bit
  , Signal RxDom (BitVector 32)
  , Signal RxDom Bit
  , Signal TxDom (BitVector 3)
  )
topEntity txClk txRst txEna txSop txEop txData txValid
          rxClk rxRst rxEna rxReady =
  (txReadyOut, rxSopOut, rxEopOut, rxDataOut, rxValidOut, ledOut)
  where
    -- TX Domain
    txEnaS = toEnable (bitToBool <$> txEna)

    txMealy st input =
      let (nextSt, _trigValid) = txFsm st input
      in (nextSt, (txReadyFlag nextSt, txTriggerMsg nextSt))

    txOut = mealy txClk txRst txEnaS txMealy nullTxState
            (bundle (txSop, txEop, txData, txValid))

    txReadyOut = fst <$> txOut
    txTriggerOut = snd <$> txOut

    -- Clock Domain Crossing (TX -> RX)
    (fifoData, fifoNotEmpty, _fifoFull) =
      asyncFIFOSynchronizer
        d4
        txClk
        rxClk
        txRst
        rxRst
        txEnaS
        rxEnaS
        rxReadEnBool
        txTriggerOut

    -- RX Domain
    rxEnaS = toEnable (bitToBool <$> rxEna)

    rxMealy st (ready, (notEmpty, maybeTrigger)) =
      let triggerMaybe = if notEmpty then maybeTrigger else Nothing
          (nextSt, outputs) = rxFsm st (ready, triggerMaybe)
      in (nextSt, outputs)

    rxOut = mealy rxClk rxRst rxEnaS rxMealy nullRxState
            (bundle (rxReady, bundle (fifoNotEmpty, fifoData)))

    rxSopOut = (\(sop, _, _, _) -> sop) <$> rxOut
    rxEopOut = (\(_, eop, _, _) -> eop) <$> rxOut
    rxDataOut = (\(_, _, dat, _) -> dat) <$> rxOut
    rxValidOut = (\(_, _, _, valid) -> valid) <$> rxOut

    -- Read enable for FIFO (always read when data available in RX_Idle)
    rxReadEnBool = pure True

    -- LED status - use separate mealy to track PHY state from triggers
    ledMealy :: PhyState -> Maybe (PhyState, FapiMessageHeader, BitVector 32) -> (PhyState, BitVector 3)
    ledMealy currentState triggerMaybe =
      let newState = case triggerMaybe of
                       Just (phyState, _, _) -> phyState
                       Nothing -> currentState
          leds = phyStateToLeds newState
      in (newState, leds)

    ledOut = mealy txClk txRst txEnaS ledMealy PhyIdle txTriggerOut

    phyStateToLeds PhyIdle = complement 0b111  -- All LEDs on
    phyStateToLeds PhyConfigured = complement 0b001  -- LED0 on
    phyStateToLeds PhyRunning = complement 0b010  -- LED1 on
