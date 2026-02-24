{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
--    Adding a new message type (e.g. UL_TTI body parsing) requires:
--     (1) create FapiMsgUlTti.hs with a parser + response builder,
--     (2) add a parser state record to Types,
--     (3) add a field to StateTx,
--     (4) add a case to each dispatch function. No changes to the FSM logic itself.


-- | TX-side Mealy FSM shell.
--
--   This module contains only the transport-level FSM (header parsing,
--   wire protocol, CDC FIFO push) and delegates message-specific body
--   parsing and response generation to per-message modules:
--
--     FapiMsgConfig  — PARAM, CONFIG, START, STOP
--     FapiMsgSlot    — UL_TTI, UL_DCI (generic slot ack)
--     FapiMsgDlTti   — DL_TTI.request body parsing + DL_TTI.response
--     FapiMsgTxData  — TX_DATA.request body parsing + TB capture

module GNodeBTX
  ( FsmTx(..), StateTx(..), TxOutput(..)
  , nullTxState, txMealy, calcLeds
  ) where

import Clash.Prelude
import GNodeBFAPITypes
import FapiMsgConfig  (processConfigMsg, nextPhyState)
import FapiMsgSlot    (processSlotMsg)
import FapiMsgDlTti   (parseDlTtiDword, processDlTtiResponse, buildDlTtiCdcBody)
import FapiMsgTxData  (parseTxDataDword, processTxDataResponse)

-- =============================================================================
-- FSM States
-- =============================================================================

data FsmTx
  = TX_IDLE
  | TX_WAIT_FOR_SOP
  | TX_READ_HDR_DW1
  | TX_DRAIN_PAYLOAD
  | TX_PUSH_CDC
  | TX_ERROR
  deriving (Show, Eq, Generic, NFDataX)

-- =============================================================================
-- TX State
-- =============================================================================
--
-- The FSM state is split into:
--   • Transport-level fields (common to all messages)
--   • Per-message parser sub-states (opaque to this module)

data StateTx = StateTx
  { -- Transport-level
    txFsm            :: FsmTx
  , txReadyForPacket :: Bit
  , txReadNextWord   :: Bit
  , txReadDwords     :: Unsigned 16
  , txPhyState       :: PhyState
  , txParsedReq      :: FapiParsedReq
  , txRespPayload    :: FapiRespPayload
  , txHdrDw0         :: BitVector 32
  , txHdrDw1         :: BitVector 32
  , txBodyCaptured   :: Bool
    -- CDC push
  , txCdcWords       :: Vec 16 (BitVector 32)
  , txCdcTotal       :: Unsigned 16
  , txCdcIdx         :: Unsigned 16
    -- Per-message parser sub-states
  , txDlTtiPs        :: DlTtiParseState
  , txTxDataPs       :: TxDataParseState
  } deriving (Show, Eq, Generic, NFDataX)

data TxOutput = TxOutput
  { tx_packet_ready :: Bit
  , txFifoWrite     :: Maybe (BitVector 32)
  , leds            :: BitVector 3
  , txoTbReady      :: Bit
  , txoTbLenDwords  :: Unsigned 16
  , txoTbPduIndex   :: BitVector 16
  , txoTbBaseGraph  :: CbBaseGraph  -- ^ BG1 or BG2 for the LDPC encoder
  } deriving (Show, Eq, Generic, NFDataX)

-- =============================================================================
-- Initial State
-- =============================================================================

nullTxState :: StateTx
nullTxState = StateTx
  { txFsm            = TX_IDLE
  , txReadyForPacket = 0
  , txReadNextWord   = 0
  , txReadDwords     = 0
  , txPhyState       = PHY_IDLE
  , txParsedReq      = nullFapiParsedReq
  , txRespPayload    = nullFapiRespPayload
  , txHdrDw0         = 0
  , txHdrDw1         = 0
  , txBodyCaptured   = False
  , txCdcWords       = repeat 0
  , txCdcTotal       = 0
  , txCdcIdx         = 0
  , txDlTtiPs        = nullDlTtiParseState
  , txTxDataPs       = nullTxDataParseState
  }

-- =============================================================================
-- Message Dispatch: response generation
-- =============================================================================
--
-- Given the message type, delegates to the appropriate module to
-- produce the response payload.

dispatchResponse :: PhyState -> FapiParsedReq -> DlTtiParseState
                 -> FapiRespPayload
dispatchResponse curPhy req dlTtiPs =
  let msgType = fapiMsgTypeFromId (prMsgType req)
  in case msgType of
    -- P5 config messages
    FAPI_PARAM_REQUEST  -> fst (processConfigMsg curPhy req)
    FAPI_CONFIG_REQUEST -> fst (processConfigMsg curPhy req)
    FAPI_START_REQUEST  -> fst (processConfigMsg curPhy req)
    FAPI_STOP_REQUEST   -> fst (processConfigMsg curPhy req)

    -- DL_TTI.request → DL_TTI.response
    FAPI_DL_TTI_REQUEST -> processDlTtiResponse curPhy req (dpInfo dlTtiPs)

    -- TX_DATA.request → SLOT.indication ack
    FAPI_TX_DATA_REQUEST -> processTxDataResponse curPhy req

    -- Generic slot-level messages
    FAPI_UL_TTI_REQUEST -> processSlotMsg curPhy req
    FAPI_UL_DCI_REQUEST -> processSlotMsg curPhy req

    -- Unknown / unsupported
    _ -> processSlotMsg curPhy req

-- =============================================================================
-- Message Dispatch: PHY state transition
-- =============================================================================

dispatchPhyTransition :: PhyState -> FapiMsgType -> PhyState
dispatchPhyTransition = nextPhyState

-- =============================================================================
-- CDC Word Builder
-- =============================================================================
--
-- Packs the response header + body into a flat Vec for CDC FIFO push.
-- Delegates body DW construction for DL_TTI.response to its module.

buildCdcWords :: FapiRespPayload -> DlTtiParseState
              -> (Vec 16 (BitVector 32), Unsigned 16)
buildCdcWords rp dlTtiPs =
  let dw0  = packRespHeaderDw0 rp
      dw1  = packRespHeaderDw1 rp
      nBody = bodyLenToDwords (rpMsgLen rp)

      isDlTtiResp = rpMsgType rp == fapiMsgTypeId FAPI_DL_TTI_RESPONSE

      -- Standard body dwords
      stdBdw0 = if nBody >= 2
                  then rpSfn rp ++# rpSlot rp
                  else rpErrCode rp ++# rpPhyState rp ++# (0 :: BitVector 16)
      stdBdw1 = rpErrCode rp ++# rpPhyState rp ++# (0 :: BitVector 16)

      -- DL_TTI-specific body dwords
      (dltBdw0, dltBdw1) = buildDlTtiCdcBody rp (dpInfo dlTtiPs)

      bdw0 = if isDlTtiResp then dltBdw0 else stdBdw0
      bdw1 = if isDlTtiResp then dltBdw1 else stdBdw1

      total = 2 + nBody

      cdcWords = dw0  :> dw1  :> bdw0  :> bdw1 :>
              0    :> 0    :> 0     :> 0    :>
              0    :> 0    :> 0     :> 0    :>
              0    :> 0    :> 0     :> 0    :> Nil

  in (cdcWords, total)

-- =============================================================================
-- Message Dispatch: body dword parsing
-- =============================================================================
--
-- Called for each body dword in TX_DRAIN_PAYLOAD.  Routes to the
-- appropriate per-message parser.

dispatchBodyDword :: StateTx -> FapiMsgType -> BitVector 32 -> Unsigned 16
                  -> StateTx
dispatchBodyDword st msgType dw bodyIdx =
  case msgType of
    FAPI_DL_TTI_REQUEST ->
      let ps' = parseDlTtiDword (txDlTtiPs st) dw bodyIdx
      in st { txDlTtiPs = ps' }

    FAPI_TX_DATA_REQUEST ->
      let ps' = parseTxDataDword (txTxDataPs st) dw bodyIdx
      in st { txTxDataPs = ps' }

    -- Config and generic slot messages: no body parsing needed
    _ -> st

-- =============================================================================
-- TX Combinational Logic
-- =============================================================================

txStateComb :: StateTx -> Bit -> PacketControl -> Bit -> StateTx
txStateComb current@StateTx{..} tx_packet_empty tx_pkt_ctrl fifoFull =
  let future = current { txReadNextWord = 0 }
  in case txFsm of

    TX_IDLE ->
      if tx_packet_empty == 0
        then future { txFsm = TX_WAIT_FOR_SOP }
        else future

    TX_WAIT_FOR_SOP ->
      let withReady = future { txReadyForPacket = 1 }
      in if pkt_sop tx_pkt_ctrl == 1 && data_valid tx_pkt_ctrl == 1
           then
             let hdw0 = pktData tx_pkt_ctrl
             in withReady
                  { txReadyForPacket = 0
                  , txFsm            = TX_READ_HDR_DW1
                  , txReadDwords     = 1
                  , txHdrDw0         = hdw0
                  , txReadNextWord   = 1
                  , txBodyCaptured   = False
                  -- Reset per-message parser states
                  , txDlTtiPs        = nullDlTtiParseState
                  , txTxDataPs       = nullTxDataParseState
                  }
           else withReady

    TX_READ_HDR_DW1 ->
      let withRead = future { txReadNextWord = 1 }
      in if data_valid tx_pkt_ctrl == 1
           then
             let hdw1 = pktData tx_pkt_ctrl
                 (msgTypeId, msgLen) = parseHeaderDw1 hdw1
                 (_, handle, phyIdRaw) = parseHeaderDw0 txHdrDw0
                 phyId   = resize phyIdRaw
                 isDone  = pkt_eop tx_pkt_ctrl == 1
                 msgType = fapiMsgTypeFromId msgTypeId

                 parsedReq = nullFapiParsedReq
                   { prValid   = 1
                   , prMsgType = msgTypeId
                   , prHandle  = handle
                   , prPhyId   = phyId
                   , prBodyLen = msgLen
                   , prBodyDw0 = 0
                   }

             in if isDone
                  then
                    -- No body: process immediately
                    let resp = dispatchResponse txPhyState parsedReq txDlTtiPs
                        newPhy = dispatchPhyTransition txPhyState msgType
                        (cdcWords, total) = buildCdcWords resp txDlTtiPs
                    in withRead
                         { txFsm         = TX_PUSH_CDC
                         , txReadDwords  = 2
                         , txPhyState    = newPhy
                         , txParsedReq   = parsedReq
                         , txRespPayload = resp
                         , txCdcWords    = cdcWords
                         , txCdcTotal    = total
                         , txCdcIdx      = 0
                         , txHdrDw1      = hdw1
                         , txReadNextWord = 0
                         }
                  else
                    -- Body follows — drain and parse
                    withRead
                      { txFsm         = TX_DRAIN_PAYLOAD
                      , txReadDwords  = 2
                      , txParsedReq   = parsedReq
                      , txHdrDw1      = hdw1
                      , txReadNextWord = 1
                      , txBodyCaptured = False
                      }
           else withRead

    TX_DRAIN_PAYLOAD ->
      let withRead = future { txReadNextWord = 1 }
      in if data_valid tx_pkt_ctrl == 1
           then
             let dwordsNow = txReadDwords + 1
                 curData   = pktData tx_pkt_ctrl
                 msgTypeId = prMsgType txParsedReq
                 msgType   = fapiMsgTypeFromId msgTypeId
                 bodyIdx   = dwordsNow - 3

                 -- Capture first body dword for SFN/slot
                 isFirstBody = not txBodyCaptured && dwordsNow == 3
                 updatedReq  = if isFirstBody
                                 then txParsedReq { prBodyDw0 = curData }
                                 else txParsedReq
                 captured    = txBodyCaptured || isFirstBody

                 st1 = withRead
                   { txReadDwords   = dwordsNow
                   , txParsedReq    = updatedReq
                   , txBodyCaptured = captured
                   }

                 -- Delegate body parsing to per-message module
                 stParsed = dispatchBodyDword st1 msgType curData bodyIdx

             in if pkt_eop tx_pkt_ctrl == 1
                  then
                    let StateTx { txParsedReq = spReq
                                , txDlTtiPs   = spDlTti } = stParsed
                        resp = dispatchResponse txPhyState spReq spDlTti
                        newPhy = dispatchPhyTransition txPhyState msgType
                        (cdcWords, total) = buildCdcWords resp spDlTti
                    in stParsed
                         { txFsm         = TX_PUSH_CDC
                         , txReadNextWord = 0
                         , txPhyState    = newPhy
                         , txRespPayload = resp
                         , txCdcWords    = cdcWords
                         , txCdcTotal    = total
                         , txCdcIdx      = 0
                         }
                  else stParsed
           else withRead

    TX_PUSH_CDC ->
      if fifoFull == 0
        then
          let idx = txCdcIdx
              nextIdx = idx + 1
          in if nextIdx >= txCdcTotal
               then future { txFsm = TX_IDLE, txCdcIdx = nextIdx }
               else future { txCdcIdx = nextIdx }
        else future

    TX_ERROR -> future { txFsm = TX_IDLE }

-- =============================================================================
-- LEDs
-- =============================================================================

calcLeds :: FsmTx -> BitVector 3
calcLeds TX_IDLE          = complement 0b111
calcLeds TX_WAIT_FOR_SOP  = complement 0b001
calcLeds TX_READ_HDR_DW1  = complement 0b010
calcLeds TX_DRAIN_PAYLOAD = complement 0b010
calcLeds TX_PUSH_CDC      = complement 0b011
calcLeds TX_ERROR         = complement 0b110

-- =============================================================================
-- Mealy Machine
-- =============================================================================

txMealy :: StateTx -> (Bit, PacketControl, Bit) -> (StateTx, TxOutput)
txMealy current (tx_packet_empty, tx_packet_control, fifoFull) =
  let
    future = txStateComb current tx_packet_empty tx_packet_control fifoFull

    tx_ready = case txFsm current of
      TX_WAIT_FOR_SOP  -> txReadyForPacket current .|. txReadNextWord current
      TX_READ_HDR_DW1  -> txReadNextWord current
      TX_DRAIN_PAYLOAD -> txReadNextWord current
      _                -> 0

    fifoWr = case txFsm current of
      TX_PUSH_CDC ->
        if fifoFull == 0
          then let idx = txCdcIdx current
               in Just (txCdcWords current !! idx)
          else Nothing
      _ -> Nothing

    -- TB status from TX_DATA parser sub-state
    tbBuf = tpTbBuffer (txTxDataPs current)

    output = TxOutput
      { tx_packet_ready = tx_ready
      , txFifoWrite     = fifoWr
      , leds            = calcLeds (txFsm current)
      , txoTbReady      = tbReady tbBuf
      , txoTbLenDwords  = tbLenDwords tbBuf
      , txoTbPduIndex   = tbPduIndex tbBuf
      , txoTbBaseGraph  = tpCbBaseGraph (txTxDataPs current)
      }
  in (future, output)
