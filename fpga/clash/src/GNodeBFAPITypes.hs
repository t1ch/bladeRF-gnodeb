{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}

module GNodeBFAPITypes
  ( -- * FAPI Message Type Classification
    FapiMsgType(..), fapiMsgTypeId, fapiMsgTypeFromId
    -- * PHY State
  , PhyState(..), phyStateToVal
    -- * SCF-222 Common Message Header
  , FapiHeader(..), nullFapiHeader, parseHeaderDw0, parseHeaderDw1
    -- * Parsed Request (after header extraction)
  , FapiParsedReq(..), nullFapiParsedReq
    -- * Response Payload
  , FapiRespPayload(..), nullFapiRespPayload
  , packRespHeaderDw0, packRespHeaderDw1
    -- * Error Codes
  , FapiErrorCode(..), fapiErrorCodeVal
    -- * Packet Control (bladeRF)
  , PacketControl(..), nullPacketControl
    -- * Body Parser Sub-FSM Phase
  , BodyParsePhase(..)
    -- * Per-message parser state records
  , DlTtiParseState(..), nullDlTtiParseState
  , TxDataParseState(..), nullTxDataParseState
    -- * NR CRC types
  , NrCrcType(..)
  , NrCrcState(..), nullNrCrcState
    -- * DL_TTI types
  , DlPduType(..), dlPduTypeFromId
  , DlTtiInfo(..), nullDlTtiInfo
  , PdschInfo(..), nullPdschInfo
  , PdcchInfo(..), nullPdcchInfo
  , SsbInfo(..), nullSsbInfo
  , CsiRsInfo(..), nullCsiRsInfo
    -- * TX_DATA types
  , TxDataInfo(..), nullTxDataInfo
  , TxDataPdu(..), nullTxDataPdu
    -- * Transport Block buffer
  , TbBuffer(..), nullTbBuffer
    -- * Utilities
  , bodyLenToDwords, maxBodyDwords
    -- * Constants
  , maxPdschPerSlot, maxPdcchPerSlot, maxSsbPerSlot, maxCsiRsPerSlot
  , maxTbDwords, maxCdcWords
  ) where

import Clash.Prelude

-- =============================================================================
-- FAPI Message Types (SCF-222 Table 3.2-1)
-- =============================================================================

data FapiMsgType
  = FAPI_PARAM_REQUEST              -- 0x00
  | FAPI_PARAM_RESPONSE             -- 0x01
  | FAPI_CONFIG_REQUEST             -- 0x02
  | FAPI_CONFIG_RESPONSE            -- 0x03
  | FAPI_START_REQUEST              -- 0x04
  | FAPI_STOP_REQUEST               -- 0x05
  | FAPI_STOP_INDICATION            -- 0x06
  | FAPI_ERROR_INDICATION           -- 0x07
  | FAPI_RESET_REQUEST              -- 0x08
  | FAPI_RESET_INDICATION           -- 0x09
  | FAPI_CONNECTIVITY_INDICATION    -- 0x0A
  | FAPI_DL_TTI_REQUEST             -- 0x80
  | FAPI_UL_TTI_REQUEST             -- 0x81
  | FAPI_SLOT_INDICATION            -- 0x82
  | FAPI_UL_DCI_REQUEST             -- 0x83
  | FAPI_TX_DATA_REQUEST            -- 0x84
  | FAPI_RX_DATA_INDICATION         -- 0x85
  | FAPI_CRC_INDICATION             -- 0x86
  | FAPI_UCI_INDICATION             -- 0x87
  | FAPI_SRS_INDICATION             -- 0x88
  | FAPI_RACH_INDICATION            -- 0x89
  | FAPI_DL_TTI_RESPONSE            -- 0x8A
  | FAPI_TIMING_INDICATION          -- 0x8B
  | FAPI_UL_METRICS_INDICATION      -- 0x8C
  | FAPI_RIM_RS_INDICATION          -- 0x8D
  | FAPI_ADV_SLEEP_CTRL_REQUEST     -- 0x8D (direction-dependent)
  | FAPI_ADV_SLEEP_CTRL_INDICATION  -- 0x8E
  | FAPI_MSG_UNKNOWN
  deriving (Show, Eq, Generic, NFDataX)

fapiMsgTypeId :: FapiMsgType -> BitVector 16
fapiMsgTypeId FAPI_PARAM_REQUEST             = 0x0000
fapiMsgTypeId FAPI_PARAM_RESPONSE            = 0x0001
fapiMsgTypeId FAPI_CONFIG_REQUEST            = 0x0002
fapiMsgTypeId FAPI_CONFIG_RESPONSE           = 0x0003
fapiMsgTypeId FAPI_START_REQUEST             = 0x0004
fapiMsgTypeId FAPI_STOP_REQUEST              = 0x0005
fapiMsgTypeId FAPI_STOP_INDICATION           = 0x0006
fapiMsgTypeId FAPI_ERROR_INDICATION          = 0x0007
fapiMsgTypeId FAPI_RESET_REQUEST             = 0x0008
fapiMsgTypeId FAPI_RESET_INDICATION          = 0x0009
fapiMsgTypeId FAPI_CONNECTIVITY_INDICATION   = 0x000A
fapiMsgTypeId FAPI_DL_TTI_REQUEST            = 0x0080
fapiMsgTypeId FAPI_UL_TTI_REQUEST            = 0x0081
fapiMsgTypeId FAPI_SLOT_INDICATION           = 0x0082
fapiMsgTypeId FAPI_UL_DCI_REQUEST            = 0x0083
fapiMsgTypeId FAPI_TX_DATA_REQUEST           = 0x0084
fapiMsgTypeId FAPI_RX_DATA_INDICATION        = 0x0085
fapiMsgTypeId FAPI_CRC_INDICATION            = 0x0086
fapiMsgTypeId FAPI_UCI_INDICATION            = 0x0087
fapiMsgTypeId FAPI_SRS_INDICATION            = 0x0088
fapiMsgTypeId FAPI_RACH_INDICATION           = 0x0089
fapiMsgTypeId FAPI_DL_TTI_RESPONSE           = 0x008A
fapiMsgTypeId FAPI_TIMING_INDICATION         = 0x008B
fapiMsgTypeId FAPI_UL_METRICS_INDICATION     = 0x008C
fapiMsgTypeId FAPI_RIM_RS_INDICATION         = 0x008D
fapiMsgTypeId FAPI_ADV_SLEEP_CTRL_REQUEST    = 0x008D
fapiMsgTypeId FAPI_ADV_SLEEP_CTRL_INDICATION = 0x008E
fapiMsgTypeId FAPI_MSG_UNKNOWN               = 0x00FF

fapiMsgTypeFromId :: BitVector 16 -> FapiMsgType
fapiMsgTypeFromId 0x0000 = FAPI_PARAM_REQUEST
fapiMsgTypeFromId 0x0001 = FAPI_PARAM_RESPONSE
fapiMsgTypeFromId 0x0002 = FAPI_CONFIG_REQUEST
fapiMsgTypeFromId 0x0003 = FAPI_CONFIG_RESPONSE
fapiMsgTypeFromId 0x0004 = FAPI_START_REQUEST
fapiMsgTypeFromId 0x0005 = FAPI_STOP_REQUEST
fapiMsgTypeFromId 0x0006 = FAPI_STOP_INDICATION
fapiMsgTypeFromId 0x0007 = FAPI_ERROR_INDICATION
fapiMsgTypeFromId 0x0008 = FAPI_RESET_REQUEST
fapiMsgTypeFromId 0x0009 = FAPI_RESET_INDICATION
fapiMsgTypeFromId 0x000A = FAPI_CONNECTIVITY_INDICATION
fapiMsgTypeFromId 0x0080 = FAPI_DL_TTI_REQUEST
fapiMsgTypeFromId 0x0081 = FAPI_UL_TTI_REQUEST
fapiMsgTypeFromId 0x0082 = FAPI_SLOT_INDICATION
fapiMsgTypeFromId 0x0083 = FAPI_UL_DCI_REQUEST
fapiMsgTypeFromId 0x0084 = FAPI_TX_DATA_REQUEST
fapiMsgTypeFromId 0x0085 = FAPI_RX_DATA_INDICATION
fapiMsgTypeFromId 0x0086 = FAPI_CRC_INDICATION
fapiMsgTypeFromId 0x0087 = FAPI_UCI_INDICATION
fapiMsgTypeFromId 0x0088 = FAPI_SRS_INDICATION
fapiMsgTypeFromId 0x0089 = FAPI_RACH_INDICATION
fapiMsgTypeFromId 0x008A = FAPI_DL_TTI_RESPONSE
fapiMsgTypeFromId 0x008B = FAPI_TIMING_INDICATION
fapiMsgTypeFromId 0x008C = FAPI_UL_METRICS_INDICATION
fapiMsgTypeFromId 0x008D = FAPI_RIM_RS_INDICATION
fapiMsgTypeFromId 0x008E = FAPI_ADV_SLEEP_CTRL_INDICATION
fapiMsgTypeFromId _      = FAPI_MSG_UNKNOWN

-- =============================================================================
-- PHY State
-- =============================================================================

data PhyState = PHY_IDLE | PHY_CONFIGURED | PHY_RUNNING
  deriving (Show, Eq, Generic, NFDataX)

phyStateToVal :: PhyState -> BitVector 8
phyStateToVal PHY_IDLE       = 0x00
phyStateToVal PHY_CONFIGURED = 0x01
phyStateToVal PHY_RUNNING    = 0x02

-- =============================================================================
-- SCF-222 Common Message Header
-- =============================================================================

data FapiHeader = FapiHeader
  { hdrNumMsg   :: BitVector 8
  , hdrHandle   :: BitVector 16
  , hdrPhyId    :: BitVector 8
  , hdrMsgType  :: BitVector 16
  , hdrMsgLen   :: BitVector 16
  } deriving (Show, Eq, Generic, NFDataX)

nullFapiHeader :: FapiHeader
nullFapiHeader = FapiHeader 0 0 0 0xFFFF 0

parseHeaderDw0 :: BitVector 32 -> (BitVector 8, BitVector 16, BitVector 8)
parseHeaderDw0 dw0 =
  let numMsg = slice d31 d24 dw0
      handle = slice d23 d8  dw0
      phyId  = slice d7  d0  dw0
  in (numMsg, handle, phyId)

parseHeaderDw1 :: BitVector 32 -> (BitVector 16, BitVector 16)
parseHeaderDw1 dw1 =
  let msgType = slice d31 d16 dw1
      msgLen  = slice d15 d0  dw1
  in (msgType, msgLen)

-- =============================================================================
-- Parsed Request
-- =============================================================================

data FapiParsedReq = FapiParsedReq
  { prValid     :: Bit
  , prMsgType   :: BitVector 16
  , prHandle    :: BitVector 16
  , prPhyId     :: BitVector 8
  , prBodyLen   :: BitVector 16
  , prBodyDw0   :: BitVector 32
  } deriving (Show, Eq, Generic, NFDataX)

nullFapiParsedReq :: FapiParsedReq
nullFapiParsedReq = FapiParsedReq 0 0xFFFF 0 0 0 0

-- =============================================================================
-- Response Payload
-- =============================================================================

data FapiRespPayload = FapiRespPayload
  { rpValid     :: Bit
  , rpMsgType   :: BitVector 16
  , rpHandle    :: BitVector 16
  , rpPhyId     :: BitVector 8
  , rpMsgLen    :: BitVector 16
  , rpErrCode   :: BitVector 8
  , rpPhyState  :: BitVector 8
  , rpSfn       :: BitVector 16
  , rpSlot      :: BitVector 16
  } deriving (Show, Eq, Generic, NFDataX)

nullFapiRespPayload :: FapiRespPayload
nullFapiRespPayload = FapiRespPayload 0 0xFFFF 0 0 0 0 0 0 0

packRespHeaderDw0 :: FapiRespPayload -> BitVector 32
packRespHeaderDw0 rp =
  (1 :: BitVector 8) ++# rpHandle rp ++# rpPhyId rp

packRespHeaderDw1 :: FapiRespPayload -> BitVector 32
packRespHeaderDw1 rp =
  rpMsgType rp ++# rpMsgLen rp

-- =============================================================================
-- Error Codes
-- =============================================================================

data FapiErrorCode
  = FAPI_MSG_OK | FAPI_MSG_INVALID_STATE | FAPI_MSG_INVALID_CONFIG
  | FAPI_SFN_OUT_OF_SYNC | FAPI_MSG_SLOT_ERR
  | FAPI_MSG_BCH_MISSING | FAPI_MSG_INVALID_SFN
  deriving (Show, Eq, Generic, NFDataX)

fapiErrorCodeVal :: FapiErrorCode -> BitVector 8
fapiErrorCodeVal FAPI_MSG_OK             = 0x00
fapiErrorCodeVal FAPI_MSG_INVALID_STATE  = 0x01
fapiErrorCodeVal FAPI_MSG_INVALID_CONFIG = 0x02
fapiErrorCodeVal FAPI_SFN_OUT_OF_SYNC   = 0x03
fapiErrorCodeVal FAPI_MSG_SLOT_ERR      = 0x04
fapiErrorCodeVal FAPI_MSG_BCH_MISSING   = 0x05
fapiErrorCodeVal FAPI_MSG_INVALID_SFN   = 0x06

-- =============================================================================
-- Packet Control (bladeRF)
-- =============================================================================

data PacketControl = PacketControl
  { pkt_sop    :: Bit
  , pkt_eop    :: Bit
  , data_valid :: Bit
  , pktData    :: BitVector 32
  } deriving (Show, Eq, Generic, NFDataX)

nullPacketControl :: PacketControl
nullPacketControl = PacketControl 0 0 0 0

-- =============================================================================
-- Body Parser Sub-FSM Phase
-- =============================================================================
--
-- Shared across all message parsers that do progressive body parsing.
-- Each message parser interprets these phases in its own context.

data BodyParsePhase
  = BP_HEADER          -- ^ Reading body header dwords (SFN/slot, nPDUs etc.)
  | BP_SKIP_FIXED      -- ^ Skipping fixed-size fields
  | BP_PDU_HEADER      -- ^ Reading [pduType | pduSize] or [pduLength]
  | BP_PDU_BODY        -- ^ Reading PDU body dwords
  | BP_TLV_HEADER      -- ^ TX_DATA: reading TLV tag + length
  | BP_TLV_DATA        -- ^ TX_DATA: reading TLV value (TB payload)
  | BP_DONE            -- ^ All PDUs parsed, absorbing until EOP
  deriving (Show, Eq, Generic, NFDataX)

-- =============================================================================
-- NR CRC Type Selector (3GPP TS 38.212)
-- =============================================================================
--
-- Selects which CRC polynomial to use for TB integrity checking.
-- CRC-24A: TB size > 3824 bits  (TS 38.212 Sec 7.2.1)
-- CRC-24B: Code-block CRC       (TS 38.212 Sec 5.1)
-- CRC-16:  TB size <= 3824 bits (TS 38.212 Sec 7.2.1)

data NrCrcType
  = NR_CRC24A
  | NR_CRC24B
  | NR_CRC16
  | NR_CRC_NONE
  deriving (Show, Eq, Generic, NFDataX)

-- | CRC accumulator state for inline NR TB CRC computation.
--   Holds the shift registers for all three NR CRC variants and
--   the selector indicating which polynomial is active.
--   Owned by NewRadioCRC; embedded in parser state records.
--
--   Registers are stored as BitVectors (not Vec n Bit) to match
--   the StreamingCRC parallel step interface directly, avoiding
--   pack/unpack conversions on every CRC update cycle.
data NrCrcState = NrCrcState
  { ncCrcType   :: NrCrcType      -- ^ Which CRC variant is active
  , ncCrc24AReg :: BitVector 24   -- ^ CRC-24A accumulator
  , ncCrc24BReg :: BitVector 24   -- ^ CRC-24B accumulator
  , ncCrc16Reg  :: BitVector 16   -- ^ CRC-16  accumulator
  } deriving (Show, Eq, Generic, NFDataX)

nullNrCrcState :: NrCrcState
nullNrCrcState = NrCrcState
  { ncCrcType   = NR_CRC_NONE
  , ncCrc24AReg = 0
  , ncCrc24BReg = 0
  , ncCrc16Reg  = 0
  }

-- =============================================================================
-- DL_TTI Per-message Parser State
-- =============================================================================
--
-- Tracks position within a DL_TTI.request body during progressive
-- dword-by-dword ingestion.

data DlTtiParseState = DlTtiParseState
  { dpPhase       :: BodyParsePhase
  , dpBodyDwIdx   :: Unsigned 16   -- ^ Current body dword index
  , dpInfo        :: DlTtiInfo     -- ^ Accumulated scheduling info
  , dpCurPduIdx   :: Unsigned 16   -- ^ Current PDU index in the nPDUs loop
  , dpCurPduType  :: BitVector 16  -- ^ pdu-Type of current PDU
  , dpCurPduSizeDw :: Unsigned 16  -- ^ Body dwords in current PDU
  , dpCurPduDwRead :: Unsigned 16  -- ^ Dwords read within current PDU body
  , dpSkipRemain  :: Unsigned 16   -- ^ Dwords remaining to skip
  } deriving (Show, Generic, NFDataX)

deriving instance Eq DlTtiParseState

nullDlTtiParseState :: DlTtiParseState
nullDlTtiParseState = DlTtiParseState
  { dpPhase        = BP_HEADER
  , dpBodyDwIdx    = 0
  , dpInfo         = nullDlTtiInfo
  , dpCurPduIdx    = 0
  , dpCurPduType   = 0xFFFF
  , dpCurPduSizeDw = 0
  , dpCurPduDwRead = 0
  , dpSkipRemain   = 0
  }

-- =============================================================================
-- TX_DATA Per-message Parser State
-- =============================================================================

data TxDataParseState = TxDataParseState
  { tpPhase        :: BodyParsePhase
  , tpBodyDwIdx    :: Unsigned 16
  , tpInfo         :: TxDataInfo
  , tpCurPdu       :: Unsigned 16   -- ^ Current PDU being parsed
  , tpCurPduDwRead :: Unsigned 16   -- ^ Dwords read within current PDU body
  , tpPduRemainDw  :: Unsigned 16   -- ^ Dwords remaining in current PDU
  , tpTlvTag       :: BitVector 16  -- ^ Current TLV tag
  , tpTlvLenDw     :: Unsigned 16   -- ^ TLV value length in dwords
  , tpTlvDwRead    :: Unsigned 16   -- ^ TLV value dwords read so far
  -- TB capture
  , tpTbBuffer     :: TbBuffer
  , tpTbWriteIdx   :: Unsigned 16   -- ^ Next write position
  -- CRC accumulator for inline TB integrity checking
  , tpCrcState     :: NrCrcState    -- ^ Inline TB CRC accumulator
  } deriving (Show, Eq, Generic, NFDataX)

nullTxDataParseState :: TxDataParseState
nullTxDataParseState = TxDataParseState
  { tpPhase        = BP_HEADER
  , tpBodyDwIdx    = 0
  , tpInfo         = nullTxDataInfo
  , tpCurPdu       = 0
  , tpCurPduDwRead = 0
  , tpPduRemainDw  = 0
  , tpTlvTag       = 0
  , tpTlvLenDw     = 0
  , tpTlvDwRead    = 0
  , tpTbBuffer     = nullTbBuffer
  , tpTbWriteIdx   = 0
  , tpCrcState     = nullNrCrcState
  }

-- =============================================================================
-- DL PDU Types (SCF-222 Table 3.4.2-1)
-- =============================================================================

data DlPduType
  = DL_PDU_PDCCH     -- 0
  | DL_PDU_PDSCH     -- 1
  | DL_PDU_CSI_RS    -- 2
  | DL_PDU_SSB       -- 3
  | DL_PDU_OCNG      -- 4
  | DL_PDU_PRS       -- 5
  | DL_PDU_RIM_RS    -- 6
  | DL_PDU_RB_AGG    -- 7
  | DL_PDU_UNKNOWN
  deriving (Show, Eq, Generic, NFDataX)

dlPduTypeFromId :: BitVector 16 -> DlPduType
dlPduTypeFromId 0 = DL_PDU_PDCCH
dlPduTypeFromId 1 = DL_PDU_PDSCH
dlPduTypeFromId 2 = DL_PDU_CSI_RS
dlPduTypeFromId 3 = DL_PDU_SSB
dlPduTypeFromId 4 = DL_PDU_OCNG
dlPduTypeFromId 5 = DL_PDU_PRS
dlPduTypeFromId 6 = DL_PDU_RIM_RS
dlPduTypeFromId 7 = DL_PDU_RB_AGG
dlPduTypeFromId _ = DL_PDU_UNKNOWN

-- =============================================================================
-- Constants
-- =============================================================================

maxPdschPerSlot :: Unsigned 16
maxPdschPerSlot = 4

maxPdcchPerSlot :: Unsigned 16
maxPdcchPerSlot = 4

maxSsbPerSlot :: Unsigned 16
maxSsbPerSlot = 2

maxCsiRsPerSlot :: Unsigned 16
maxCsiRsPerSlot = 4

maxTbDwords :: Unsigned 16
maxTbDwords = 256

maxCdcWords :: Unsigned 16
maxCdcWords = 16

-- =============================================================================
-- DL_TTI Scheduling Info
-- =============================================================================

data DlTtiInfo = DlTtiInfo
  { dtSfn        :: BitVector 16
  , dtSlot       :: BitVector 16
  , dtNumPdus    :: BitVector 16
  -- PDSCH
  , dtNumPdsch   :: Unsigned 16
  , dtPdsch      :: Vec 4 PdschInfo
  -- PDCCH
  , dtNumPdcch   :: Unsigned 16
  , dtPdcch      :: Vec 4 PdcchInfo
  -- SSB
  , dtNumSsb     :: Unsigned 16
  , dtSsb        :: Vec 2 SsbInfo
  -- CSI-RS
  , dtNumCsiRs   :: Unsigned 16
  , dtCsiRs      :: Vec 4 CsiRsInfo
  , dtValid      :: Bit
  } deriving (Show, Eq, Generic, NFDataX)

nullDlTtiInfo :: DlTtiInfo
nullDlTtiInfo = DlTtiInfo
  { dtSfn      = 0
  , dtSlot     = 0
  , dtNumPdus  = 0
  , dtNumPdsch = 0
  , dtPdsch    = repeat nullPdschInfo
  , dtNumPdcch = 0
  , dtPdcch    = repeat nullPdcchInfo
  , dtNumSsb   = 0
  , dtSsb      = repeat nullSsbInfo
  , dtNumCsiRs = 0
  , dtCsiRs    = repeat nullCsiRsInfo
  , dtValid    = 0
  }

-- -----------------------------------------------------------------------------
-- PDSCH (SCF-222 Table 3.4.2.2-1, simplified for PoC)
-- -----------------------------------------------------------------------------

data PdschInfo = PdschInfo
  { piValid       :: Bit
  , piPduIndex    :: BitVector 16
  , piRnti        :: BitVector 16
  , piTbSizeBytes :: BitVector 32
  , piBwpSize     :: BitVector 16
  , piBwpStart    :: BitVector 16
  } deriving (Show, Eq, Generic, NFDataX)

nullPdschInfo :: PdschInfo
nullPdschInfo = PdschInfo 0 0 0 0 0 0

-- -----------------------------------------------------------------------------
-- PDCCH (SCF-222 Table 3.4.2.3-1, simplified for PoC)
-- -----------------------------------------------------------------------------

data PdcchInfo = PdcchInfo
  { pcValid      :: Bit
  , pcPduIndex   :: BitVector 16
  , pcRnti       :: BitVector 16
  , pcBwpSize    :: BitVector 16
  , pcBwpStart   :: BitVector 16
  , pcAggLevel   :: BitVector 8
  , pcCceIndex   :: BitVector 8
  } deriving (Show, Eq, Generic, NFDataX)

nullPdcchInfo :: PdcchInfo
nullPdcchInfo = PdcchInfo 0 0 0 0 0 0 0

-- -----------------------------------------------------------------------------
-- SSB (SCF-222 Table 3.4.2.5-1, simplified for PoC)
-- -----------------------------------------------------------------------------

data SsbInfo = SsbInfo
  { sbValid               :: Bit
  , sbPhysCellId          :: BitVector 16
  , sbBetaPss             :: BitVector 8
  , sbSsbBlockIdx         :: BitVector 8
  , sbSsbSubcarrierOffset :: BitVector 8
  , sbSsbOffsetPointA     :: BitVector 16
  , sbBchPayload          :: BitVector 32
  } deriving (Show, Eq, Generic, NFDataX)

nullSsbInfo :: SsbInfo
nullSsbInfo = SsbInfo 0 0 0 0 0 0 0

-- -----------------------------------------------------------------------------
-- CSI-RS (SCF-222 Table 3.4.2.4-1, simplified for PoC)
-- -----------------------------------------------------------------------------

data CsiRsInfo = CsiRsInfo
  { crValid        :: Bit
  , crPduIndex     :: BitVector 16
  , crBwpSize      :: BitVector 16
  , crBwpStart     :: BitVector 16
  , crStartRb      :: BitVector 16
  , crNrb          :: BitVector 16
  , crScramblingId :: BitVector 16
  } deriving (Show, Eq, Generic, NFDataX)

nullCsiRsInfo :: CsiRsInfo
nullCsiRsInfo = CsiRsInfo 0 0 0 0 0 0 0

-- =============================================================================
-- TX_DATA Types
-- =============================================================================

data TxDataInfo = TxDataInfo
  { tdSfn       :: BitVector 16
  , tdSlot      :: BitVector 16
  , tdNumPdus   :: BitVector 16
  , tdPdus      :: Vec 4 TxDataPdu
  , tdValid     :: Bit
  } deriving (Show, Eq, Generic, NFDataX)

nullTxDataInfo :: TxDataInfo
nullTxDataInfo = TxDataInfo 0 0 0 (repeat nullTxDataPdu) 0

data TxDataPdu = TxDataPdu
  { txpValid      :: Bit
  , txpPduIndex   :: BitVector 16
  , txpCwIndex    :: BitVector 8
  , txpTbLenBytes :: BitVector 32
  , txpTbOffset   :: Unsigned 16
  , txpCrc        :: BitVector 32  -- ^ Computed TB CRC (zero-padded to 32 bits)
  } deriving (Show, Eq, Generic, NFDataX)

nullTxDataPdu :: TxDataPdu
nullTxDataPdu = TxDataPdu 0 0 0 0 0 0

-- =============================================================================
-- Transport Block Buffer
-- =============================================================================

data TbBuffer = TbBuffer
  { tbData       :: Vec 256 (BitVector 32)
  , tbLenDwords  :: Unsigned 16
  , tbPduIndex   :: BitVector 16
  , tbSfn        :: BitVector 16
  , tbSlot       :: BitVector 16
  , tbCrc        :: BitVector 32  -- ^ Computed CRC (zero-padded to 32 bits)
  , tbReady      :: Bit
  , tbConsumed   :: Bit
  } deriving (Show, Eq, Generic, NFDataX)

nullTbBuffer :: TbBuffer
nullTbBuffer = TbBuffer (repeat 0) 0 0 0 0 0 0 0

-- =============================================================================
-- Utilities
-- =============================================================================

bodyLenToDwords :: BitVector 16 -> Unsigned 16
bodyLenToDwords blen =
  let b = unpack blen :: Unsigned 16
  in (b + 3) `div` 4

maxBodyDwords :: Unsigned 16
maxBodyDwords = 8
