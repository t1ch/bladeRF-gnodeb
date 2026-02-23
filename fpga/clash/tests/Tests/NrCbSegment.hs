{-# LANGUAGE DataKinds #-}

-- | Unit tests for NrCbSegment.computeCbParams and the inline CB CRC-24B
--   path in FapiMsgTxData.parseTxDataDword.

module Tests.NrCbSegment (cbSegTests) where

import Prelude
import Test.Tasty
import Test.Tasty.HUnit

import Clash.Prelude (BitVector)
import GNodeBFAPITypes
import NrCbSegment (computeCbParams)
import FapiMsgTxData (parseTxDataDword)

-- =============================================================================
-- computeCbParams tests
-- =============================================================================

-- Small TB (100 bytes): BG2, C=1, kDw=26
test_cbParams_small :: TestTree
test_cbParams_small = testCase "computeCbParams 100 → (BG2,1,26)" $ do
  let (bg, c, kDw) = computeCbParams 100
  bg   @?= BG2
  c    @?= 1
  kDw  @?= 26

-- Large TB (1100 bytes): BG1, C=2, kDw=138
test_cbParams_large :: TestTree
test_cbParams_large = testCase "computeCbParams 1100 → (BG1,2,138)" $ do
  let (bg, c, kDw) = computeCbParams 1100
  bg   @?= BG1
  c    @?= 2
  kDw  @?= 138

-- Boundary: exactly 478 bytes → CRC-16 / BG2
test_cbParams_boundary478 :: TestTree
test_cbParams_boundary478 = testCase "computeCbParams 478 → BG2" $ do
  let (bg, _, _) = computeCbParams 478
  bg @?= BG2

-- Boundary: 479 bytes → CRC-24A / BG1
test_cbParams_boundary479 :: TestTree
test_cbParams_boundary479 = testCase "computeCbParams 479 → BG1" $ do
  let (bg, _, _) = computeCbParams 479
  bg @?= BG1

-- =============================================================================
-- parseTxDataDword integration tests
-- =============================================================================

-- Build a minimal TX_DATA.request dword sequence for a single PDU.
-- The TB has tbBytes payload bytes (0-padded to dwords).
-- Returns the final TxDataParseState after all dwords are consumed.
runTxDataParser :: Int -> TxDataParseState
runTxDataParser tbBytes =
  let tbLenDw  = (tbBytes + 3) `div` 4
      payloadDw = replicate tbLenDw (0xDEADBEEF :: BitVector 32)

      -- TX_DATA body dword sequence:
      --  DW0: SFN=1, Slot=2
      --  DW1: controlLength=0, nPDUs=1
      --  DW2: pduLength = (3 + tbLenDw * 4) rounded (just use big value)
      --  DW3: pduIndex=0, cwIndex=0, pad=0
      --  DW4: numTLV=1
      --  DW5: tag=0, pad=0
      --  DW6: tbLenBytes (TLV length)
      --  DW7..DW(7+tbLenDw-1): payload
      pduLen :: BitVector 32
      pduLen = fromIntegral (3 * 4 + tbLenDw * 4)   -- rough pduLength

      dwords :: [BitVector 32]
      dwords = [ 0x00010002                 -- SFN=1, Slot=2
               , 0x00000001                 -- controlLen=0, nPDUs=1
               , pduLen                     -- pduLength
               , 0x00000000                 -- pduIndex=0, cwIndex=0
               , 0x00000001                 -- numTLV=1
               , 0x00000000                 -- tag=0, pad=0
               , fromIntegral tbBytes       -- TLV length = TB size in bytes
               ] ++ payloadDw

      go st [] _       = st
      go st (d:ds) idx = go (parseTxDataDword st d idx) ds (idx + 1)

  in go nullTxDataParseState dwords 0

-- Small TB: C=1, no CB CRC
test_parseTxData_small :: TestTree
test_parseTxData_small = testCase "small TB: C=1, no CB CRC" $ do
  let st = runTxDataParser 100
      tb = tpTbBuffer st
  tpCbNumCbs st  @?= 1
  -- 25 payload dwords + 1 CRC-16 dword = 26
  tbLenDwords tb @?= 26

-- 476-byte TB: BG2, C=2, kDw=60.
-- Expected TB buffer layout:
--   [0..59]   CB0 payload (60 dwords)
--   [60]      CB0 CRC-24B
--   [61..119] CB1 payload (59 dwords; 476 bytes = 119 total payload dwords)
--   [120]     CB1 CRC-24B
--   [121]     TB CRC-16 (476 < 479 bytes threshold)
--   tbLenDwords = 122
test_parseTxData_cbCrcInTbBuf :: TestTree
test_parseTxData_cbCrcInTbBuf = testCase "476-byte TB: CB CRC-24B interleaved in TB buffer" $ do
  let st = runTxDataParser 476
      tb = tpTbBuffer st
  tpCbNumCbs st  @?= 2
  tbLenDwords tb @?= 122

-- Large TB: C=2
test_parseTxData_large :: TestTree
test_parseTxData_large = testCase "large TB: C=2" $ do
  let st = runTxDataParser 1100
  tpCbNumCbs st @?= 2

-- =============================================================================
-- Test group
-- =============================================================================

cbSegTests :: TestTree
cbSegTests = testGroup "NrCbSegment"
  [ testGroup "computeCbParams"
      [ test_cbParams_small
      , test_cbParams_large
      , test_cbParams_boundary478
      , test_cbParams_boundary479
      ]
  , testGroup "parseTxDataDword (CBS integration)"
      [ test_parseTxData_small
      , test_parseTxData_cbCrcInTbBuf
      , test_parseTxData_large
      ]
  ]
