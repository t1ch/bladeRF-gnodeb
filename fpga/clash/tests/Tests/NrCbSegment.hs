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

-- Small TB (100 bytes): BG2, C=1
-- B=800, B'=816, kBits=816, kDw=ceil(816/32)=26
test_cbParams_small :: TestTree
test_cbParams_small = testCase "computeCbParams 100 → (BG2,1,26,816,16)" $ do
  let (bg, c, kDw, kBits, split) = computeCbParams 100
  bg    @?= BG2
  c     @?= 1
  kDw   @?= 26
  kBits @?= 816
  split @?= 16

-- Large TB (1100 bytes): BG1, C=2
-- B=8800, B'=8824, C=ceil(8824/8424)=2, kBits=ceil(8824/2)=4412
-- kDw=ceil(4412/32)=138, split=4412 mod 32 = 28
test_cbParams_large :: TestTree
test_cbParams_large = testCase "computeCbParams 1100 → (BG1,2,138,4412,28)" $ do
  let (bg, c, kDw, kBits, split) = computeCbParams 1100
  bg    @?= BG1
  c     @?= 2
  kDw   @?= 138
  kBits @?= 4412
  split @?= 28

-- Boundary: exactly 478 bytes → CRC-16 / BG2
test_cbParams_boundary478 :: TestTree
test_cbParams_boundary478 = testCase "computeCbParams 478 → BG2" $ do
  let (bg, _, _, _, _) = computeCbParams 478
  bg @?= BG2

-- Boundary: 479 bytes → CRC-24A / BG1
test_cbParams_boundary479 :: TestTree
test_cbParams_boundary479 = testCase "computeCbParams 479 → BG1" $ do
  let (bg, _, _, _, _) = computeCbParams 479
  bg @?= BG1

-- TB = 2103 bytes: was C=3 (bug), now C=2 (correct)
-- B=16824, B'=16848, K_cb=8448, denom=8424
-- C=ceil(16848/8424)=2
test_cbParams_2103 :: TestTree
test_cbParams_2103 = testCase "computeCbParams 2103 → (BG1,2,_,_,_)" $ do
  let (bg, c, _, _, _) = computeCbParams 2103
  bg @?= BG1
  c  @?= 2

-- TB = 2102 bytes: same boundary fix
-- B=16816, B'=16840, C=ceil(16840/8424)=2
test_cbParams_2102 :: TestTree
test_cbParams_2102 = testCase "computeCbParams 2102 → (BG1,2,_,_,_)" $ do
  let (bg, c, _, _, _) = computeCbParams 2102
  bg @?= BG1
  c  @?= 2

-- Verify kBits and splitBit for a known case
-- TB = 2100 bytes: B=16800, B'=16824, C=2, kBits=ceil(16824/2)=8412
-- 8412 mod 32 = 28 (262×32=8384, 8412-8384=28)
test_cbParams_kBits :: TestTree
test_cbParams_kBits = testCase "computeCbParams 2100: kBits=8412, split=28" $ do
  let (_, _, _, kBits, split) = computeCbParams 2100
  kBits @?= 8412
  split @?= 28

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

-- 476-byte TB: BG2, C=1 after Kcb threshold fix.
-- 476 bytes = 119 payload dwords, B'=3824, Kcb_BG2=3840.
-- B' > K_cb is false → C = 1.
-- Expected TB buffer layout:
--   [0..118]  payload (119 dwords)
--   [119]     TB CRC-16
--   tbLenDwords = 120
test_parseTxData_cbCrcInTbBuf :: TestTree
test_parseTxData_cbCrcInTbBuf = testCase "476-byte TB: C=1 after Kcb threshold fix" $ do
  let st = runTxDataParser 476
      tb = tpTbBuffer st
  tpCbNumCbs st  @?= 1
  tbLenDwords tb @?= 120

-- Large TB: C=2
test_parseTxData_large :: TestTree
test_parseTxData_large = testCase "large TB: C=2" $ do
  let st = runTxDataParser 1100
  tpCbNumCbs st @?= 2

-- TB = 2103 bytes: C=2 (was C=3 bug with dword-granularity)
test_parseTxData_2103 :: TestTree
test_parseTxData_2103 = testCase "2103-byte TB: C=2 (not 3)" $ do
  let st = runTxDataParser 2103
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
      , test_cbParams_2103
      , test_cbParams_2102
      , test_cbParams_kBits
      ]
  , testGroup "parseTxDataDword (CBS integration)"
      [ test_parseTxData_small
      , test_parseTxData_cbCrcInTbBuf
      , test_parseTxData_large
      , test_parseTxData_2103
      ]
  ]
