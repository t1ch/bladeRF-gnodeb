{-# LANGUAGE DataKinds #-}

-- | Tests for CB CRC-24B interleaving in the flat TB buffer.
--
--   Verifies that parseTxDataDword correctly inserts CB CRC-24B dwords into
--   the TbBuffer at CB boundaries, writes the TB CRC at the end, and leaves
--   payload slots untouched.
--
--   Test cases:
--     • C=1 (100-byte TB): no CB CRC dwords inserted; TB CRC-16 at slot 25.
--     • C=2 BG2 (476-byte TB): exact slot positions for CB0 CRC, CB1 CRC,
--       and TB CRC-16 verified against independently computed values.
--     • Intermediate cbReady signal: fires exactly after the CB0 boundary
--       and is cleared on the first dword of CB1.
--     • Final CbBuffer state: cbIndex, cbLenDwords, cbCrc, and cbData
--       for the last CB checked against expected values.

module Tests.CbCrcInterleave (cbCrcInterleaveTests) where

import Prelude
import Test.Tasty
import Test.Tasty.HUnit

import Clash.Prelude (BitVector, toList)
import GNodeBFAPITypes
import FapiMsgTxData (parseTxDataDword)
import NewRadioCRC (updateNrCrc, finalizeNrCrc)

-- =============================================================================
-- CRC helpers (independent reference implementation)
-- =============================================================================

-- | CRC-24B over a list of 32-bit dwords.
crc24B :: [BitVector 32] -> BitVector 32
crc24B dws =
  let st0 = nullNrCrcState { ncCrcType = NR_CRC24B }
  in finalizeNrCrc (foldl updateNrCrc st0 dws)

-- | CRC-16 over a list of 32-bit dwords.
crc16 :: [BitVector 32] -> BitVector 32
crc16 dws =
  let st0 = nullNrCrcState { ncCrcType = NR_CRC16 }
  in finalizeNrCrc (foldl updateNrCrc st0 dws)

-- =============================================================================
-- Parser driver
-- =============================================================================

-- | Build the TX_DATA.request dword sequence for a single PDU with
--   tbBytes inline payload bytes, all filled with 0xDEADBEEF.
--
--   Layout (mirrors Tests.NrCbSegment.runTxDataParser):
--     [0] SFN=1, Slot=2
--     [1] controlLength=0, nPDUs=1
--     [2] pduLength
--     [3] pduIndex=0, cwIndex=0, pad=0
--     [4] numTLV=1
--     [5] tag=0, pad=0
--     [6] TLV length = tbBytes
--     [7 .. 6+tbLenDw] payload (0xDEADBEEF × tbLenDw)
buildDwords :: Int -> [BitVector 32]
buildDwords tbBytes =
  let tbLenDw   = (tbBytes + 3) `div` 4
      payloadDw = replicate tbLenDw 0xDEADBEEF
      pduLen :: BitVector 32
      pduLen    = fromIntegral (3 * 4 + tbLenDw * 4)
  in [ 0x00010002
     , 0x00000001
     , pduLen
     , 0x00000000
     , 0x00000001
     , 0x00000000
     , fromIntegral tbBytes
     ] ++ payloadDw

-- | Run the parser to completion and return the final state.
runParser :: Int -> TxDataParseState
runParser tbBytes =
  foldl (\st dw -> parseTxDataDword st dw 0)
        nullTxDataParseState
        (buildDwords tbBytes)

-- | Return one state per dword consumed (state *after* that dword).
runParserSteps :: Int -> [TxDataParseState]
runParserSteps tbBytes =
  drop 1 $ scanl (\st dw -> parseTxDataDword st dw 0)
               nullTxDataParseState
               (buildDwords tbBytes)

-- =============================================================================
-- C=1 (100-byte TB, no CB CRC interleaving)
-- =============================================================================
--
-- 100 bytes → 25 payload dwords, tbTotDw = 26, C = 1, kDw = 26.
-- TB buffer layout: [0..24 payload] [25 TB CRC-16]
-- No CB CRC dwords are inserted.

test_c1_payload_preserved :: TestTree
test_c1_payload_preserved =
  testCase "C=1: payload dwords written verbatim" $ do
    let buf = toList (tbData (tpTbBuffer (runParser 100)))
    buf !! 0  @?= 0xDEADBEEF   -- first payload slot
    buf !! 24 @?= 0xDEADBEEF   -- last payload slot

test_c1_tbCrc_at_slot25 :: TestTree
test_c1_tbCrc_at_slot25 =
  testCase "C=1: TB CRC-16 at slot 25" $ do
    let buf = toList (tbData (tpTbBuffer (runParser 100)))
    buf !! 25 @?= crc16 (replicate 25 0xDEADBEEF)

test_c1_tbLenDwords :: TestTree
test_c1_tbLenDwords =
  testCase "C=1: tbLenDwords = 26" $
    tbLenDwords (tpTbBuffer (runParser 100)) @?= 26

-- =============================================================================
-- C=2 BG2 (476-byte TB) — precomputed reference values
-- =============================================================================
--
-- 476 bytes = 119 payload dwords (exact), tbTotDw = 120.
-- BG2 (B_bits = 3808 ≤ 3824), bcbDw = 119.
-- C = ceil(120 / 119) = 2, kDw = ceil(120 / 2) = 60.
-- TB CRC type: CRC-16 (476 ≤ 478 bytes threshold).
--
-- TB buffer layout:
--   [0  .. 59]  CB0 payload   (60 dwords of 0xDEADBEEF)
--   [60]        CB0 CRC-24B
--   [61 .. 119] CB1 payload   (59 dwords of 0xDEADBEEF)
--   [120]       CB1 CRC-24B
--   [121]       TB  CRC-16    (over all 119 payload dwords)
--   tbLenDwords = 122

expectedCb0Crc476 :: BitVector 32
expectedCb0Crc476 = crc24B (replicate 60 0xDEADBEEF)

expectedCb1Crc476 :: BitVector 32
expectedCb1Crc476 = crc24B (replicate 59 0xDEADBEEF)

expectedTbCrc476 :: BitVector 32
expectedTbCrc476 = crc16 (replicate 119 0xDEADBEEF)

test_476_tbLenDwords :: TestTree
test_476_tbLenDwords =
  testCase "476-byte: tbLenDwords = 122" $
    tbLenDwords (tpTbBuffer (runParser 476)) @?= 122

test_476_cb0_payload_boundary :: TestTree
test_476_cb0_payload_boundary =
  testCase "476-byte: CB0 boundary payload slots intact" $ do
    let buf = toList (tbData (tpTbBuffer (runParser 476)))
    buf !! 0  @?= 0xDEADBEEF   -- first slot of CB0
    buf !! 59 @?= 0xDEADBEEF   -- last slot of CB0 (slot 60 is the CRC)

test_476_cb0_crc :: TestTree
test_476_cb0_crc =
  testCase "476-byte: CB0 CRC-24B at slot 60" $ do
    let buf = toList (tbData (tpTbBuffer (runParser 476)))
    buf !! 60 @?= expectedCb0Crc476

test_476_cb1_payload_boundary :: TestTree
test_476_cb1_payload_boundary =
  testCase "476-byte: CB1 boundary payload slots intact" $ do
    let buf = toList (tbData (tpTbBuffer (runParser 476)))
    buf !! 61  @?= 0xDEADBEEF   -- first slot of CB1
    buf !! 119 @?= 0xDEADBEEF   -- last slot of CB1 (slot 120 is the CRC)

test_476_cb1_crc :: TestTree
test_476_cb1_crc =
  testCase "476-byte: CB1 CRC-24B at slot 120" $ do
    let buf = toList (tbData (tpTbBuffer (runParser 476)))
    buf !! 120 @?= expectedCb1Crc476

test_476_tb_crc :: TestTree
test_476_tb_crc =
  testCase "476-byte: TB CRC-16 at slot 121" $ do
    let buf = toList (tbData (tpTbBuffer (runParser 476)))
    buf !! 121 @?= expectedTbCrc476

-- Sanity: the CRC slots must not accidentally contain the payload pattern.
test_476_crc_slots_differ_from_payload :: TestTree
test_476_crc_slots_differ_from_payload =
  testCase "476-byte: CRC slots differ from payload pattern (0xDEADBEEF)" $ do
    let buf = toList (tbData (tpTbBuffer (runParser 476)))
    assertBool "slot 60 (CB0 CRC) should not be 0xDEADBEEF"
               (buf !! 60  /= 0xDEADBEEF)
    assertBool "slot 120 (CB1 CRC) should not be 0xDEADBEEF"
               (buf !! 120 /= 0xDEADBEEF)
    assertBool "slot 121 (TB CRC) should not be 0xDEADBEEF"
               (buf !! 121 /= 0xDEADBEEF)

-- =============================================================================
-- 476-byte TB — intermediate cbReady signal
-- =============================================================================
--
-- Sequence indices for buildDwords 476:
--   [0]       SFN/Slot
--   [1]       nPDUs
--   [2]       pduLength
--   [3]       pduIndex/cwIndex
--   [4]       numTLV
--   [5]       tag
--   [6]       tbLenBytes       ← BP_TLV_HEADER fires here, kDw=60 set
--   [7..125]  payload (119 dwords)
--
-- cbBoundary fires after the 60th payload dword (sequence index 66).
-- At that point tpCbBuffer has cbReady=1, cbIndex=0.
-- On the next dword (index 67) a fresh buffer for CB1 is started:
-- cbReady=0, cbIndex=1.

test_476_intermediate_cbReady :: TestTree
test_476_intermediate_cbReady =
  testCase "476-byte: cbReady=1 fires at step 66 (CB0 boundary)" $ do
    let steps = runParserSteps 476
        cbuf  = tpCbBuffer (steps !! 66)
    cbReady cbuf @?= 1
    cbIndex cbuf @?= 0
    cbTotal cbuf @?= 2

test_476_cb1_buffer_fresh :: TestTree
test_476_cb1_buffer_fresh =
  testCase "476-byte: cbReady=0 and cbIndex=1 at step 67 (CB1 started)" $ do
    let steps = runParserSteps 476
        cbuf  = tpCbBuffer (steps !! 67)
    cbReady cbuf @?= 0
    cbIndex cbuf @?= 1

-- =============================================================================
-- 476-byte TB — final CbBuffer state (last CB = CB1)
-- =============================================================================
--
-- After tlvDone, tpCbBuffer holds the last CB:
--   cbIndex      = 1  (second CB, 0-based)
--   cbTotal      = 2
--   cbLenDwords  = 60  (59 payload + 1 CRC-24B)
--   cbCrcPresent = 1
--   cbCrc        = CRC-24B of 59 dwords of 0xDEADBEEF
--   cbData[0..58]  = 0xDEADBEEF
--   cbData[59]     = CB1 CRC-24B

test_476_lastCb_metadata :: TestTree
test_476_lastCb_metadata =
  testCase "476-byte: last CB metadata (index, total, lenDwords, crcPresent)" $ do
    let cbuf = tpCbBuffer (runParser 476)
    cbIndex      cbuf @?= 1
    cbTotal      cbuf @?= 2
    cbLenDwords  cbuf @?= 60
    cbCrcPresent cbuf @?= 1

test_476_lastCb_crcValue :: TestTree
test_476_lastCb_crcValue =
  testCase "476-byte: last CB cbCrc == CRC-24B over 59 dwords" $
    cbCrc (tpCbBuffer (runParser 476)) @?= expectedCb1Crc476

test_476_lastCb_data :: TestTree
test_476_lastCb_data =
  testCase "476-byte: CB1 cbData payload and CRC dwords correct" $ do
    let cdata = toList (cbData (tpCbBuffer (runParser 476)))
    cdata !! 0  @?= 0xDEADBEEF       -- first payload dword of CB1
    cdata !! 58 @?= 0xDEADBEEF       -- last payload dword of CB1
    cdata !! 59 @?= expectedCb1Crc476 -- CRC-24B appended at position 59

-- =============================================================================
-- Test group
-- =============================================================================

cbCrcInterleaveTests :: TestTree
cbCrcInterleaveTests = testGroup "CB CRC interleaving"
  [ testGroup "C=1: no interleaving (100-byte TB)"
      [ test_c1_payload_preserved
      , test_c1_tbCrc_at_slot25
      , test_c1_tbLenDwords
      ]
  , testGroup "C=2 BG2: TB buffer layout (476-byte TB)"
      [ test_476_tbLenDwords
      , test_476_cb0_payload_boundary
      , test_476_cb0_crc
      , test_476_cb1_payload_boundary
      , test_476_cb1_crc
      , test_476_tb_crc
      , test_476_crc_slots_differ_from_payload
      ]
  , testGroup "C=2 BG2: intermediate cbReady signal (476-byte TB)"
      [ test_476_intermediate_cbReady
      , test_476_cb1_buffer_fresh
      ]
  , testGroup "C=2 BG2: final CbBuffer state (476-byte TB)"
      [ test_476_lastCb_metadata
      , test_476_lastCb_crcValue
      , test_476_lastCb_data
      ]
  ]
