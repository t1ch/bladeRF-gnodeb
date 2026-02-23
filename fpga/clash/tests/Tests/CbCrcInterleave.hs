{-# LANGUAGE DataKinds #-}

-- | Tests for CB CRC-24B interleaving in the flat TB buffer.
--
--   Verifies that parseTxDataDword correctly inserts CB CRC-24B dwords into
--   the TbBuffer at CB boundaries, writes the TB CRC at the end, and leaves
--   payload slots untouched.
--
--   Test cases:
--     • C=1 (100-byte TB): no CB CRC dwords inserted; TB CRC-16 at slot 25.
--     • C=1 (476-byte TB): after Kcb threshold fix; TB CRC-16 at slot 119.
--     • Seeded C=2 regression: synthetic state fed 6 dwords; verifies that
--       the last CB's CRC-24B folds in the TB CRC dword per spec §5.2.2.

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

-- | CRC-24A over a list of 32-bit dwords.
crc24A :: [BitVector 32] -> BitVector 32
crc24A dws =
  let st0 = nullNrCrcState { ncCrcType = NR_CRC24A }
  in finalizeNrCrc (foldl updateNrCrc st0 dws)

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
-- C=1 (476-byte TB) — after Kcb threshold fix
-- =============================================================================
--
-- 476 bytes = 119 payload dwords, tbTotDw = 120.
-- BG2 (B_bits = 3808 ≤ 3824), Kcb_BG2 = 120 dwords.
-- 120 > 120 is false → C = 1, kDw = 120.
-- TB CRC type: CRC-16 (476 ≤ 478 bytes threshold).
--
-- TB buffer layout:
--   [0  .. 118]  payload (119 dwords of 0xDEADBEEF)
--   [119]        TB CRC-16
--   tbLenDwords = 120

test_476_c1_tbLenDwords :: TestTree
test_476_c1_tbLenDwords =
  testCase "476-byte C=1: tbLenDwords = 120" $
    tbLenDwords (tpTbBuffer (runParser 476)) @?= 120

test_476_c1_payload_preserved :: TestTree
test_476_c1_payload_preserved =
  testCase "476-byte C=1: payload dwords written verbatim" $ do
    let buf = toList (tbData (tpTbBuffer (runParser 476)))
    buf !! 0   @?= 0xDEADBEEF   -- first payload slot
    buf !! 118 @?= 0xDEADBEEF   -- last payload slot

test_476_c1_tbCrc_at_slot119 :: TestTree
test_476_c1_tbCrc_at_slot119 =
  testCase "476-byte C=1: TB CRC-16 at slot 119" $ do
    let buf = toList (tbData (tpTbBuffer (runParser 476)))
    buf !! 119 @?= crc16 (replicate 119 0xDEADBEEF)

-- =============================================================================
-- Seeded C=2 regression — Fix 2: last CB CRC-24B folds in TB CRC dword
-- =============================================================================
--
-- Synthetic TxDataParseState seeded directly into BP_TLV_DATA with:
--   tpCbNumCbs = 2, tpCbPayDw = 3, tpTlvLenDw = 6
--   TB CRC type: CRC-24A (tpCrcState)
--
-- Feed 6 dwords of 0xDEADBEEF.  Expected buffer layout:
--
--   [0,1,2]  CB0 payload   (0xDEADBEEF × 3)
--   [3]      CB0 CRC-24B   = crc24B [0xDEADBEEF × 3]
--   [4,5,6]  CB1 payload   (0xDEADBEEF × 3)
--   [7]      TB  CRC-24A   = crc24A [0xDEADBEEF × 6]
--   [8]      CB1 CRC-24B   = crc24B ([0xDEADBEEF × 3] ++ [tbCrcDword])
--   tbLenDwords = 9
--
-- This directly verifies that the last CB's CRC-24B covers the TB CRC dword.

seedState :: TxDataParseState
seedState = nullTxDataParseState
  { tpPhase        = BP_TLV_DATA
  , tpTlvLenDw     = 6
  , tpTlvDwRead    = 0
  , tpCbNumCbs     = 2
  , tpCbPayDw      = 3
  , tpCbDwInBlock  = 0
  , tpCrcState     = nullNrCrcState { ncCrcType = NR_CRC24A }
  , tpCbCrcState   = nullNrCrcState { ncCrcType = NR_CRC24B }
  , tpInfo         = nullTxDataInfo { tdNumPdus = 1 }
  , tpPduRemainDw  = 6
  }

runSeededParser :: TxDataParseState
runSeededParser =
  foldl (\st dw -> parseTxDataDword st dw 0)
        seedState
        (replicate 6 (0xDEADBEEF :: BitVector 32))

expectedCb0CrcSeeded :: BitVector 32
expectedCb0CrcSeeded = crc24B (replicate 3 0xDEADBEEF)

expectedTbCrcSeeded :: BitVector 32
expectedTbCrcSeeded = crc24A (replicate 6 0xDEADBEEF)

expectedCb1CrcSeeded :: BitVector 32
expectedCb1CrcSeeded = crc24B (replicate 3 0xDEADBEEF ++ [expectedTbCrcSeeded])

test_seeded_c2_tbLenDwords :: TestTree
test_seeded_c2_tbLenDwords =
  testCase "seeded C=2: tbLenDwords = 9" $
    tbLenDwords (tpTbBuffer runSeededParser) @?= 9

test_seeded_c2_cb0_crc_at_slot3 :: TestTree
test_seeded_c2_cb0_crc_at_slot3 =
  testCase "seeded C=2: CB0 CRC-24B at slot 3" $ do
    let buf = toList (tbData (tpTbBuffer runSeededParser))
    buf !! 3 @?= expectedCb0CrcSeeded

test_seeded_c2_cb1_payload_slots :: TestTree
test_seeded_c2_cb1_payload_slots =
  testCase "seeded C=2: CB1 payload slots 4-6 intact" $ do
    let buf = toList (tbData (tpTbBuffer runSeededParser))
    buf !! 4 @?= 0xDEADBEEF
    buf !! 5 @?= 0xDEADBEEF
    buf !! 6 @?= 0xDEADBEEF

test_seeded_c2_tb_crc_at_slot7 :: TestTree
test_seeded_c2_tb_crc_at_slot7 =
  testCase "seeded C=2: TB CRC-24A at slot 7" $ do
    let buf = toList (tbData (tpTbBuffer runSeededParser))
    buf !! 7 @?= expectedTbCrcSeeded

test_seeded_c2_cb1_crc_at_slot8 :: TestTree
test_seeded_c2_cb1_crc_at_slot8 =
  testCase "seeded C=2: CB1 CRC-24B at slot 8 (includes TB CRC dword)" $ do
    let buf = toList (tbData (tpTbBuffer runSeededParser))
    buf !! 8 @?= expectedCb1CrcSeeded

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
  , testGroup "C=1: no interleaving (476-byte TB, Kcb fix)"
      [ test_476_c1_tbLenDwords
      , test_476_c1_payload_preserved
      , test_476_c1_tbCrc_at_slot119
      ]
  , testGroup "Seeded C=2: last CB CRC covers TB CRC dword (Fix 2 regression)"
      [ test_seeded_c2_tbLenDwords
      , test_seeded_c2_cb0_crc_at_slot3
      , test_seeded_c2_cb1_payload_slots
      , test_seeded_c2_tb_crc_at_slot7
      , test_seeded_c2_cb1_crc_at_slot8
      ]
  ]
