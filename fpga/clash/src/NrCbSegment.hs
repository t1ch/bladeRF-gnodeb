{-# LANGUAGE DataKinds #-}

-- | Code Block Segmentation parameters for 5G NR (3GPP TS 38.212 Sec 5.2.2).
--
--   Provides a pure combinational function that computes CBS parameters
--   from the transport block size.  Called once in the BP_TLV_HEADER
--   state of FapiMsgTxData — no pipeline state required.
--
--   Algorithm (bit-level accuracy):
--
--     B_bits  = tbLenBytes * 8
--     BG      = BG1 if B_bits > 3824 else BG2
--     L_tb    = BG1 → 24 (CRC-24A)   BG2 → 16 (CRC-16)
--     B'      = B_bits + L_tb         (3GPP TS 38.212 §5.2.2)
--     K_cb    = BG1 → 8448           BG2 → 3840           [bits]
--     L_cb    = 24                                          [CRC-24B bits]
--     C       = ceil(B' / (K_cb - L_cb))  if B' > K_cb, else 1
--     kBits   = ceil(B' / C)              [per-CB segment bits from b]
--     kDw     = ceil(kBits / 32)          [boundary dword detection]
--     splitBit = kBits mod 32             [0 = boundary at dword edge]

module NrCbSegment
  ( computeCbParams
  ) where

import Clash.Prelude
import GNodeBFAPITypes (CbBaseGraph(..))

-- | Compute CBS parameters from TB size in bytes.
--   Called once in BP_TLV_HEADER.  Pure combinational.
--   Returns (baseGraph, C, kDw, kBits, splitBit).
computeCbParams :: BitVector 32 -> (CbBaseGraph, Unsigned 8, Unsigned 16, Unsigned 16, Unsigned 6)
computeCbParams tbLenBytes =
  let tbBytes  = unpack tbLenBytes :: Unsigned 32
      bBits    = tbBytes `shiftL` 3
      bg       = if bBits > 3824 then BG1 else BG2
      lTb      = if bBits > 3824 then 24 else 16 :: Unsigned 32
      bPrime   = bBits + lTb                                      -- B' (bits)
      kcbBits  = if bg == BG1 then 8448 else 3840 :: Unsigned 32  -- K_cb (bits)
      lCb      = 24 :: Unsigned 32                                 -- CRC-24B

      -- C = ceil(B' / (K_cb - L_cb)) when B' > K_cb, else 1
      denom    = kcbBits - lCb
      c32      = if bPrime > kcbBits
                   then (bPrime + denom - 1) `div` denom
                   else 1 :: Unsigned 32
      c        = truncateB c32 :: Unsigned 8

      -- Per-CB segment of b: kBits = ceil(B'/C)
      -- (data bits from b assigned to each CB, before per-CB CRC)
      kBits32  = (bPrime + c32 - 1) `div` c32 :: Unsigned 32
      kBits    = truncateB kBits32 :: Unsigned 16

      -- Dword count per CB segment (for boundary detection)
      kDw      = truncateB ((kBits32 + 31) `shiftR` 5) :: Unsigned 16

      -- Intra-dword split offset: 0 means boundary at dword edge (no split)
      splitBit = truncateB (kBits32 .&. 31) :: Unsigned 6

  in (bg, c, kDw, kBits, splitBit)
