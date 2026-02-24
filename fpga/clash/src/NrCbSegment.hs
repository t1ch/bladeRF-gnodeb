{-# LANGUAGE DataKinds #-}

-- | Code Block Segmentation parameters for 5G NR (3GPP TS 38.212 Sec 5.2.2).
--
--   Provides a pure combinational function that computes CBS parameters
--   from the transport block size.  Called once in the BP_TLV_HEADER
--   state of FapiMsgTxData — no pipeline state required.
--
--   Algorithm (Z_c-aligned, bit-level accuracy):
--
--     B_bits  = tbLenBytes * 8
--     BG      = BG1 if B_bits > 3824 else BG2
--     L_tb    = BG1 → 24 (CRC-24A)   BG2 → 16 (CRC-16)
--     B'      = B_bits + L_tb         (3GPP TS 38.212 §5.2.2)
--     K_cb    = BG1 → 8448           BG2 → 3840           [bits]
--     L_cb    = 24                                          [CRC-24B bits]
--     C       = ceil(B' / (K_cb - L_cb))  if B' > K_cb, else 1
--     kTarget = ceil(B' / C)              [min LDPC block size needed]
--     Z_c     = min valid lifting size s.t. mult*Z_c >= kTarget  (Table 5.3.2-1)
--     K       = mult * Z_c             [LDPC block size, Z_c-aligned]
--     F       = K * C - B'            [filler bits in CB 0]
--     K-F     = K - F                 [payload bits for CB 0]
--     kDwCb0  = ceil((K-F) / 32)     [CB 0 boundary in dwords]
--     kDwCb1  = ceil(K / 32)         [CB 1..C-1 boundary in dwords]

module NrCbSegment
  ( computeCbParams
  ) where

import Clash.Prelude
import GNodeBFAPITypes (CbBaseGraph(..))

-- | Valid lifting-size values from 3GPP TS 38.212 Table 5.3.2-1, ascending.
validZcValues :: Vec 51 (Unsigned 10)
validZcValues
  = 2 :> 3 :> 4 :> 5 :> 6 :> 7 :> 8 :> 9 :> 10 :> 11
  :> 12 :> 13 :> 14 :> 15 :> 16 :> 18 :> 20 :> 22 :> 24 :> 26
  :> 28 :> 30 :> 32 :> 36 :> 40 :> 44 :> 48 :> 52 :> 56 :> 60
  :> 64 :> 72 :> 80 :> 88 :> 96 :> 104 :> 112 :> 120 :> 128 :> 144
  :> 160 :> 176 :> 192 :> 208 :> 224 :> 240 :> 256 :> 288 :> 320 :> 352
  :> 384 :> Nil

-- | Find the minimum valid Z_c such that mult*Z_c >= kTarget.
--   Synthesises to a 51-stage combinational MUX chain.
lookupZc :: CbBaseGraph -> Unsigned 32 -> Unsigned 10
lookupZc bg kTarget =
  let mult = if bg == BG1 then 22 else 10 :: Unsigned 32
      step acc zc = if resize zc * mult >= kTarget && zc < acc
                      then zc
                      else acc
  in foldl step 384 validZcValues

-- | Compute CBS parameters from TB size in bytes.
--   Called once in BP_TLV_HEADER.  Pure combinational.
--   Returns (baseGraph, C, kDwCb0, splitCb0, kDwCb1, splitCb1, kBits, fillerBits).
computeCbParams :: BitVector 32
  -> ( CbBaseGraph  -- bg
     , Unsigned 8   -- C (number of code blocks)
     , Unsigned 16  -- kDwCb0   = ceil((K-F)/32)  — CB 0 boundary in dwords
     , Unsigned 6   -- splitCb0 = (K-F) mod 32
     , Unsigned 16  -- kDwCb1   = ceil(K/32)       — CB 1..C-1 boundary in dwords
     , Unsigned 6   -- splitCb1 = K mod 32
     , Unsigned 16  -- kBits    = K (Z_c-aligned LDPC block size)
     , Unsigned 16  -- fillerBits = F = K*C - B'
     )
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

      -- kTarget = ceil(B'/C): minimum per-CB LDPC block size needed
      kTarget  = (bPrime + c32 - 1) `div` c32 :: Unsigned 32

      -- Z_c: smallest valid lifting size satisfying mult*Z_c >= kTarget
      zc       = lookupZc bg kTarget
      mult     = if bg == BG1 then 22 else 10 :: Unsigned 32

      -- K: Z_c-aligned LDPC block size
      kFull    = resize zc * mult :: Unsigned 32

      -- F: filler bits in CB 0 (F = K*C - B')
      filler   = kFull * c32 - bPrime :: Unsigned 32

      -- K-F: actual payload bits drawn from b for CB 0
      kData    = kFull - filler :: Unsigned 32

      ceil32 x = (x + 31) `shiftR` 5

      kDwCb0   = truncateB (ceil32 kData) :: Unsigned 16
      splitCb0 = truncateB (kData .&. 31) :: Unsigned 6
      kDwCb1   = truncateB (ceil32 kFull) :: Unsigned 16
      splitCb1 = truncateB (kFull .&. 31) :: Unsigned 6
      kBits    = truncateB kFull          :: Unsigned 16
      fillerB  = truncateB filler         :: Unsigned 16

  in (bg, c, kDwCb0, splitCb0, kDwCb1, splitCb1, kBits, fillerB)
