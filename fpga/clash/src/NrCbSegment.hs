{-# LANGUAGE DataKinds #-}

-- | Code Block Segmentation parameters for 5G NR (3GPP TS 38.212 Sec 5.2.2).
--
--   Provides a pure combinational function that computes CBS parameters
--   from the transport block size.  Called once in the BP_TLV_HEADER
--   state of FapiMsgTxData — no pipeline state required.
--
--   Algorithm (dword-granularity PoC):
--
--     B_bits  = tbLenBytes * 8
--     BG      = BG1 if B_bits > 3824 else BG2
--     Kcb_dw  = BG1 → 264  (floor(8448/32))   BG2 → 120  (floor(3840/32))  [segmentation threshold]
--     bcb_dw  = BG1 → 263  (floor(8424/32))   BG2 → 119  (floor(3816/32))  [= (Kcb-L)/32, C denominator]
--     L       = BG1 → 24 (CRC-24A)             BG2 → 16 (CRC-16)
--     B'      = B_bits + L                      (3GPP TS 38.212 §5.2.2)
--     tbTotDw = ceil(B' / 32)                  -- payload + CRC dwords
--     C       = ceil(tbTotDw / bcb_dw)  if tbTotDw > Kcb_dw, else 1
--     kDw     = ceil(tbTotDw / C)

module NrCbSegment
  ( computeCbParams
  ) where

import Clash.Prelude
import GNodeBFAPITypes (CbBaseGraph(..), bg1BcbDwords, bg2BcbDwords, bg1KcbDwords, bg2KcbDwords)

-- | Compute CBS parameters from TB size in bytes.
--   Called once in BP_TLV_HEADER.  Pure combinational.
--   Returns (baseGraph, C, kDw).
computeCbParams :: BitVector 32 -> (CbBaseGraph, Unsigned 8, Unsigned 16)
computeCbParams tbLenBytes =
  let tbBytes  = unpack tbLenBytes :: Unsigned 32
      bBits    = tbBytes `shiftL` 3
      bg       = if bBits > 3824 then BG1 else BG2
      bcbDw    = if bg == BG1 then bg1BcbDwords else bg2BcbDwords  -- (Kcb-L)/32 for C calc
      kcbDw    = if bg == BG1 then bg1KcbDwords else bg2KcbDwords  -- Kcb/32 for threshold
      -- B' = B_bits + L  (3GPP TS 38.212 §5.2.2)
      crcBits  = if bg == BG1 then 24 else 16 :: Unsigned 32
      bPrime   = bBits + crcBits
      -- Total dwords: ceil(B' / 32)
      tbTotDw  = truncateB ((bPrime + 31) `shiftR` 5) :: Unsigned 16
      -- Number of code blocks C (segmentation threshold is Kcb, not Kcb-L)
      c16 :: Unsigned 16
      c16      = if tbTotDw > resize kcbDw
                   then (tbTotDw + resize bcbDw - 1) `div` resize bcbDw
                   else 1
      c        = truncateB c16 :: Unsigned 8
      -- Payload dwords per CB (ceiling division)
      kDw      = (tbTotDw + c16 - 1) `div` c16
  in (bg, c, kDw)
