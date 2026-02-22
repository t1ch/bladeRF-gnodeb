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
--     Kcb_dw  = BG1 → 263  (floor(8424/32))   BG2 → 119  (floor(3816/32))
--     tbTotDw = (tbLenBytes + 3) / 4 + 1       -- payload dwords + 1 CRC dword
--     C       = ceil(tbTotDw / Kcb_dw)  if tbTotDw > Kcb_dw, else 1
--     kDw     = ceil(tbTotDw / C)

module NrCbSegment
  ( computeCbParams
  ) where

import Clash.Prelude
import GNodeBFAPITypes (CbBaseGraph(..), bg1BcbDwords, bg2BcbDwords)

-- | Compute CBS parameters from TB size in bytes.
--   Called once in BP_TLV_HEADER.  Pure combinational.
--   Returns (baseGraph, C, kDw).
computeCbParams :: BitVector 32 -> (CbBaseGraph, Unsigned 8, Unsigned 16)
computeCbParams tbLenBytes =
  let tbBytes  = unpack tbLenBytes :: Unsigned 32
      bBits    = tbBytes `shiftL` 3
      bg       = if bBits > 3824 then BG1 else BG2
      bcbDw    = if bg == BG1 then bg1BcbDwords else bg2BcbDwords
      -- Total dwords: TB payload rounded up to dwords, plus 1 TB CRC dword
      tbTotDw  = truncateB ((tbBytes + 3) `div` 4) + 1 :: Unsigned 16
      -- Number of code blocks C
      c16 :: Unsigned 16
      c16      = if tbTotDw > resize bcbDw
                   then (tbTotDw + resize bcbDw - 1) `div` resize bcbDw
                   else 1
      c        = truncateB c16 :: Unsigned 8
      -- Payload dwords per CB (ceiling division)
      kDw      = (tbTotDw + c16 - 1) `div` c16
  in (bg, c, kDw)
