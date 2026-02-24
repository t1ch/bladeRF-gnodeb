{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | Streaming parallel CRC engine for Clash.
--
--   Computes CRC updates by folding the serial CRC step across all bits
--   of a parallel data chunk.  Clash unrolls the fold into a purely
--   combinational XOR tree — no Template Haskell or compile-time matrix
--   precomputation required.
--
--   The key insight (from the StreamingCRC PoC) is that @foldl serialStep@
--   over the bit-vector of a data chunk is semantically identical to the
--   F/G matrix approach, but lets Clash's own normaliser derive the XOR
--   tree directly from the recursive definition.
--
--   This module provides:
--
--     • 'parallelCRCStep' — one-cycle combinational CRC update for an
--       arbitrary polynomial width and data chunk width.
--
--     • 'streamingCRC' — a clocked streaming wrapper with start/valid
--       handshake, suitable for direct synthesis.
--
--   Reference:
--     Ross N. Williams, "A Painless Guide to CRC Error Detection Algorithms"

module StreamingCRC
  ( -- * Combinational CRC step
    parallelCRCStep
    -- * Range-gated CRC step
  , rangeParallelCRCStep
    -- * Clocked streaming component
  , streamingCRC
  ) where

import Clash.Prelude

-- =============================================================================
-- Combinational Parallel CRC Step
-- =============================================================================

-- | Compute one parallel CRC update over a data chunk.
--
--   @parallelCRCStep poly currentCrc dataChunk@
--
--   Folds the standard MSB-first serial CRC step across every bit of
--   @dataChunk@ (MSB processed first).  Clash unrolls this into a
--   single-cycle combinational XOR tree whose depth is O(dataWidth)
--   and whose gate count matches the TH-generated approach.
--
--   The polynomial is specified as a 'BitVector' of the CRC width
--   (excluding the implicit leading 1).  For example, CRC-32 Ethernet:
--
--   @
--     poly = 0x04C11DB7 :: BitVector 32
--   @
--
--   This function is pure combinational — no state, no clock.
parallelCRCStep
  :: forall crcN dataN
   . ( KnownNat crcN
     , KnownNat dataN )
  => BitVector crcN             -- ^ CRC polynomial (without leading 1)
  -> BitVector crcN             -- ^ Current CRC accumulator
  -> BitVector dataN            -- ^ Data chunk to process
  -> BitVector crcN             -- ^ Updated CRC accumulator
parallelCRCStep poly currentCrc dataChunk =
  foldl serialStep currentCrc (bv2v dataChunk)
  where
    serialStep :: BitVector crcN -> Bit -> BitVector crcN
    serialStep acc inBit =
      let fb      = msb acc `xor` inBit
          shifted = shiftL acc 1
      in if fb == high then shifted `xor` poly else shifted

-- =============================================================================
-- Range-Gated Parallel CRC Step
-- =============================================================================

-- | Compute a CRC update over a sub-range of bits within a 32-bit dword.
--
--   @rangeParallelCRCStep poly crc startBit endBit dw@
--
--   Processes only bits @[startBit, endBit)@ of @dw@ through the CRC
--   polynomial (MSB-first, bit 0 = MSB).  Bits outside the range are
--   skipped — the accumulator passes through unchanged.
--
--   Clash unrolls this into 32 stages with per-stage comparator muxes:
--   same combinational depth as a full 32-bit step.
rangeParallelCRCStep
  :: forall crcN
   . KnownNat crcN
  => BitVector crcN       -- ^ CRC polynomial (without leading 1)
  -> BitVector crcN       -- ^ Current CRC accumulator
  -> Unsigned 6           -- ^ startBit (inclusive, 0 = MSB)
  -> Unsigned 6           -- ^ endBit (exclusive, 32 = process through LSB)
  -> BitVector 32         -- ^ Data dword
  -> BitVector crcN
rangeParallelCRCStep poly crc startBit endBit dw =
  foldl gatedStep crc (zip (indicesI :: Vec 32 (Index 32)) (bv2v dw))
  where
    gatedStep :: BitVector crcN -> (Index 32, Bit) -> BitVector crcN
    gatedStep acc (idx, inBit) =
      let idx6 = bitCoerce idx :: Unsigned 5
          idxU = resize idx6   :: Unsigned 6
      in if idxU >= startBit && idxU < endBit
           then let fb      = msb acc `xor` inBit
                    shifted = shiftL acc 1
                in if fb == high then shifted `xor` poly else shifted
           else acc

-- =============================================================================
-- Clocked Streaming CRC Component
-- =============================================================================

-- | Streaming CRC with start/valid handshake.
--
--   @streamingCRC poly initCrc start valid dataIn@
--
--   On each cycle where @valid@ is high, the CRC accumulator is updated
--   with the next data chunk.  When @start@ is asserted simultaneously,
--   the accumulator is re-initialised to @initCrc@ before processing
--   the first chunk (suitable for back-to-back message streams with no
--   dead cycle).
--
--   The output is the running CRC value, updated one cycle after the
--   corresponding input.
streamingCRC
  :: forall crcN dataN dom
   . ( HiddenClockResetEnable dom
     , KnownNat crcN
     , KnownNat dataN )
  => BitVector crcN                         -- ^ CRC polynomial
  -> BitVector crcN                         -- ^ Initial CRC value
  -> Signal dom Bool                        -- ^ start: high on first chunk
  -> Signal dom Bool                        -- ^ valid: high when dataIn valid
  -> Signal dom (BitVector dataN)           -- ^ dataIn: stream of data chunks
  -> Signal dom (BitVector crcN)            -- ^ crcOut: running CRC
streamingCRC poly initCrc start valid dataIn = crcReg
  where
    crcReg       = regEn initCrc valid nextCrc
    currentState = mux start (pure initCrc) crcReg
    nextCrc      = parallelCRCStep poly <$> currentState <*> dataIn
