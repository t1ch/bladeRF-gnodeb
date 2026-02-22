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
    serialStep acc bit =
      let feedback = msb acc `xor` bit
          shifted  = shiftL acc 1
      in if feedback == high then shifted `xor` poly else shifted

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
