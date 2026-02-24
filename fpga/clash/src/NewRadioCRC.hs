{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | New Radio CRC implementation for 5G NR transport block CRC computation.
--
--   Uses the StreamingCRC parallel step function to compute CRC updates:
--   Clash's normaliser unfolds the serial CRC step across all 32 data
--   bits into a purely combinational XOR tree — no Template Haskell or
--   compile-time matrix precomputation required.
--
--   The CRC engine is MSB-first throughout: data dwords are fed in
--   natural bit order with no reversal at any stage.
--
--   Supports all three NR CRC polynomials defined in 3GPP TS 38.212:
--
--     CRC-24A: x²⁴+x²³+x¹⁸+x¹⁷+x¹⁴+x¹¹+x¹⁰+x⁷+x⁶+x⁵+x⁴+x³+x+1
--              Generator polynomial = 0x864CFB (without leading 1)
--              Used for TB CRC when TB size > 3824 bits (Sec 7.2.1)
--
--     CRC-24B: x²⁴+x²³+x⁶+x⁵+x+1
--              Generator polynomial = 0x800063 (without leading 1)
--              Used for code block CRC (Sec 5.1)
--
--     CRC-16:  x¹⁶+x¹²+x⁵+1
--              Generator polynomial = 0x1021 (without leading 1)
--              Used for TB CRC when TB size ≤ 3824 bits (Sec 7.2.1)
--
--   All step functions use 32-bit parallel data width to match the
--   FAPI TLV dword ingestion rate (one CRC update per clock cycle per
--   dword consumed).
--
--   Usage:
--     1. resetNrCrc     — zero accumulators
--     2. selectCrcType  — set the active CRC polynomial from TB size
--     3. updateNrCrc    — feed each TB data dword (called per cycle)
--     4. finalizeNrCrc  — extract the computed CRC after the last dword

module NewRadioCRC
  ( -- * Combinational CRC step functions
    nrCrc24AStep
  , nrCrc24BStep
  , nrCrc16Step
    -- * Fixed-width CRC-24B steps (for TB CRC → last CB feeding)
  , nrCrc24BStep24
  , nrCrc24BStep16
    -- * Range-gated CRC-24B step (for CB boundary splits)
  , rangeCrc24BStep
    -- * Feed raw TB CRC register into CB CRC-24B
  , updateCbCrcWithTbCrc
    -- * NR CRC polynomials (without leading 1)
  , polyCrc24A
  , polyCrc24B
  , polyCrc16
    -- * State-based API for inline TB CRC computation
  , resetNrCrc
  , selectCrcType
  , updateNrCrc
  , finalizeNrCrc
  ) where

import Clash.Prelude
import StreamingCRC   (parallelCRCStep, rangeParallelCRCStep)
import GNodeBFAPITypes (NrCrcType(..), NrCrcState(..))

-- =============================================================================
-- NR CRC Polynomials (3GPP TS 38.212)
-- =============================================================================
--
-- Specified as BitVectors without the implicit leading 1, suitable for
-- direct use with parallelCRCStep.

-- | CRC-24A polynomial: 0x864CFB
--   x²⁴+x²³+x¹⁸+x¹⁷+x¹⁴+x¹¹+x¹⁰+x⁷+x⁶+x⁵+x⁴+x³+x+1
polyCrc24A :: BitVector 24
polyCrc24A = 0x864CFB

-- | CRC-24B polynomial: 0x800063
--   x²⁴+x²³+x⁶+x⁵+x+1
polyCrc24B :: BitVector 24
polyCrc24B = 0x800063

-- | CRC-16 polynomial: 0x1021
--   x¹⁶+x¹²+x⁵+1
polyCrc16 :: BitVector 16
polyCrc16 = 0x1021

-- =============================================================================
-- NR CRC Combinational Step Functions
-- =============================================================================
--
-- Each function takes a 32-bit data dword and the current CRC register,
-- and returns the updated CRC register.  Pure combinational — Clash
-- unfolds the serial step into an XOR tree automatically.

-- | CRC-24A one-dword parallel step.
nrCrc24AStep :: BitVector 32 -> BitVector 24 -> BitVector 24
nrCrc24AStep dw crc = parallelCRCStep polyCrc24A crc dw

-- | CRC-24B one-dword parallel step.
nrCrc24BStep :: BitVector 32 -> BitVector 24 -> BitVector 24
nrCrc24BStep dw crc = parallelCRCStep polyCrc24B crc dw

-- | CRC-16 one-dword parallel step.
nrCrc16Step :: BitVector 32 -> BitVector 16 -> BitVector 16
nrCrc16Step dw crc = parallelCRCStep polyCrc16 crc dw

-- =============================================================================
-- Fixed-Width CRC-24B Steps (for TB CRC → Last CB Feeding)
-- =============================================================================
--
-- These feed the raw TB CRC register (24 or 16 bits) directly into a
-- CRC-24B accumulator — no zero-padding, no finalizeNrCrc roundtrip.

-- | CRC-24B step over a 24-bit input (for CRC-24A register feeding).
nrCrc24BStep24 :: BitVector 24 -> BitVector 24 -> BitVector 24
nrCrc24BStep24 dw24 crc = parallelCRCStep polyCrc24B crc dw24

-- | CRC-24B step over a 16-bit input (for CRC-16 register feeding).
nrCrc24BStep16 :: BitVector 16 -> BitVector 24 -> BitVector 24
nrCrc24BStep16 dw16 crc = parallelCRCStep polyCrc24B crc dw16

-- =============================================================================
-- Range-Gated CRC-24B Step (for CB Boundary Splits)
-- =============================================================================

-- | CRC-24B update over an arbitrary bit range within a 32-bit dword.
--   Used when a CB boundary falls mid-dword.
rangeCrc24BStep :: BitVector 24 -> Unsigned 6 -> Unsigned 6 -> BitVector 32 -> BitVector 24
rangeCrc24BStep crc startBit endBit dw =
  rangeParallelCRCStep polyCrc24B crc startBit endBit dw

-- =============================================================================
-- Feed Raw TB CRC Register into CB CRC-24B
-- =============================================================================

-- | Feed the raw TB CRC register into a CB CRC-24B accumulator.
--   Feeds exactly the meaningful CRC bits (24 for CRC-24A, 16 for CRC-16)
--   through a fixed-width parallelCRCStep — no zero-padding.
updateCbCrcWithTbCrc :: NrCrcState -> NrCrcState -> NrCrcState
updateCbCrcWithTbCrc tbSt cbSt =
  case ncCrcType tbSt of
    NR_CRC24A -> cbSt { ncCrc24BReg = nrCrc24BStep24 (ncCrc24AReg tbSt) (ncCrc24BReg cbSt) }
    NR_CRC16  -> cbSt { ncCrc24BReg = nrCrc24BStep16 (ncCrc16Reg  tbSt) (ncCrc24BReg cbSt) }
    _         -> cbSt

-- =============================================================================
-- State-based API
-- =============================================================================

-- | Reset all CRC accumulators to zero.
--   Call at PDU boundary before beginning a new TB.
resetNrCrc :: NrCrcState -> NrCrcState
resetNrCrc st = st
  { ncCrc24AReg = 0
  , ncCrc24BReg = 0
  , ncCrc16Reg  = 0
  , ncCrcType   = NR_CRC_NONE
  }

-- | Select the CRC type from TB size in bytes (3GPP TS 38.212 Sec 7.2.1).
--
--   TB size > 3824 bits (478 bytes) → CRC-24A
--   TB size ≤ 3824 bits             → CRC-16
--
--   CRC-24B (code-block CRC) is selected separately by the segmentation
--   stage and is not derived from TB size.
selectCrcType :: BitVector 32 -> NrCrcType
selectCrcType tbSizeBytes =
  let tbSz = unpack tbSizeBytes :: Unsigned 32
  in if tbSz > 478 then NR_CRC24A else NR_CRC16

-- | Feed one 32-bit TB data dword through the active CRC engine.
--   Pure MSB-first — no bit reversal, no phase tracking.
updateNrCrc :: NrCrcState -> BitVector 32 -> NrCrcState
updateNrCrc st dw =
  case ncCrcType st of
    NR_CRC24A -> st { ncCrc24AReg = nrCrc24AStep dw (ncCrc24AReg st) }
    NR_CRC24B -> st { ncCrc24BReg = nrCrc24BStep dw (ncCrc24BReg st) }
    NR_CRC16  -> st { ncCrc16Reg  = nrCrc16Step  dw (ncCrc16Reg  st) }
    _         -> st

-- | Extract the computed CRC value after the last TB dword has been
--   processed.
--
--   Returns a 32-bit vector with the CRC in the most-significant bits
--   and zero padding in the LSBs (MSB-first layout):
--
--     CRC-24A/B → CRC in bits [31:8],  zeros in bits [7:0]
--     CRC-16    → CRC in bits [31:16], zeros in bits [15:0]
--
--   (3GPP TS 38.212 Sec 7.2.1)
finalizeNrCrc :: NrCrcState -> BitVector 32
finalizeNrCrc st =
  case ncCrcType st of
    NR_CRC24A -> ncCrc24AReg st ++# (0 :: BitVector 8)
    NR_CRC24B -> ncCrc24BReg st ++# (0 :: BitVector 8)
    NR_CRC16  -> ncCrc16Reg  st ++# (0 :: BitVector 16)
    _         -> 0
