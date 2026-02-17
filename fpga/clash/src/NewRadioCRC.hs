{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}

-- | New Radio CRC implementation for 5G NR transport block CRC computation.
--
--   Instantiates parallel CRC combinational logic at compile time via
--   Template Haskell from ParallelCRC, for all three NR CRC polynomials
--   defined in 3GPP TS 38.212:
--
--     CRC-24A: x^24+x^23+x^18+x^17+x^14+x^11+x^10+x^7+x^6+x^5+x^4+x^3+x+1
--              Used for TB CRC when TB size > 3824 bits (Sec 7.2.1)
--
--     CRC-24B: x^24+x^23+x^6+x^5+x+1
--              Used for code block CRC (Sec 5.1)
--
--     CRC-16:  x^16+x^12+x^5+1
--              Used for TB CRC when TB size <= 3824 bits (Sec 7.2.1)
--
--   All combinational functions are generated with 32-bit parallel data
--   width to match the FAPI TLV dword ingestion rate (one CRC update
--   per clock cycle per dword consumed).
--
--   The CRC computation follows the PoC pattern with phase-dependent
--   bit-reversal:
--     CRCStarting    → first dword: comb dataIn crcReg
--     CRCCalculating → intermediate: comb (reverse dataIn) (reverse crcReg)
--     CRCDone        → last dword:   reverse (comb (reverse dataIn) (reverse crcReg))
--
--   The extra output reversal on CRCDone produces the final CRC value
--   ready for appending to the transport block.
--
--   Usage:
--     1. resetNrCrc     — zero accumulators, set phase to CRCStarting
--     2. selectCrcType  — set the active CRC from TB size
--     3. updateNrCrc    — feed each TB data dword (called per cycle);
--                         caller must set ncCrcPhase = CRCDone before
--                         the last dword
--     4. finalizeNrCrc  — extract computed CRC after last dword

module NewRadioCRC
  ( -- * Combinational CRC cores (TH-generated)
    nrCrc24AComb
  , nrCrc24BComb
  , nrCrc16Comb
    -- * State-based API for inline TB CRC computation
  , resetNrCrc
  , selectCrcType
  , updateNrCrc
  , finalizeNrCrc
  ) where

import Clash.Prelude
import ParallelCRC
import GNodeBFAPITypes (NrCrcType(..), NrCrcPhase(..), NrCrcState(..))

-- =============================================================================
-- NR CRC Combinational Logic (generated at compile time via TH)
-- =============================================================================
--
-- Each function takes a data vector and the current CRC register,
-- and returns the updated CRC register. These are pure combinational
-- XOR trees — no sequential state. The caller holds the register in
-- NrCrcState and feeds it back each clock cycle.

-- | CRC-24A parallel combinational core.
nrCrc24AComb :: Vec 32 Bit -> Vec 24 Bit -> Vec 24 Bit
nrCrc24AComb = $(parallelCRCGenerator 32
  [1,1,0,0,0,0,1,1,0,0,1,0,0,1,1,1,1,1,1,0,1,1,0,1,1])

-- | CRC-24B parallel combinational core.
nrCrc24BComb :: Vec 32 Bit -> Vec 24 Bit -> Vec 24 Bit
nrCrc24BComb = $(parallelCRCGenerator 32
  [1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,1,1,1])

-- | CRC-16 parallel combinational core.
nrCrc16Comb :: Vec 32 Bit -> Vec 16 Bit -> Vec 16 Bit
nrCrc16Comb = $(parallelCRCGenerator 32
  [1,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,1])

-- =============================================================================
-- State-based API
-- =============================================================================

-- | Reset all CRC accumulators to zero and set phase to CRCStarting.
--   Call at PDU boundary before beginning a new TB.
resetNrCrc :: NrCrcState -> NrCrcState
resetNrCrc st = st
  { ncCrc24AReg = repeat 0
  , ncCrc24BReg = repeat 0
  , ncCrc16Reg  = repeat 0
  , ncCrcType   = NR_CRC_NONE
  , ncCrcPhase  = CRCStarting
  }

-- | Select the CRC type from TB size in bytes (3GPP TS 38.212 Sec 7.2.1).
--   TB size > 3824 bits (478 bytes) → CRC-24A, otherwise → CRC-16.
selectCrcType :: BitVector 32 -> NrCrcType
selectCrcType tbSizeBytes =
  let tbSz = unpack tbSizeBytes :: Unsigned 32
  in if tbSz > 478 then NR_CRC24A else NR_CRC16

-- | Feed one 32-bit TB data dword through the active CRC engine.
--
--   Bit-reversal is phase-dependent, following the PoC pattern:
--     CRCStarting    → comb dataIn crcReg
--     CRCCalculating → comb (reverse dataIn) (reverse crcReg)
--     CRCDone        → reverse (comb (reverse dataIn) (reverse crcReg))
--
--   After the first dword the phase transitions to CRCCalculating.
--   The caller must set ncCrcPhase = CRCDone before feeding the last
--   dword so the output reversal produces the final CRC value.
updateNrCrc :: NrCrcState -> BitVector 32 -> NrCrcState
updateNrCrc st dw =
  let dataVec = unpack dw :: Vec 32 Bit
      phase   = ncCrcPhase st
  in case ncCrcType st of
    NR_CRC24A ->
      let reg  = ncCrc24AReg st
          crc' = case phase of
            CRCStarting    -> nrCrc24AComb dataVec reg
            CRCDone        -> reverse (nrCrc24AComb (reverse dataVec) (reverse reg))
            _              -> nrCrc24AComb (reverse dataVec) (reverse reg)
          nextPhase = case phase of
            CRCStarting -> CRCCalculating
            p           -> p
      in st { ncCrc24AReg = crc', ncCrcPhase = nextPhase }

    NR_CRC24B ->
      let reg  = ncCrc24BReg st
          crc' = case phase of
            CRCStarting    -> nrCrc24BComb dataVec reg
            CRCDone        -> reverse (nrCrc24BComb (reverse dataVec) (reverse reg))
            _              -> nrCrc24BComb (reverse dataVec) (reverse reg)
          nextPhase = case phase of
            CRCStarting -> CRCCalculating
            p           -> p
      in st { ncCrc24BReg = crc', ncCrcPhase = nextPhase }

    NR_CRC16 ->
      let reg  = ncCrc16Reg st
          crc' = case phase of
            CRCStarting    -> nrCrc16Comb dataVec reg
            CRCDone        -> reverse (nrCrc16Comb (reverse dataVec) (reverse reg))
            _              -> nrCrc16Comb (reverse dataVec) (reverse reg)
          nextPhase = case phase of
            CRCStarting -> CRCCalculating
            p           -> p
      in st { ncCrc16Reg = crc', ncCrcPhase = nextPhase }

    _ -> st

-- | Extract the computed CRC value after the last TB dword has been
--   processed with ncCrcPhase = CRCDone.
--
--   Returns a 32-bit vector with zero padding in the MSBs:
--     CRC-24A/B → bits [31:24] zero, CRC in bits [23:0]
--     CRC-16    → bits [31:16] zero, CRC in bits [15:0]
--   (3GPP TS 38.212 Sec 7.2.1)
finalizeNrCrc :: NrCrcState -> BitVector 32
finalizeNrCrc st =
  case ncCrcType st of
    NR_CRC24A -> (0 :: BitVector 8)  ++# pack (ncCrc24AReg st)
    NR_CRC24B -> (0 :: BitVector 8)  ++# pack (ncCrc24BReg st)
    NR_CRC16  -> (0 :: BitVector 16) ++# pack (ncCrc16Reg st)
    _         -> 0
