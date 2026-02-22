{-# LANGUAGE DataKinds #-}

-- | PDCCH PDU body dword parser (SCF-222 Table 3.4.2.3-1, simplified for PoC).
--
--   Dword layout within the PDU body:
--     DW0: [bwpSize(16)   | bwpStart(16)]
--     DW1: [scs(8)        | cp(8)        | pduIndex(16)]
--     DW2: [rnti(16)      | aggLevel(8)  | cceIndex(8)]
--     DW3+: skipped

module FapiDlPduPdcch
  ( parsePdcchBodyDw
  ) where

import Clash.Prelude
import GNodeBFAPITypes

parsePdcchBodyDw :: PdcchInfo -> Unsigned 16 -> BitVector 32 -> PdcchInfo
parsePdcchBodyDw pc dwIdx dw =
  case dwIdx of
    0 -> nullPdcchInfo
           { pcValid    = 1
           , pcBwpSize  = slice d31 d16 dw
           , pcBwpStart = slice d15 d0  dw
           }
    1 -> pc { pcPduIndex = slice d15 d0  dw }
    2 -> pc { pcRnti     = slice d31 d16 dw
            , pcAggLevel = slice d15 d8  dw
            , pcCceIndex = slice d7  d0  dw
            }
    _ -> pc
