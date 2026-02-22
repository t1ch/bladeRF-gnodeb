{-# LANGUAGE DataKinds #-}

-- | PDSCH PDU body dword parser (SCF-222 Table 3.4.2.2-1, simplified for PoC).
--
--   Dword layout within the PDU body:
--     DW0: [bwpSize(16)  | bwpStart(16)]
--     DW1: [scs(8)       | cp(8)        | pduIndex(16)]
--     DW2: [rnti(16)     | pad(16)]
--     DW3: [tbSizeBytes(32)]
--     DW4+: skipped

module FapiDlPduPdsch
  ( parsePdschBodyDw
  ) where

import Clash.Prelude
import GNodeBFAPITypes

parsePdschBodyDw :: PdschInfo -> Unsigned 16 -> BitVector 32 -> PdschInfo
parsePdschBodyDw pInfo dwIdx dw =
  case dwIdx of
    0 -> nullPdschInfo
           { piValid    = 1
           , piBwpSize  = slice d31 d16 dw
           , piBwpStart = slice d15 d0  dw
           }
    1 -> pInfo { piPduIndex    = slice d15 d0  dw }
    2 -> pInfo { piRnti        = slice d31 d16 dw }
    3 -> pInfo { piTbSizeBytes = dw               }
    _ -> pInfo
