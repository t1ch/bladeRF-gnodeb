{-# LANGUAGE DataKinds #-}

-- | CSI-RS PDU body dword parser (SCF-222 Table 3.4.2.4-1, simplified for PoC).
--
--   Dword layout within the PDU body:
--     DW0: [bwpSize(16)     | bwpStart(16)]
--     DW1: [scs(8)          | cp(8)        | pduIndex(16)]
--     DW2: [startRb(16)     | nrb(16)]
--     DW3: [scramblingId(16)| pad(16)]
--     DW4+: skipped

module FapiDlPduCsiRs
  ( parseCsiRsBodyDw
  ) where

import Clash.Prelude
import GNodeBFAPITypes

parseCsiRsBodyDw :: CsiRsInfo -> Unsigned 16 -> BitVector 32 -> CsiRsInfo
parseCsiRsBodyDw cr dwIdx dw =
  case dwIdx of
    0 -> nullCsiRsInfo
           { crValid    = 1
           , crBwpSize  = slice d31 d16 dw
           , crBwpStart = slice d15 d0  dw
           }
    1 -> cr { crPduIndex     = slice d15 d0  dw }
    2 -> cr { crStartRb      = slice d31 d16 dw
            , crNrb          = slice d15 d0  dw
            }
    3 -> cr { crScramblingId = slice d31 d16 dw }
    _ -> cr
