{-# LANGUAGE DataKinds #-}

-- | SSB PDU body dword parser (SCF-222 Table 3.4.2.5-1, simplified for PoC).
--
--   SSB carries the BCH/MIB payload directly in the DL_TTI.request body;
--   there is no corresponding TX_DATA.request for SSB.
--
--   Dword layout within the PDU body:
--     DW0: [physCellId(16)         | betaPss(8)          | ssbBlockIdx(8)]
--     DW1: [ssbSubcarrierOffset(8) | ssbOffsetPointA(16) | pad(8)]
--     DW2: [bchPayload(32)]   -- 24-bit MIB, zero-padded MSBs
--     DW3+: skipped

module FapiDlPduSsb
  ( parseSsbBodyDw
  ) where

import Clash.Prelude
import GNodeBFAPITypes

parseSsbBodyDw :: SsbInfo -> Unsigned 16 -> BitVector 32 -> SsbInfo
parseSsbBodyDw sb dwIdx dw =
  case dwIdx of
    0 -> nullSsbInfo
           { sbValid               = 1
           , sbPhysCellId          = slice d31 d16 dw
           , sbBetaPss             = slice d15 d8  dw
           , sbSsbBlockIdx         = slice d7  d0  dw
           }
    1 -> sb { sbSsbSubcarrierOffset = slice d31 d24 dw
            , sbSsbOffsetPointA     = slice d23 d8  dw
            }
    2 -> sb { sbBchPayload = dw }
    _ -> sb
