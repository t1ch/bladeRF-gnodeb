-- Uncomment the following line:
--{-# OPTIONS_GHC -ddump-splices #-}
--
-- To see the spliced in code dumped to stdout:
--
-- TestSimpleCRC.hs:7:6-43: Splicing expression
--     parallelCRCGenerator 4 [1, 0, 0, 1, 0, 1]
--   ======>
--     \ (crcIn_alJU :: Vec 4 Bit) (dataIn_alJT :: Vec 5 Bit)
--       -> (((((crcIn_alJU !! 1) `xor` (dataIn_alJT !! 3))
--               `xor` (dataIn_alJT !! 0))
--              `xor` (crcIn_alJU !! 4))
--             :>
--               (((crcIn_alJU !! 2) `xor` (dataIn_alJT !! 1))
--                  :>
--                    (((((((crcIn_alJU !! 1) `xor` (dataIn_alJT !! 3))
--                           `xor` (dataIn_alJT !! 2))
--                          `xor` (dataIn_alJT !! 0))
--                         `xor` (crcIn_alJU !! 4))
--                        `xor` (crcIn_alJU !! 3))
--                       :>
--                         (((((crcIn_alJU !! 2) `xor` (dataIn_alJT !! 3))
--                              `xor` (dataIn_alJT !! 1))
--                             `xor` (crcIn_alJU !! 4))
--                            :>
--                              ((((crcIn_alJU !! 0) `xor` (dataIn_alJT !! 2))
--                                  `xor` (crcIn_alJU !! 3))
--                                 :> Nil)))))
module NewRadioCRC where
import Clash.Prelude
import ParallelCRC
data CRCState = CRCStarting | CRCCalculating | CRCDone
  deriving (Show, Generic, NFDataX, Eq)

newRadioCRCComb :: Vec 3 Bit -> Vec 5 Bit -> Vec 5 Bit
newRadioCRCComb = $(parallelCRCGenerator 3 [1,0,0,1,0,1])

newRadioCRCNextState crcCurrentState (dataIn,crcState)
  |crcState == CRCStarting = (newRadioCRCComb dataIn crcCurrentState)
  |crcState == CRCDone = reverse (newRadioCRCComb (reverse dataIn) (reverse crcCurrentState))
  |otherwise = (newRadioCRCComb (reverse dataIn) (reverse crcCurrentState))

newRadioCRC input  = moore newRadioCRCNextState id (0:>0:>0:>0:>0:>Nil) input
