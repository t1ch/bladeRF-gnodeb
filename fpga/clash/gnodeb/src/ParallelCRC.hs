{-# LANGUAGE TemplateHaskell #-}
module ParallelCRC (parallelCRCGenerator) where
import Prelude
import Data.Matrix
import Language.Haskell.TH
import qualified Clash.Sized.Vector as V
import Clash.Prelude (Vec,Bit)


myXOR 0 0 = 0
myXOR 0 1 = 1
myXOR 1 0 = 1
myXOR 1 1 = 0

{-
      See A PAINLESS GUIDE TO CRC ERROR DETECTION ALGORITHMS by Ross N. Williams. for the Serial CRC algorithm
      See  Parallel CRC Generator http://outputlogic.com/?p=158 for the parallel crc generator algorithm
-}
crcAux ::  [Int] -> [Int] -> [Int] -> [Int]

crcAux [] c _ = c

crcAux (messageMSB:message) (crcShiftRegMSB:crcShiftReg) (polynomialMSB:polynomial)
  | crcShiftRegMSB == 1 = crcAux message crcShiftReg'' (polynomialMSB:polynomial)
  |otherwise = crcAux message crcShiftReg' (polynomialMSB:polynomial)
  where
    crcShiftReg' =  crcShiftReg ++ [messageMSB]
    crcShiftReg'' = Prelude.zipWith (myXOR) crcShiftReg' polynomial

simpleCRC (messageMSB:message) (polynomialMSB:polynomial) =
  crcAux ms' c (polynomialMSB:polynomial)
  where
    d = length (polynomialMSB:polynomial) - 1
    c = replicate d 0
    ms' = (messageMSB:message) ++ c

--- For the example in the article Parallel CRC Generator http://outputlogic.com/?p=158 try the following
--- parallelCRCGenerator 4 [1,0,0,1,0,1]


iterateRows :: Int -> Int -> Int -> Matrix Int -> ExpQ -> [Q Exp]
iterateRows row col rowMax matrix messageOrCSR
  |row > rowMax = []
  |otherwise =
    if elementIsOne
      then nextTerm:nextTerm'
      else nextTerm'
  where
    element = (getElem row col matrix)
    elementIsOne = element == 1
    nextTerm =  [| $(messageOrCSR) V.!! $(litE (IntegerL (toInteger row - 1))) |]
    nextTerm' = (iterateRows (row+1) col rowMax matrix messageOrCSR)

iterateColumns :: Int -> Int -> Matrix Int -> Matrix Int -> ExpQ -> ExpQ -> [[Q Exp]]
iterateColumns col colMax matrixF matrixG message crcShiftReg
  |col > colMax = []
  |otherwise = sumOfTerms : iterateColumns (col+1) colMax matrixF matrixG message crcShiftReg
  where
    rowMaxF = nrows matrixF
    rowMaxG = nrows matrixG
    sumOfTerms = (++) (iterateRows 1 col rowMaxF matrixF crcShiftReg)
                      (iterateRows 1 col rowMaxG matrixG message)

parallelCRCGenerator :: Int -> [Int] -> ExpQ
parallelCRCGenerator messageWidth polynomial = result
  where
    polynomialWidth = (length polynomial) - 1
    f = fromLists $ Prelude.map (\x -> reverse (crcAux (replicate messageWidth 0) x polynomial)) (Prelude.map reverse (toLists (identity polynomialWidth)))
    g = fromLists $ Prelude.map (\x -> reverse (crcAux (replicate polynomialWidth 0) x polynomial)) (take messageWidth (Prelude.map reverse (toLists (identity polynomialWidth))))
    allTerms dataIn crcIn =
      iterateColumns 1 polynomialWidth f g (varE dataIn) (varE crcIn)
    rowTerms :: ExpQ -> ExpQ -> ExpQ
    rowTerms x acc = [| $(acc) `xor` $(x) |]
    foldRowTerms :: [ExpQ] -> ExpQ
    foldRowTerms (x:xs) = foldr rowTerms x xs
    foldColumnTerms :: [ExpQ] -> ExpQ -> ExpQ
    foldColumnTerms x acc = [|$(foldRowTerms x) :> $(acc)|]
    result = do
      dataIn <- newName "dataIn"
      crcIn  <- newName "crcIn"
      lamE [sigP (varP dataIn) [t|Vec $(litT (numTyLit (toInteger messageWidth))) Bit|]
           ,sigP (varP crcIn)  [t|Vec $(litT (numTyLit (toInteger polynomialWidth))) Bit|] ]
        (foldr foldColumnTerms [|Nil|] (allTerms dataIn crcIn))
