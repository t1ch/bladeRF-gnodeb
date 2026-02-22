{-# LANGUAGE TemplateHaskell #-}

-- | Compile-time parallel CRC combinational logic generator.
--
--   Generates pure XOR-tree functions that compute one CRC update step
--   over an /n/-bit parallel data word in a single clock cycle, replacing
--   the traditional bit-serial shift register.
--
--   Algorithm — from "Parallel CRC Generator" (outputlogic.com/?p=158):
--
--     Given a CRC polynomial of degree /k/ and a parallel data width /n/,
--     two Boolean matrices are derived:
--
--       F  (k × k) — maps the current CRC register to the next CRC
--                     register when the data input is all zeros.
--
--       G  (n × k) — maps the data input to the next CRC register
--                     when the current CRC register is all zeros.
--
--     Each column /j/ of the output CRC register is then:
--
--       crc'[j] = XOR of { crcIn[i] | F[i][j] = 1 }
--               ⊕ XOR of { dataIn[i] | G[i][j] = 1 }
--
--     The F and G matrices are computed at compile time by running the
--     serial CRC algorithm over the standard basis vectors of ℤ₂ⁿ.
--     Template Haskell splices the resulting XOR trees into the
--     generated function body, producing fully unrolled combinational
--     logic with zero run-time overhead.
--
--   Usage (in a Clash module with TemplateHaskell enabled):
--
--   @
--     crc32Comb :: Vec 8 Bit -> Vec 32 Bit -> Vec 32 Bit
--     crc32Comb = $(parallelCRCGenerator 8 [1,0,0,...,1])
--   @
--
--   The polynomial is specified MSB-first as a list of 0/1 integers,
--   including the leading 1.  For a degree-/k/ polynomial, the list
--   has /k + 1/ elements.
--
--   Reference:
--     Ross N. Williams, "A Painless Guide to CRC Error Detection Algorithms"
--     (for the serial CRC algorithm underlying F/G matrix derivation)

module ParallelCRC (parallelCRCGenerator) where

import Prelude
import Data.Matrix          (Matrix, fromLists, toLists, identity, getElem, nrows)
import Language.Haskell.TH
import qualified Clash.Sized.Vector as V
import Clash.Prelude         (Vec, Bit)

-- =============================================================================
-- Serial CRC Kernel (used only at compile time for matrix derivation)
-- =============================================================================

-- | Single-bit XOR over {0, 1} integers.
--   Total on the domain {0, 1}; all other inputs are a programming error
--   (caught by GHC's incomplete-pattern warning if -Wall is enabled).
bitXor :: Int -> Int -> Int
bitXor 0 0 = 0
bitXor 0 1 = 1
bitXor 1 0 = 1
bitXor 1 1 = 0
bitXor _ _ = error "ParallelCRC.bitXor: arguments must be 0 or 1"

-- | Core serial CRC shift-register step.
--
--   @serialCrcStep message crcReg polynomial@
--
--   Processes every bit in /message/ through the CRC shift register
--   according to the standard long-division algorithm:
--
--     1. Shift the MSB of the register out.
--     2. Shift the next message bit in at the LSB.
--     3. If the ejected bit was 1, XOR the register with the polynomial
--        (excluding the leading 1, which is implicit).
--
--   The polynomial list includes the leading 1 for clarity but only
--   the remaining /k/ coefficients participate in the XOR.
serialCrcStep :: [Int] -> [Int] -> [Int] -> [Int]
serialCrcStep []             crc _poly = crc
serialCrcStep (msgBit:msg) (crcMsb:crc) poly@(_leadingOne:polyTail)
  | crcMsb == 1 = serialCrcStep msg (zipWith bitXor shifted polyTail) poly
  | otherwise    = serialCrcStep msg shifted poly
  where
    shifted = crc ++ [msgBit]
serialCrcStep _ _ _ = error "ParallelCRC.serialCrcStep: invalid polynomial (empty)"

-- =============================================================================
-- F and G Matrix Construction
-- =============================================================================

-- | Build the F matrix (k × k).
--
--   Column /i/ of F is obtained by running the serial CRC over an
--   all-zero message of length /n/ (the data width) with the CRC
--   register initialised to basis vector /eᵢ/.
--
--   Intuitively: "what does the polynomial feedback do to each register
--   bit when no new data arrives for /n/ cycles?"
buildMatrixF :: Int -> [Int] -> Matrix Int
buildMatrixF dataWidth poly =
  let k         = length poly - 1
      zeroMsg   = replicate dataWidth 0
      basisRows = map reverse (toLists (identity k))
      resultRows = map (\basis -> reverse (serialCrcStep zeroMsg basis poly)) basisRows
  in fromLists resultRows

-- | Build the G matrix (n × k).
--
--   Column /i/ of G is obtained by running the serial CRC over
--   basis vector /eᵢ/ (of length /n/) with the CRC register
--   initialised to zero.
--
--   Intuitively: "what is the CRC contribution of each individual
--   data bit in isolation?"
buildMatrixG :: Int -> [Int] -> Matrix Int
buildMatrixG dataWidth poly =
  let k        = length poly - 1
      zeroCrc  = replicate k 0
      -- Take the first /dataWidth/ basis vectors from an identity matrix
      -- of size >= dataWidth.  (identity k works because k >= dataWidth
      -- is not required — we only take the rows we need.)
      basisRows = take dataWidth (map reverse (toLists (identity (max dataWidth k))))
      resultRows = map (\basis -> reverse (serialCrcStep zeroCrc basis poly)) basisRows
  in fromLists resultRows

-- =============================================================================
-- Template Haskell XOR-Tree Code Generation
-- =============================================================================

-- | Collect all matrix entries equal to 1 in column /col/ (rows 1..rowMax)
--   and emit TH expressions that index into the given input vector.
--
--   Returns a list of @[| vec V.!! idx |]@ expressions — one per set bit.
gatherColumnTerms :: Int -> Int -> Matrix Int -> ExpQ -> [ExpQ]
gatherColumnTerms col rowMax matrix vecExpr =
  [ [| $(vecExpr) V.!! $(litE (IntegerL (toInteger row - 1))) |]
  | row <- [1 .. rowMax]
  , getElem row col matrix == 1
  ]

-- | For each output CRC bit (column 1..k), collect contributing terms
--   from both the F matrix (CRC register feedback) and the G matrix
--   (data input contribution).
--
--   Returns a list of /k/ term-lists, one per output bit.
allColumnTerms :: Matrix Int -> Matrix Int -> ExpQ -> ExpQ -> [[ExpQ]]
allColumnTerms matF matG crcInExpr dataInExpr =
  [ gatherColumnTerms col (nrows matF) matF crcInExpr
    ++ gatherColumnTerms col (nrows matG) matG dataInExpr
  | col <- [1 .. ncols']
  ]
  where
    -- Both matrices have the same number of columns (= polynomial degree k)
    ncols' = length (head (toLists matF))

-- | Fold a non-empty list of bit expressions into a single XOR tree.
--
--   @foldXorTree [a, b, c]  ≡  a `xor` b `xor` c@
foldXorTree :: [ExpQ] -> ExpQ
foldXorTree []     = error "ParallelCRC.foldXorTree: empty term list (degenerate polynomial?)"
foldXorTree [x]    = x
foldXorTree (x:xs) = foldl (\acc t -> [| $(acc) `xor` $(t) |]) x xs

-- | Build a Clash Vec literal from a list of bit expressions,
--   right-folding with the @(:>)@ cons operator and @Nil@.
--
--   @buildVecExpr [e0, e1, e2]  ≡  e0 :> e1 :> e2 :> Nil@
buildVecExpr :: [ExpQ] -> ExpQ
buildVecExpr = foldr (\e acc -> [| $(e) :> $(acc) |]) [| Nil |]

-- =============================================================================
-- Public API
-- =============================================================================

-- | @parallelCRCGenerator dataWidth polynomial@
--
--   Splice this inside @$(...)@ to generate a function:
--
--   @
--     Vec dataWidth Bit -> Vec polyDegree Bit -> Vec polyDegree Bit
--   @
--
--   that computes one parallel CRC update step.
--
--   /dataWidth/  — number of parallel data bits processed per call.
--   /polynomial/ — CRC polynomial coefficients, MSB-first, including
--                  the leading 1.  Length = degree + 1.
--
--   Example (CRC-16 with 32-bit parallel data):
--
--   @
--     crc16Comb :: Vec 32 Bit -> Vec 16 Bit -> Vec 16 Bit
--     crc16Comb = $(parallelCRCGenerator 32 [1,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,1])
--   @
parallelCRCGenerator :: Int -> [Int] -> ExpQ
parallelCRCGenerator dataWidth polynomial = do
  -- Derive the F (feedback) and G (data) Boolean matrices at compile time
  let matF = buildMatrixF dataWidth polynomial
      matG = buildMatrixG dataWidth polynomial
      polyDegree = length polynomial - 1

  -- Create fresh TH names for the lambda parameters
  dataInName <- newName "dataIn"
  crcInName  <- newName "crcIn"

  let dataInE = varE dataInName
      crcInE  = varE crcInName

  -- For each output bit, gather the XOR terms and fold them
  let terms    = allColumnTerms matF matG crcInE dataInE
      xorTrees = map foldXorTree terms

  -- Build the result: \dataIn crcIn -> bit0 :> bit1 :> ... :> Nil
  lamE
    [ sigP (varP dataInName)
        [t| Vec $(litT (numTyLit (toInteger dataWidth))) Bit |]
    , sigP (varP crcInName)
        [t| Vec $(litT (numTyLit (toInteger polyDegree))) Bit |]
    ]
    (buildVecExpr xorTrees)
