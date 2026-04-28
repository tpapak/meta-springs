{-# LANGUAGE BangPatterns #-}
-- |
-- Module      : Data.Meta.Matrix
-- Description : Pure Haskell matrix operations replacing hmatrix
-- Copyright   : (c) Thodoris Papakonstantinou, 2026
-- License     : GPL-3
-- Maintainer  : dev@tpapak.com
--
-- Minimal matrix library for NMA spring network computations.
-- Uses unboxed vectors for O(1) element access and cache-friendly layout.
-- Replaces hmatrix dependency to enable WASM compilation.
module Data.Meta.Matrix
  ( Matrix
  , Vector
  -- * Construction
  , fromLists
  , fromList
  , fromRows
  , diagl
  , ident
  , reshape
  -- * Access
  , (!)
  , atIndex
  , toList
  , toLists
  , flatten
  , rows
  , cols
  , size
  , takeDiag
  -- * Operations
  , (<>)
  , tr
  , scale
  , diag
  , inv
  , det
  , pinv
  , mSub
  , mDiv
  , mAdd
  -- * LAPACK-backed fast variants (use these in hot loops)
  , invFast
  , detFast
  , mulFast
  -- * Vector operations
  , mapVectorWithIndex
  ) where

import Prelude hiding ((<>))
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import qualified Data.Vector.Storable as VS
import qualified Numeric.LinearAlgebra as LA
import Control.Monad.ST
import Data.STRef

-- | Row-major matrix backed by unboxed vector
data Matrix = Matrix
  { mRows :: {-# UNPACK #-} !Int
  , mCols :: {-# UNPACK #-} !Int
  , mData :: !(V.Vector Double)
  } deriving (Eq)

instance Show Matrix where
  show m = "(Matrix " ++ show (mRows m) ++ "x" ++ show (mCols m) ++ ")"

-- | Vector as list (kept for API compatibility)
type Vector = [Double]

-- * Construction

fromLists :: [[Double]] -> Matrix
fromLists [] = Matrix 0 0 V.empty
fromLists rs =
  let r = length rs
      c = length (head rs)
  in Matrix r c (V.fromList (concat rs))

fromList :: [Double] -> Vector
fromList = id

fromRows :: [Vector] -> Matrix
fromRows = fromLists

diagl :: [Double] -> Matrix
diagl ds =
  let n = length ds
  in Matrix n n $ V.generate (n*n) $ \idx ->
       let (i, j) = idx `divMod` n
       in if i == j then ds !! i else 0

ident :: Int -> Matrix
ident n = Matrix n n $ V.generate (n*n) $ \idx ->
  let (i, j) = idx `divMod` n
  in if i == j then 1 else 0

reshape :: Int -> Int -> [Double] -> Matrix
reshape r c xs = Matrix r c (V.fromList (take (r*c) xs))

-- * Access

-- | Row indexing
(!) :: Matrix -> Int -> Vector
(!) (Matrix _ c d) i =
  let off = i * c
  in [d V.! (off + j) | j <- [0..c-1]]

-- | Element access
atIndex :: Matrix -> (Int, Int) -> Double
atIndex (Matrix _ c d) (i, j) = d V.! (i * c + j)
{-# INLINE atIndex #-}

toList :: Vector -> [Double]
toList = id

toLists :: Matrix -> [[Double]]
toLists m = [m ! i | i <- [0..mRows m - 1]]

flatten :: Matrix -> Vector
flatten = V.toList . mData

rows :: Matrix -> Int
rows = mRows

cols :: Matrix -> Int
cols = mCols

size :: Matrix -> (Int, Int)
size m = (mRows m, mCols m)

takeDiag :: Matrix -> Vector
takeDiag m = [atIndex m (i, i) | i <- [0..min (mRows m) (mCols m) - 1]]

-- * Operations

-- | Matrix multiplication using mutable accumulation
(<>) :: Matrix -> Matrix -> Matrix
(<>) a b = runST $ do
  let !ra = mRows a
      !ca = mCols a
      !cb = mCols b
      !da = mData a
      !db = mData b
  result <- MV.new (ra * cb)
  let go i j
        | i >= ra = return ()
        | j >= cb = go (i+1) 0
        | otherwise = do
            let !off_a = i * ca
                loop !k !acc
                  | k >= ca = MV.unsafeWrite result (i * cb + j) acc
                  | otherwise =
                      let !v = (da `V.unsafeIndex` (off_a + k)) * (db `V.unsafeIndex` (k * cb + j))
                       in loop (k+1) (acc + v)
            loop 0 0
            go i (j+1)
  go 0 0
  Matrix ra cb <$> V.unsafeFreeze result
{-# INLINE (<>) #-}

-- | Transpose
tr :: Matrix -> Matrix
tr m = runST $ do
  let !r = mRows m
      !c = mCols m
      !d = mData m
  result <- MV.new (r * c)
  let go i j
        | i >= r = return ()
        | j >= c = go (i+1) 0
        | otherwise = do
            MV.unsafeWrite result (j * r + i) (d `V.unsafeIndex` (i * c + j))
            go i (j+1)
  go 0 0
  Matrix c r <$> V.unsafeFreeze result

-- | Scalar multiplication
scale :: Double -> Matrix -> Matrix
scale s m = Matrix (mRows m) (mCols m) (V.map (* s) (mData m))

-- | Element-wise subtraction
mSub :: Matrix -> Matrix -> Matrix
mSub a b = Matrix (mRows a) (mCols a) (V.zipWith (-) (mData a) (mData b))

-- | Element-wise addition
mAdd :: Matrix -> Matrix -> Matrix
mAdd a b = Matrix (mRows a) (mCols a) (V.zipWith (+) (mData a) (mData b))

-- | Scalar divided by each element
mDiv :: Double -> Matrix -> Matrix
mDiv s m = Matrix (mRows m) (mCols m) (V.map (s /) (mData m))

-- | Construct diagonal matrix from a vector
diag :: Vector -> Matrix
diag = diagl

-- | Matrix inverse via Gaussian elimination with partial pivoting.
-- Uses mutable vectors in ST for O(n³) performance.
inv :: Matrix -> Matrix
inv m = runST $ do
  let !n = mRows m
      !d = mData m
  -- Build augmented matrix [A | I] as mutable vector
  aug <- MV.new (n * 2 * n)
  -- Fill left half with A, right half with I
  let idx i j = i * (2*n) + j
  sequence_ [MV.write aug (idx i j)
    (if j < n then d V.! (i*n+j)
              else if j - n == i then 1 else 0)
    | i <- [0..n-1], j <- [0..2*n-1]]
  -- Forward elimination with partial pivoting
  sequence_ $ flip map [0..n-1] $ \col -> do
    -- Find pivot
    pivotRef <- newSTRef col
    pivotVal <- newSTRef =<< (abs <$> MV.read aug (idx col col))
    sequence_ $ flip map [col+1..n-1] $ \row -> do
      v <- abs <$> MV.read aug (idx row col)
      pv <- readSTRef pivotVal
      if v > pv
        then writeSTRef pivotRef row >> writeSTRef pivotVal v
        else return ()
    pivot <- readSTRef pivotRef
    -- Swap rows
    if pivot /= col
      then sequence_ [do
        a <- MV.read aug (idx col j)
        b <- MV.read aug (idx pivot j)
        MV.write aug (idx col j) b
        MV.write aug (idx pivot j) a
        | j <- [0..2*n-1]]
      else return ()
    -- Normalize pivot row
    pv <- MV.read aug (idx col col)
    sequence_ [MV.modify aug (/ pv) (idx col j) | j <- [0..2*n-1]]
    -- Eliminate below
    sequence_ $ flip map [col+1..n-1] $ \row -> do
      factor <- MV.read aug (idx row col)
      sequence_ $ flip map [0..2*n-1] $ \j -> do
        pivotElem <- MV.read aug (idx col j)
        MV.modify aug (\x -> x - factor * pivotElem) (idx row j)
  -- Back substitution
  sequence_ $ flip map (reverse [0..n-1]) $ \col ->
    sequence_ $ flip map [0..col-1] $ \row -> do
      factor <- MV.read aug (idx row col)
      sequence_ $ flip map [0..2*n-1] $ \j -> do
        pivotElem <- MV.read aug (idx col j)
        MV.modify aug (\x -> x - factor * pivotElem) (idx row j)
  -- Extract right half
  result <- V.generateM (n*n) $ \k ->
    let (!i, !j) = k `divMod` n
    in MV.read aug (idx i (n+j))
  return $ Matrix n n result

-- | Determinant via Gaussian elimination with partial pivoting.
det :: Matrix -> Double
det m = runST $ do
  let !n = mRows m
      !d = mData m
  a <- MV.new (n * n)
  sequence_ [MV.write a (i*n+j) (d V.! (i*n+j)) | i <- [0..n-1], j <- [0..n-1]]
  signRef <- newSTRef (1.0 :: Double)
  -- Forward elimination
  sequence_ $ flip map [0..n-1] $ \col -> do
    -- Partial pivoting
    pivotRef <- newSTRef col
    pivotVal <- newSTRef =<< (abs <$> MV.read a (col*n+col))
    sequence_ $ flip map [col+1..n-1] $ \row -> do
      v <- abs <$> MV.read a (row*n+col)
      pv <- readSTRef pivotVal
      if v > pv then writeSTRef pivotRef row >> writeSTRef pivotVal v else return ()
    pivot <- readSTRef pivotRef
    if pivot /= col
      then do
        modifySTRef' signRef negate
        sequence_ [do { a_ <- MV.read a (col*n+j); b_ <- MV.read a (pivot*n+j);
                       MV.write a (col*n+j) b_; MV.write a (pivot*n+j) a_ } | j <- [0..n-1]]
      else return ()
    -- Eliminate below
    pv <- MV.read a (col*n+col)
    sequence_ $ flip map [col+1..n-1] $ \row -> do
      factor <- MV.read a (row*n+col)
      let f = factor / pv
      sequence_ $ flip map [col..n-1] $ \j -> do
        pivElem <- MV.read a (col*n+j)
        MV.modify a (\x -> x - f * pivElem) (row*n+j)
  -- Product of diagonal
  sign <- readSTRef signRef
  diags <- sequence [MV.read a (i*n+i) | i <- [0..n-1]]
  return $ sign * product diags

-- * LAPACK-backed fast operations (via hmatrix)
--
-- These convert the unboxed-vector representation to hmatrix (Storable) once
-- per call, run the LAPACK routine, and convert back. For n × n matrices with
-- n ≳ 20 they are orders of magnitude faster than the pure-Haskell Gaussian-
-- elimination versions above. Use these in hot loops (IRLS inner, Ξ grid
-- sweeps). The pure-Haskell versions remain for WASM compatibility.

-- Internal conversions.
toLA :: Matrix -> LA.Matrix Double
toLA (Matrix r c d) =
  (r LA.>< c) (V.toList d)

fromLA :: LA.Matrix Double -> Matrix
fromLA m =
  let r = LA.rows m
      c = LA.cols m
      xs = VS.toList (LA.flatten m)
  in  Matrix r c (V.fromList xs)

-- | LAPACK-backed matrix inverse.
invFast :: Matrix -> Matrix
invFast = fromLA . LA.inv . toLA

-- | LAPACK-backed determinant.
detFast :: Matrix -> Double
detFast = LA.det . toLA

-- | LAPACK-backed matrix multiplication (BLAS gemm).
mulFast :: Matrix -> Matrix -> Matrix
mulFast a b = fromLA (toLA a LA.<> toLA b)

-- | Moore-Penrose pseudoinverse via Gaussian elimination.
-- For square singular matrices (like Laplacians where rows sum to 0):
--   L⁺ = (L + eeᵀ/n)⁻¹ - eeᵀ/n
-- For full-rank rectangular: A⁺ = (AᵀA)⁻¹ Aᵀ.
pinv :: Matrix -> Matrix
pinv a =
  let (r, c) = size a
  in if r == c
       then let n = fromIntegral r
                jn = Matrix r c $ V.replicate (r*c) (1/n)
            in inv (a `mAdd` jn) `mSub` jn
       else let at = tr a
            in inv (at <> a) <> at

-- * Vector operations

mapVectorWithIndex :: (Int -> Double -> Double) -> Vector -> Vector
mapVectorWithIndex f = zipWith f [0..]
