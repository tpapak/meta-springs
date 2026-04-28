{-# LANGUAGE BangPatterns #-}
-- | Explicit spring-network representation of an LMM.
--
-- A node is β_j (j<p), u_k (j=p+k), y_i (pinned, value y_i), or 0 (pinned).
-- A spring has:
--   * a list of free-node coefficients
--   * a constant term from the pinned endpoints
--   * a hardness (stiffness)
--   * a label
--
-- Its *extension* at a candidate point x = (β, u) is
--
--     e(x) = Σ_j coef_j · x_j  +  const
--
-- and its energy is  ½ · hardness · e(x)².  Two kinds of springs arise:
--
--   observation spring for row i:
--       extension = y_i − X_i β − Z_i u
--       coefficients:  (β_j, −X_{i,j}), (u_k, −Z_{i,k})
--       const      :  +y_i
--       hardness   :  1/σ²_ε
--
--   shrinkage spring for random-effect k:
--       extension = u_k − 0
--       coefficients:  (u_k, +1)
--       const      :  0
--       hardness   :  1/τ²
--
-- Solving: differentiate the total energy w.r.t. each free node, set the
-- gradient to zero.  The resulting linear system A·x = b has
--
--     A_{j,j'} = Σ_e  k_e · c_{e,j} · c_{e,j'}
--     b_j     = −Σ_e  k_e · const_e · c_{e,j}
--
-- built by accumulating rank-1 updates PER SPRING.  Nothing else is done
-- with X and Z as dense matrices — every entry of A and b is a sum of
-- spring contributions, exactly as a physical spring network would give.
module Test.MixedModel.SpringNet
  ( NodeId(..)
  , SpringEdge(..)
  , SpringNetwork(..)
  , buildSpringNetwork
  , buildSpringNetworkVecRE
  , buildSpringNetworkIRLS
  , assembleSystem
  , solveSpringNetwork
  , solveSpringNetworkWithVar
  , summarizeNetwork
  ) where

import Prelude hiding ((<>))
import Data.List (foldl')
import qualified Numeric.LinearAlgebra as LA
import Numeric.LinearAlgebra (Vector, Matrix, (!), atIndex, size, cols)

-- | Named node types for diagnostics.
data NodeId
  = NBeta Int          -- ^ fixed-effect node β_j
  | NU    Int          -- ^ random-effect node u_k
  | NData Int          -- ^ pinned data anchor y_i
  | NZero              -- ^ pinned zero (shared target for shrinkage springs)
  deriving (Eq, Ord, Show)

-- | One spring = linear functional of free-node positions, plus a
-- constant (pinned endpoints).
data SpringEdge = SpringEdge
  { sLabel    :: String
  , sCoefs    :: [(Int, Double)]    -- ^ (free-node index, coefficient)
  , sConst    :: Double             -- ^ const term from pinned endpoints
  , sHardness :: Double
  , sEndpoints :: [NodeId]          -- ^ all endpoints (free + pinned) for diagnostics
  } deriving Show

-- | Full network: free-node count, which of those are β (first snPBeta of them),
-- and the springs.
data SpringNetwork = SpringNetwork
  { snNumFree  :: Int               -- ^ total free nodes = p + q
  , snPBeta    :: Int               -- ^ # β-nodes (indices 0 .. p-1)
  , snSprings  :: [SpringEdge]
  , snDataVals :: [(Int, Double)]   -- ^ pinned-anchor values (i, y_i) for inspection
  } deriving Show

-- | Build the spring network for an LMM at fixed (σ²_ε, τ²).
-- Only nonzero (X, Z) entries become coefficients — sparsity preserved.
buildSpringNetwork
  :: Vector Double   -- ^ y  (length n)
  -> Matrix Double   -- ^ X  (n × p)
  -> Matrix Double   -- ^ Z  (n × q)
  -> Double          -- ^ σ²_ε
  -> Double          -- ^ τ²
  -> SpringNetwork
buildSpringNetwork y x z sigma2E tau2 =
  let n     = size y
      p     = cols x
      q     = cols z
      kObs  = 1 / sigma2E
      kShr  = 1 / tau2

      obsSpring i =
        let yi   = y ! i
            bCs  = [ (j,      negate (x `atIndex` (i, j)))
                   | j <- [0 .. p - 1]
                   , abs (x `atIndex` (i, j)) > 0 ]
            uCs  = [ (p + k,  negate (z `atIndex` (i, k)))
                   | k <- [0 .. q - 1]
                   , abs (z `atIndex` (i, k)) > 0 ]
            eps  = [ NBeta j | (j,_) <- bCs ]
                ++ [ NU (k - p) | (k,_) <- uCs ]
                ++ [ NData i ]
        in SpringEdge
             { sLabel     = "obs_" ++ show i
             , sCoefs     = bCs ++ uCs
             , sConst     = yi
             , sHardness  = kObs
             , sEndpoints = eps
             }

      shrSpring k =
        SpringEdge
          { sLabel     = "shr_u" ++ show k
          , sCoefs     = [(p + k, 1.0)]
          , sConst     = 0
          , sHardness  = kShr
          , sEndpoints = [NU k, NZero]
          }

      obsSprs = [ obsSpring i | i <- [0 .. n - 1] ]
      shrSprs = [ shrSpring k | k <- [0 .. q - 1] ]
  in SpringNetwork
       { snNumFree  = p + q
       , snPBeta    = p
       , snSprings  = obsSprs ++ shrSprs
       , snDataVals = [ (i, y ! i) | i <- [0 .. n - 1] ]
       }

-- | Solve for the equilibrium positions of free nodes.
-- Assembles A and b by accumulating rank-1 contributions per spring,
-- then solves  A · x = b.
-- | Build the spring network for an LMM with vector random effects (tier 3)
-- at fixed (σ²_ε, Σ).  Σ is an r × r covariance matrix, shared across all J
-- groups; each group's shrinkage energy is ½ · u_jᵀ Σ⁻¹ u_j, decomposed into
-- **r self-springs + r(r−1)/2 cross-springs per group**:
--
--   self-spring on u_{g,i}          : hardness = (row sum of Σ⁻¹ row i)
--   cross-spring u_{g,i} ↔ u_{g,j}  : hardness = −(Σ⁻¹)_{i,j}       (i < j)
--
-- The decomposition is exact: total shrinkage energy equals ½ Σ_g u_gᵀΣ⁻¹u_g.
-- For positively correlated Σ (typical) the cross-spring hardness is positive
-- (since Σ⁻¹_{ij} < 0 for a 2×2 Σ with positive ρ).
buildSpringNetworkVecRE
  :: Vector Double   -- ^ y  (length n)
  -> Matrix Double   -- ^ X  (n × p)
  -> Matrix Double   -- ^ Z  (n × q = r·J)
  -> Int             -- ^ r = # random-effects per group
  -> Int             -- ^ J = # groups
  -> Matrix Double   -- ^ Σ  (r × r random-effects covariance)
  -> Double          -- ^ σ²_ε  (residual variance)
  -> SpringNetwork
buildSpringNetworkVecRE y x z r j sigma sigma2E =
  let n    = size y
      p    = cols x
      q    = cols z
      kObs = 1 / sigma2E
      si   = LA.inv sigma            -- Σ⁻¹
      rowSum i = sum [ si `atIndex` (i, c) | c <- [0 .. r - 1] ]
      selfK i        = rowSum i
      crossK i1 i2   = negate (si `atIndex` (i1, i2))

      obsSpring iRow =
        let yi  = y ! iRow
            bCs = [ (c, negate (x `atIndex` (iRow, c)))
                  | c <- [0 .. p - 1]
                  , abs (x `atIndex` (iRow, c)) > 0 ]
            uCs = [ (p + c, negate (z `atIndex` (iRow, c)))
                  | c <- [0 .. q - 1]
                  , abs (z `atIndex` (iRow, c)) > 0 ]
            eps = [ NBeta c | (c,_) <- bCs ]
               ++ [ NU (c - p) | (c,_) <- uCs ]
               ++ [ NData iRow ]
        in SpringEdge
             { sLabel     = "obs_" ++ show iRow
             , sCoefs     = bCs ++ uCs
             , sConst     = yi
             , sHardness  = kObs
             , sEndpoints = eps
             }

      -- r self-springs per group
      selfSpring g i =
        let idx = p + g * r + i
        in SpringEdge
             { sLabel     = "shr_self_g" ++ show g ++ "_i" ++ show i
             , sCoefs     = [(idx, 1.0)]
             , sConst     = 0
             , sHardness  = selfK i
             , sEndpoints = [NU (g * r + i), NZero]
             }
      -- r(r-1)/2 cross-springs per group
      crossSpring g i1 i2 =
        let idx1 = p + g * r + i1
            idx2 = p + g * r + i2
        in SpringEdge
             { sLabel     = "shr_cross_g" ++ show g
                          ++ "_i" ++ show i1 ++ "_j" ++ show i2
             , sCoefs     = [(idx1, 1.0), (idx2, -1.0)]
             , sConst     = 0
             , sHardness  = crossK i1 i2
             , sEndpoints = [NU (g * r + i1), NU (g * r + i2)]
             }

      obsSprs   = [ obsSpring iRow | iRow <- [0 .. n - 1] ]
      selfSprs  = [ selfSpring   g i           | g <- [0 .. j - 1], i <- [0 .. r - 1] ]
      crossSprs = [ crossSpring  g i1 i2
                  | g <- [0 .. j - 1]
                  , i1 <- [0 .. r - 1]
                  , i2 <- [i1 + 1 .. r - 1]
                  ]
  in SpringNetwork
       { snNumFree  = p + q
       , snPBeta    = p
       , snSprings  = obsSprs ++ selfSprs ++ crossSprs
       , snDataVals = [ (i, y ! i) | i <- [0 .. n - 1] ]
       }

-- | Build the spring network for one IRLS iteration of a logistic GLMM
-- with single grouping (scalar τ²).
--
-- Observation springs have **per-observation hardness** w_i = p_i(1−p_i)
-- (Fisher weights) and **working response** z̃_i = η_i + (y_i − p_i)/w_i as
-- the pinned target.  Shrinkage springs have uniform hardness 1/τ², no
-- residual-variance scaling (Bernoulli has no σ²_ε).
buildSpringNetworkIRLS
  :: Vector Double   -- ^ working response z̃  (length n)
  -> Matrix Double   -- ^ X  (n × p)
  -> Matrix Double   -- ^ Z  (n × q)
  -> Vector Double   -- ^ w  per-obs IRLS weights (length n)
  -> Double          -- ^ τ² (shrinkage hardness = 1/τ²)
  -> SpringNetwork
buildSpringNetworkIRLS zw x z w tau2 =
  let n    = size zw
      p    = cols x
      q    = cols z
      kShr = 1 / tau2

      obsSpring iRow =
        let bCs = [ (c, negate (x `atIndex` (iRow, c)))
                  | c <- [0 .. p - 1]
                  , abs (x `atIndex` (iRow, c)) > 0 ]
            uCs = [ (p + c, negate (z `atIndex` (iRow, c)))
                  | c <- [0 .. q - 1]
                  , abs (z `atIndex` (iRow, c)) > 0 ]
            eps = [ NBeta c | (c,_) <- bCs ]
               ++ [ NU (c - p) | (c,_) <- uCs ]
               ++ [ NData iRow ]
        in SpringEdge
             { sLabel     = "obs_" ++ show iRow
             , sCoefs     = bCs ++ uCs
             , sConst     = zw ! iRow
             , sHardness  = w  ! iRow
             , sEndpoints = eps
             }

      shrSpring k =
        SpringEdge
          { sLabel     = "shr_u" ++ show k
          , sCoefs     = [(p + k, 1.0)]
          , sConst     = 0
          , sHardness  = kShr
          , sEndpoints = [NU k, NZero]
          }
      obsSprs = [ obsSpring i | i <- [0 .. n - 1] ]
      shrSprs = [ shrSpring k | k <- [0 .. q - 1] ]
  in SpringNetwork
       { snNumFree  = p + q
       , snPBeta    = p
       , snSprings  = obsSprs ++ shrSprs
       , snDataVals = [ (i, zw ! i) | i <- [0 .. n - 1] ]
       }

-- | Assemble the linear system A·x = b implied by the springs.
-- A accumulates rank-1 outer products; b accumulates −k·const·coef forcing.
assembleSystem :: SpringNetwork -> (Matrix Double, Vector Double)
assembleSystem net =
  let m   = snNumFree net
      a0  = LA.konst 0 (m, m) :: Matrix Double
      b0  = LA.konst 0 m      :: Vector Double
      stepA !a spr =
        let k  = sHardness spr
            cs = sCoefs spr
            aU = [ ((i, j), k * ci * cj) | (i, ci) <- cs, (j, cj) <- cs ]
        in LA.accum a (+) aU :: Matrix Double
      stepB !b spr =
        let k  = sHardness spr
            cs = sCoefs spr
            c  = sConst spr
            bU = [ (i, negate (k * c * ci)) | (i, ci) <- cs ]
        in LA.accum b (+) bU :: Vector Double
      a = foldl' stepA a0 (snSprings net)
      b = foldl' stepB b0 (snSprings net)
  in (a, b)

-- | Solve for equilibrium node positions.
solveSpringNetwork :: SpringNetwork -> Vector Double
solveSpringNetwork net =
  let (a, b) = assembleSystem net
  in LA.flatten (a LA.<\> LA.asColumn b)

-- | Solve AND return the u-block of A⁻¹ (which equals σ²_ε · (full⁻¹)_uu,
-- i.e. the REML posterior variance Var(û | y) scaled for use in the EM
-- moment-match).
solveSpringNetworkWithVar :: SpringNetwork -> (Vector Double, Matrix Double)
solveSpringNetworkWithVar net =
  let (a, b) = assembleSystem net
      x    = LA.flatten (a LA.<\> LA.asColumn b)
      aInv = LA.inv a
      p    = snPBeta net
      q    = snNumFree net - p
      varU = LA.subMatrix (p, p) (q, q) aInv
  in (x, varU)

-- | Compact human-readable summary of the network.
summarizeNetwork :: SpringNetwork -> String
summarizeNetwork net =
  let obs  = [ s | s <- snSprings net, take 4 (sLabel s) == "obs_" ]
      shr  = [ s | s <- snSprings net, take 4 (sLabel s) == "shr_" ]
      nObs = length obs
      nShr = length shr
      hObs = if null obs then 0 else sHardness (head obs)
      hShr = if null shr then 0 else sHardness (head shr)
      p    = snPBeta net
      q    = snNumFree net - p
      n    = length (snDataVals net)
  in unlines
       [ "  Spring-network summary"
       , "    free nodes:     " ++ show (snNumFree net)
                                ++ "   (" ++ show p ++ " β,  " ++ show q ++ " u)"
       , "    pinned anchors: " ++ show n ++ " data + 1 zero"
       , "    obs springs:    " ++ show nObs ++ "   hardness 1/σ²_ε = " ++ show hObs
       , "    shr springs:    " ++ show nShr ++ "   hardness 1/τ²  = " ++ show hShr
       , "    total springs:  " ++ show (nObs + nShr)
       ]
