{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE BangPatterns #-}

-- |
-- Module      : Data.Meta.NMA
-- Description : Second stage NMA
-- Copyright   : (c) Thodoris Papakonstantinou, 2019
-- License     : GPL-3
-- Maintainer  : mail@tpapak.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Given a study graph of direct effects perform network-meta analysis
module Data.Meta.NMA
  ( NetworkEffects (..),
    Spring (..),
    SpringNetwork (..),
    SpringVertex (..),
    NaturalLength (..),
    TauSquare (..),
    TauGroup,
    TauId (..),
    makeSprings,
    makeBinomialSprings,
    nmaRegression,
    nmaRegressionBin,
    nmaRegressionBinRE,
    nmaRegressionBinGroupedREML,
    nmaRegressionBinPosteriorB,
    nmaRegressionBaselineRiskBin,
    nmaRegressionBaselineRiskBinRE,
    nmaRegressionBaselineRiskBinB,
    nmaRegressionBaselineRiskMD,
    nmaRegressionBaselineRiskMDB,
    nmaRegressionBaselineRiskBinExchGC,
    BaselineRiskFit (..),
    BaselineExchGC (..),
    BaselineSlopeModel (..),
    linearizeBinaryRegression,
    nmaRegressionRE,
    nmaRegressionPosterior,
    nmaRegressionPosteriorB,
    nmaRegressionGroupedREML,
    RegPosterior (..),
    RegPosteriorB (..),
    BetaModel (..),
    Tau2PriorR (..),
    nma,
    springNMA,
    springLength,
    naturalLength,
    hardness,
    springEnergy,
    springForce,
    springHardness,
    sigmoid,
    linearize,
    linearizeNetwork,
    updateSprings,
    updateSpringsGrouped,
    setTauGroups,
    removeStudy,
    netStudies
  )
where

import Control.Applicative
import Data.Either
import qualified Data.Graph.AdjacencyList as G
import qualified Data.Graph.AdjacencyList.BFS as BFS
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List
import qualified Data.Map.Strict as Map
import Data.Maybe
import Prelude hiding ((<>))
import Data.Meta.Effects
import Data.Meta.Matrices
import Data.Meta.Matrix
import Data.Meta.Studies
import qualified Data.Set as Set
import Data.Tuple


type NaturalLength = Double

type Hardness = Double

type SpringLength = Double

data Spring = Spring Hardness NaturalLength SpringLength
            | BinomialSpring Int Int SpringLength  -- ^ events, sampleSize, springLength
  deriving (Show, Eq)

sigmoid :: Double -> Double
sigmoid x
  | x >= 0   = let e = exp (-x) in 1 / (1 + e)
  | otherwise = let e = exp x    in e / (1 + e)

reverseSpring :: Spring -> Spring
reverseSpring (Spring k ll l) = Spring k (-ll) (-l)
reverseSpring (BinomialSpring e n l) = BinomialSpring e n (-l)

-- | k_arm: binomial hardness at arm position ψ
kArm :: Int -> Double -> Double
kArm n psi = let s = sigmoid psi in fromIntegral n * s * (1 - s)

hardness :: Spring -> Hardness
hardness (Spring k _ _) = k
hardness (BinomialSpring _ n l) =
  let s = sigmoid l
   in fromIntegral n * s * (1 - s)

naturalLength :: Spring -> NaturalLength
naturalLength (Spring _ ll _) = ll
naturalLength (BinomialSpring e n l) =
  let s = sigmoid l
      k = fromIntegral n * s * (1 - s)
      f = fromIntegral e - fromIntegral n * s
   in if k > 1e-15 then l + f / k else l

springLength :: Spring -> SpringLength
springLength (Spring _ _ l) = l
springLength (BinomialSpring _ _ l) = l

updateLength :: Spring -> SpringLength -> Spring
updateLength (Spring k ll _) l' = Spring k ll l'
updateLength (BinomialSpring e n _) l' = BinomialSpring e n l'

updateHardness :: Spring -> TauSquare -> Spring
updateHardness (Spring _ ll l) tau = Spring (1/tau) ll l
updateHardness s@(BinomialSpring _ _ _) _ = s

springEnergy :: Spring -> Double
springEnergy (Spring k ll l) = (1/2) * k * (l - ll)^2
springEnergy (BinomialSpring e n l) =
  let e' = fromIntegral e
      n' = fromIntegral n
   in n' * log (1 + exp l) - e' * l

springForce :: Spring -> Double
springForce (Spring k ll l) = -(k * (l - ll))
springForce (BinomialSpring e n l) = fromIntegral e - fromIntegral n * sigmoid l

springHardness :: Spring -> Hardness
springHardness = hardness

-- | Linearize a non-linear spring at its current position into a Gaussian spring.
-- Clamps hardness and working response to avoid divergence at extreme positions.
linearize :: Spring -> Spring
linearize s@(Spring _ _ _) = s
linearize s@(BinomialSpring _ n l) =
  let n' = fromIntegral n
      kRaw = hardness s
      kMin = max 1e-6 (n' * 1e-10)
      k = max kMin kRaw
      fk = springForce s / k
      fkClamped = max (-5.0) (min 5.0 fk)
      ll = l + fkClamped
   in Spring k ll l

-- | Linearize all springs in a network
linearizeNetwork :: SpringNetwork -> SpringNetwork
linearizeNetwork net = net { springs = Map.map linearize (springs net) }

type TauSquare = Double

-- | Tau-group identifier for per-group heterogeneity (τ²) estimation.
-- Default group is 0; classification functions assign edges to groups.
type TauGroup = Int

data TauId = TauId StudyId TreatmentId
  deriving (Show, Eq, Ord)

data SpringVertex = StudyEnd StudyId
                  | TreatmentEnd TreatmentId
                  | TauEnd TauId
  deriving (Show, Eq, Ord)

getTreatmentId :: SpringVertex -> Maybe TreatmentId
getTreatmentId (TreatmentEnd tid) = Just tid
getTreatmentId _ = Nothing

isTreatmentEnd :: SpringVertex -> Bool
isTreatmentEnd (TreatmentEnd _)  = True
isTreatmentEnd _  = False

isStudyEnd :: SpringVertex -> Bool
isStudyEnd (StudyEnd _)  = True
isStudyEnd _  = False

data SpringNetwork =
  SpringNetwork { vs2ts :: IM.IntMap SpringVertex
                , springs :: Map.Map G.Edge Spring
                , pinned :: Maybe G.Vertex
                , tauEdges :: Set.Set G.Edge
                , tauGroupMap :: Map.Map G.Edge TauGroup -- tau edge → group
                , tau2 :: TauSquare -- default / group 0 τ²
                , tau2s :: Map.Map TauGroup TauSquare -- τ² per group
                , qqTau :: Double -- Tau springs energy Qτ=2Uτ
                , qqTau' :: Double -- kτ derivative of Qτ
                , designs :: IM.IntMap Int -- Studies per design
                , dof :: Int -- degrees of freedom
  }
  deriving (Show, Eq)

-- | study ids in the network
netStudies :: SpringNetwork -> [Int]
netStudies springsnet =
  IM.keys $ IM.filter isStudyEnd $ vs2ts springsnet

-- | get dofs from vertices and designs of spring network
degreesOfFreedom ::  IM.IntMap SpringVertex-> IM.IntMap Int -> Int
degreesOfFreedom vs2ts dess =
  let ntreats = length $ IM.filter isTreatmentEnd vs2ts 
      nstudies = length $ IM.filter isStudyEnd vs2ts
      !narms = IM.foldrWithKey (\k nk ac -> ac + k * nk) 0 dess
   in narms - nstudies - (ntreats - 1)

data NetworkEffects = NetworkEffects
  { directs :: StudyGraph ,
    networkEstimates :: Map.Map TreatmentId (Map.Map TreatmentId Double),
    networkVariances :: Map.Map TreatmentId (Map.Map TreatmentId Double),
    studyDeltaVariances :: IM.IntMap [Double], -- ^ Var(δ_ij) per arm, per study vertex
    springNetwork :: SpringNetwork,
    debugmsg :: String
  }
  deriving (Show, Eq)

-- | Matrices needed for solving the linear system
data Matrices = Matrices
  { kMatrix :: Matrix, -- hardnesses 1/wi E><E matrix
    llMatrix :: [Double], -- Natural lengths
    cMatrix :: Matrix, -- Vertex-Edge Adjacency matrix (V-1)><E
    dMatrix :: Matrix, -- C><K
    xMatrix :: Matrix, -- Spanning tree matrix (V-1)><E
    dxInv :: Matrix, -- (DX)^{-1} cached for reuse
    spnTr :: Set.Set G.Edge,
    parents :: IM.IntMap G.Vertex,
    vss :: [G.Vertex],
    directEdges :: Set.Set G.Edge,
    lstar :: Matrix, -- Solution l* = (DX)^{-1} D L
    ls :: Matrix -- ls = X l*
  }

-- | makeSprings gets list of studies, the pinned vertex tau2 and effect
-- calculator and outputs the SpringNetwork
makeSprings ::
  (ArmEffect a b, Gaussian a, Gaussian b) =>
  [Study] ->
  Maybe G.Vertex ->
  Maybe TauSquare ->
  (Arm -> Either String a) ->
  Either String SpringNetwork
makeSprings studies mpinnedVertex mtau2' armEffect =
  let mtau2 = case mtau2' of 
               Just 0 -> Nothing
               Nothing -> Nothing
               Just t -> if t < 0 
                            then Nothing
                            else Just t
      !(designs', vs) =
        foldl'
          ( \(des, acvs) study ->
              let sid = getStudyId study
                  arms = getStudyArms study
                  narms = length arms
                  des' = case IM.lookup narms des of
                    Nothing -> IM.insert narms 1 des
                    Just ns -> IM.insert narms (ns+1) des
                  labels = case mtau2 of
                    Nothing ->
                      Set.insert (StudyEnd  sid) $
                        Set.union acvs $
                          Set.fromList $ map (TreatmentEnd . tidOfArm) arms
                    Just tau2 ->
                      let tauvs =
                            Set.fromList $
                              map
                                ( \a ->
                                    (TauEnd (TauId sid (tidOfArm a)))
                                )
                                arms
                          tidvs = Set.fromList $ map (TreatmentEnd . tidOfArm) arms
                       in Set.insert (StudyEnd sid) $ Set.union acvs $ Set.union tauvs tidvs
               in (des', labels)
             )
          (IM.empty, Set.empty)
          studies :: (IM.IntMap Int, Set.Set SpringVertex)
      vssts = IM.fromList $ zip [1 ..] $ Set.toList vs
      stsvs = Map.fromList $ map swap $ IM.toList vssts
      esprings =
        foldl'
          ( \eaces study ->
              let sid = getStudyId study
                  arms = getStudyArms study
                  studysprings = sequence $ case armEffect $ head arms of
                    Left err -> [Left err]
                    Right aArm ->
                      foldl'
                        ( \acc a -> case armEffect a of
                            Left err -> Left err : acc
                            Right armef ->
                              let tid = tidOfArm a
                                  vs = stsvs Map.! StudyEnd sid
                                  vt = stsvs Map.! TreatmentEnd tid
                                  vtau = stsvs Map.! (TauEnd (TauId sid tid))
                                  eff = relatedRelative armef
                                  nl = expectation eff
                                  k = 1 / variance eff
                                  spr = Spring k nl nl
                               in case mtau2 of
                                    Nothing ->
                                      let sttrEdge = G.Edge vs vt
                                       in Right (Right sttrEdge, spr) : acc
                                    Just tau2 ->
                                      let sttauEdge = G.Edge vs vtau
                                          sttauspr = Spring (2 / tau2) 0 0 -- That's where tau2 is inserted
                                          tautrEdge = G.Edge vtau vt
                                       in [ Right (Left sttauEdge, sttauspr),
                                            Right (Right tautrEdge, spr)
                                          ]
                                            ++ acc
                        )
                        []
                        arms ::
                        [Either String (Either G.Edge G.Edge, Spring)]
               in case eaces of
                    Left err -> Left err
                    Right aces ->
                      case studysprings of
                        Left err -> Left err
                        Right sprs -> Right (sprs ++ aces)
          )
          (Right [])
          studies ::
          Either String [(Either G.Edge G.Edge, Spring)]
  in case esprings of
    Left err -> Left err
    Right springs' ->
      let tauEdges = foldl' (\acc (ee, spr) -> 
                       case ee of 
                         Right _ -> acc
                         Left e -> Set.insert e acc
                     ) Set.empty springs'
          springs = map (\(ee, spr) ->
             case ee of 
               Right e -> (e, spr)
               Left e -> (e, spr)
               ) springs'
       in Right SpringNetwork { springs = Map.fromList springs
                              , vs2ts = vssts
                              , pinned = mpinnedVertex
                              , tauEdges = tauEdges
                              , tauGroupMap = Map.fromSet (const 0) tauEdges
                              , tau2 = fromMaybe 0.0 mtau2
                              , tau2s = Map.singleton 0 (fromMaybe 0.0 mtau2)
                              , qqTau = 0
                              , qqTau' = 0
                              , designs = designs'
                              , dof = degreesOfFreedom vssts designs'
                              }

-- | Fixed-effect network meta-regression: extends the makeSprings model with a
-- shared covariate slope β. Same convention as makeSprings — an arm's effect
-- equals (φ_T − φ_S), now plus β·x_i for non-reference arms (the covariate shifts
-- the treatment end). Solved as arm-level weighted least squares (the spring
-- equilibrium with the β coordinate), reference treatment pinned (d_ref = 0).
-- Returns (β, contrasts d_t−d_s at covariate x = 0).
nmaRegression ::
  (ArmEffect a b, Gaussian a, Gaussian b) =>
  [Study] ->
  Map.Map StudyId Double ->          -- ^ study-level covariate x_i
  (Arm -> Either String a) ->
  Either String (Double, Map.Map TreatmentId (Map.Map TreatmentId Double))
nmaRegression studies covmap armEffect =
  let treatments = sort $ nub $ concatMap (map tidOfArm . getStudyArms) studies
      ref        = head treatments
      nonref     = filter (/= ref) treatments
      nT         = length nonref
      sids       = map getStudyId studies
      nS         = length sids
      sidx       = Map.fromList (zip sids [0 ..])
      tidx       = Map.fromList (zip nonref [0 ..])
      nP         = nS + nT + 1          -- μ per study, d per non-ref treatment, β
      betaCol    = nS + nT
      setAt xs i v = take i xs ++ [v] ++ drop (i + 1) xs
      armRow study a = do
        ef <- armEffect a
        let rel = relatedRelative ef
            y   = expectation rel
            w   = 1 / variance rel
            t   = tidOfArm a
            si  = sidx Map.! getStudyId study
            x   = Map.findWithDefault 0 (getStudyId study) covmap
            base = setAt (replicate nP 0) si (-1)         -- φ_S coefficient −1
            row  = if t == ref
                     then base
                     else setAt (setAt base (nS + tidx Map.! t) 1) betaCol x
        return (row, y, w)
   in do
        rows <- concat <$> mapM (\study -> mapM (armRow study) (getStudyArms study)) studies
        let aMat = [ [ sum [ w * (r !! i) * (r !! j) | (r, _, w) <- rows ] | j <- [0 .. nP - 1] ]
                   | i <- [0 .. nP - 1] ]
            bVec = [ sum [ w * (r !! i) * y | (r, y, w) <- rows ] | i <- [0 .. nP - 1] ]
            theta = gaussSolve aMat bVec
            beta  = theta !! betaCol
            dOf t = if t == ref then 0 else theta !! (nS + tidx Map.! t)
            contrasts = Map.fromList
              [ (t1, Map.fromList [ (t2, dOf t2 - dOf t1) | t2 <- treatments ])
              | t1 <- treatments ]
        return (beta, contrasts)

-- | Random-effects network meta-regression: REML/EM estimate of τ² jointly with the
-- covariate slope β, on the same model as @nmaRegression@ (arm effect = φ_T − φ_S
-- + β·x for non-reference arms) plus the heterogeneity τ². Matches the springREML
-- convention exactly: reported τ² is contrast-space, arm-space is τ²/2, effective arm
-- variance v + τ²/2, BLUP shrinkage b = w·τ²ₐᵣₘ/(1+w·τ²ₐᵣₘ), Var(δ)=b²·varST + b·τ²ₐᵣₘ,
-- and EM update τ²_new = 2·(mean δ² + mean Var δ). Returns (β, τ², contrasts at x=0).
nmaRegressionRE ::
  (ArmEffect a b, Gaussian a, Gaussian b) =>
  [Study] ->
  Map.Map StudyId Double ->
  Maybe TauSquare ->                 -- ^ initial τ² guess
  (Arm -> Either String a) ->
  Either String (Double, Double, Map.Map TreatmentId (Map.Map TreatmentId Double))
nmaRegressionRE studies covmap mtau0 armEffect =
  let treatments = sort $ nub $ concatMap (map tidOfArm . getStudyArms) studies
      ref        = head treatments
      nonref     = filter (/= ref) treatments
      nT         = length nonref
      sids       = map getStudyId studies
      nS         = length sids
      sidx       = Map.fromList (zip sids [0 ..])
      tidx       = Map.fromList (zip nonref [0 ..])
      -- β is identified only if the covariate varies across the non-reference arms;
      -- otherwise drop the column (β unidentified) and report β = 0.
      nonrefCovs = [ Map.findWithDefault 0 (getStudyId s) covmap
                   | s <- studies, a <- getStudyArms s, tidOfArm a /= ref ]
      hasBeta    = length (nub nonrefCovs) > 1
      nP         = nS + nT + (if hasBeta then 1 else 0)
      betaCol    = nS + nT
      setAt xs i v = take i xs ++ [v] ++ drop (i + 1) xs
      armInfo study a = do
        ef <- armEffect a
        let rel = relatedRelative ef
            y   = expectation rel
            v   = variance rel
            t   = tidOfArm a
            si  = sidx Map.! getStudyId study
            x   = Map.findWithDefault 0 (getStudyId study) covmap
            base = setAt (replicate nP 0) si (-1)
            row  | t == ref  = base
                 | hasBeta   = setAt (setAt base (nS + tidx Map.! t) 1) betaCol x
                 | otherwise = setAt base (nS + tidx Map.! t) 1
        return (row, y, v)
   in do
        arms <- concat <$> mapM (\study -> mapM (armInfo study) (getStudyArms study)) studies
        let nArm = fromIntegral (length arms)
            solveTheta t2 =                 -- GLS at τ²; returns (theta, cov)
              let t2arm = t2 / 2
                  aMat = [ [ sum [ (r !! i) * (r !! j) / (v + t2arm) | (r, _, v) <- arms ] | j <- [0 .. nP - 1] ]
                         | i <- [0 .. nP - 1] ]
                  bVec = [ sum [ (r !! i) * y / (v + t2arm) | (r, y, v) <- arms ] | i <- [0 .. nP - 1] ]
                  cov  = matInv aMat
              in (matVec cov bVec, cov)
            -- One REML/EM step τ² → τ²_new, matching springREML exactly:
            --   τ²_new = 2·(mean δ̂² + mean Var(δ̂))   (contrast-space ⇒ factor 2)
            -- with δ̂ᵢ = bᵢ·residᵢ the per-arm BLUP, bᵢ = wᵢ·τ²ₐᵣₘ/(1+wᵢ·τ²ₐᵣₘ),
            -- and posterior Var(δ̂ᵢ|y) = (1−bᵢ)·τ²ₐᵣₘ + bᵢ²·Var(fittedᵢ).
            emStep t2 =
              let t2arm = t2 / 2
                  (theta, cov) = solveTheta t2
                  per (r, y, v) =
                    let fitted = sum (zipWith (*) r theta)
                        resid  = y - fitted
                        b      = (1 / v) * t2arm / (1 + (1 / v) * t2arm)   -- shrinkage
                        delta  = b * resid
                        varST  = quadForm r cov                            -- Var(fitted)
                        varD   = (1 - b) * t2arm + b * b * varST
                    in (delta * delta, varD)
                  ms = map per arms
              in max 0 $ 2 * (sum (map fst ms) / nArm + sum (map snd ms) / nArm)
            emLoop t2 it
              | it > (300 :: Int) = t2
              | abs (t2' - t2) < 1e-6 = t2'
              | otherwise = emLoop t2' (it + 1)
              where t2' = emStep t2
            tau2 = emLoop (fromMaybe 0.5 mtau0) 0
            (theta, _) = solveTheta tau2
            beta = if hasBeta then theta !! betaCol else 0
            dOf t = if t == ref then 0 else theta !! (nS + tidx Map.! t)
            contrasts = Map.fromList
              [ (t1, Map.fromList [ (t2, dOf t2 - dOf t1) | t2 <- treatments ])
              | t1 <- treatments ]
        return (beta, tau2, contrasts)

matVec :: [[Double]] -> [Double] -> [Double]
matVec m v = [ sum (zipWith (*) row v) | row <- m ]

quadForm :: [Double] -> [[Double]] -> Double      -- aᵀ M a
quadForm a m = sum [ (a !! i) * (m !! i !! j) * (a !! j) | i <- [0 .. n - 1], j <- [0 .. n - 1] ]
  where n = length a

matInv :: [[Double]] -> [[Double]]                -- inverse via column solves
matInv a = transpose [ gaussSolve a (unit j) | j <- [0 .. n - 1] ]
  where n = length a
        unit j = [ if i == j then 1 else 0 | i <- [0 .. n - 1] ]

-- | Solve A x = b by Gaussian elimination with partial pivoting.
gaussSolve :: [[Double]] -> [Double] -> [Double]
gaussSolve a0 b0 = back (elim (zipWith (\r y -> r ++ [y]) a0 b0) 0)
  where
    n = length b0
    elim m c
      | c >= n = m
      | otherwise =
          let piv = maximumBy (\i j -> compare (abs (m !! i !! c)) (abs (m !! j !! c))) [c .. n - 1]
              m1  = swap' c piv m
              prow = m1 !! c
              m2  = [ if r <= c then m1 !! r
                      else let f = (m1 !! r !! c) / (prow !! c)
                           in zipWith (\u v -> u - f * v) (m1 !! r) prow
                    | r <- [0 .. n - 1] ]
          in elim m2 (c + 1)
    swap' i j m = [ if k == i then m !! j else if k == j then m !! i else m !! k | k <- [0 .. n - 1] ]
    back m = foldr step (replicate n 0) [0 .. n - 1]
      where step i xs = let row = m !! i
                            s = sum [ (row !! j) * (xs !! j) | j <- [i + 1 .. n - 1] ]
                        in setAtL xs i ((row !! n - s) / (row !! i))
    setAtL xs i v = take i xs ++ [v] ++ drop (i + 1) xs

-- | log determinant of a (symmetric positive-definite) matrix via Gaussian
-- elimination with partial pivoting (sum of log|pivot|).
logDetPD :: [[Double]] -> Double
logDetPD a0 = go a0 0 0
  where
    n = length a0
    go m c acc
      | c >= n = acc
      | otherwise =
          let piv = maximumBy (\i j -> compare (abs (m !! i !! c)) (abs (m !! j !! c))) [c .. n - 1]
              m1  = [ if k == c then m !! piv else if k == piv then m !! c else m !! k | k <- [0 .. n - 1] ]
              prow = m1 !! c
              p    = prow !! c
              m2  = [ if r <= c then m1 !! r
                      else let f = (m1 !! r !! c) / p
                           in zipWith (\u v -> u - f * v) (m1 !! r) prow
                    | r <- [0 .. n - 1] ]
          in go m2 (c + 1) (acc + log (abs p))

-- | Standard normal CDF (Abramowitz–Stegun 7.1.26 erf approximation).
normCdf :: Double -> Double
normCdf x = 0.5 * (1 + erf (x / sqrt 2))
  where
    erf z = let s = signum z; az = abs z
                t = 1 / (1 + 0.3275911 * az)
                y = 1 - (((((1.061405429 * t - 1.453152027) * t) + 1.421413741) * t - 0.284496736) * t + 0.254829592) * t * exp (- az * az)
            in s * y

-- | Weighted 95% interval of a sorted-ascending grid (trapezoid CDF).
weightedCI :: [Double] -> [Double] -> (Double, Double)
weightedCI ts ws = (q 0.025, q 0.975)
  where cums = scanl1 (+) ws
        q p  = let i = length (takeWhile (< p) cums)
               in ts !! min i (length ts - 1)

-- | 2.5/97.5% quantiles of a Gaussian mixture Σ wₖ N(mₖ, vₖ) by bisection.
mixtureCI :: [Double] -> [Double] -> [Double] -> (Double, Double)
mixtureCI ws ms vs = (solve 0.025, solve 0.975)
  where cdf b = sum [ w * normCdf ((b - m) / sqrt (max 1e-300 v)) | (w, m, v) <- zip3 ws ms vs ]
        mn = minimum ms; mx = maximum ms; sd = sqrt (maximum (1e-12 : vs))
        solve p = bis (mn - 12 * sd) (mx + 12 * sd) (60 :: Int)
          where bis lo hi k
                  | k <= 0 = (lo + hi) / 2
                  | cdf mid < p = bis mid hi (k - 1)
                  | otherwise   = bis lo mid (k - 1)
                  where mid = (lo + hi) / 2

-- | Joint (β, τ²) posterior summary for network meta-regression.
data RegPosterior = RegPosterior
  { rpBeta :: (Double, Double, Double, Double)  -- ^ (mean, sd, lo95, hi95)
  , rpTau2 :: (Double, Double, Double, Double)  -- ^ (mode, mean, lo95, hi95)
  , rpGrid :: [(Double, Double, Double, Double)] -- ^ (τ², weight, β̂(τ²), Var(β|τ²))
  } deriving (Show)

-- | Heterogeneity prior for the joint posterior. @FlatT2@ = improper flat on τ²
-- (REML evidence; posterior mode = the REML point). @HalfNormalTauR σ@ = a
-- half-normal prior with scale σ on the heterogeneity SD τ (matches gemtc/multinma
-- conventions), transformed to the τ² scale.
data Tau2PriorR = FlatT2 | HalfNormalTauR Double | UniformSD Double
  deriving (Show, Eq)

-- | log prior density on τ² (Lebesgue scale, up to a constant).
logPriorT2 :: Tau2PriorR -> Double -> Double
logPriorT2 FlatT2 _ = 0
logPriorT2 (HalfNormalTauR s) t          -- τ ~ HalfNormal(s) ⇒ p(τ²) ∝ exp(−τ²/2s²)/√τ²
  | t <= 0    = -1e300
  | otherwise = - t / (2 * s * s) - 0.5 * log t
logPriorT2 (UniformSD u) t               -- τ ~ Uniform(0,u) ⇒ p(τ²) ∝ 1/√τ² on (0,u²)
  | t <= 0 || t >= u * u = -1e300
  | otherwise            = -0.5 * log t

-- | Joint (β, τ²) posterior by exact linear-Gaussian marginalisation: at each τ²
-- the fixed effects (μ, d, β) are integrated analytically, so the τ² posterior
-- ∝ exp(REML logLik)·prior(τ²) and β|τ² is Gaussian. β's marginal posterior is
-- therefore a τ²-mixture of Gaussians — a genuine joint posterior, not a profile.
-- (β is integrated, never profiled out.) The prior is on τ² (see 'Tau2PriorR').
nmaRegressionPosterior ::
  (ArmEffect a b, Gaussian a, Gaussian b) =>
  [Study] ->
  Map.Map StudyId Double ->
  Int ->                                   -- ^ τ² grid resolution (e.g. 200)
  Tau2PriorR ->                            -- ^ prior on the heterogeneity
  (Arm -> Either String a) ->
  Either String RegPosterior
nmaRegressionPosterior studies covmap nGrid prior armEffect =
  let treatments = sort $ nub $ concatMap (map tidOfArm . getStudyArms) studies
      ref        = head treatments
      nonref     = filter (/= ref) treatments
      nT         = length nonref
      sids       = map getStudyId studies
      nS         = length sids
      sidx       = Map.fromList (zip sids [0 ..])
      tidx       = Map.fromList (zip nonref [0 ..])
      nonrefCovs = [ Map.findWithDefault 0 (getStudyId s) covmap
                   | s <- studies, a <- getStudyArms s, tidOfArm a /= ref ]
      hasBeta    = length (nub nonrefCovs) > 1
      nP         = nS + nT + 1
      betaCol    = nS + nT
      setAt xs i v = take i xs ++ [v] ++ drop (i + 1) xs
      armInfo study a = do
        ef <- armEffect a
        let rel = relatedRelative ef
            y = expectation rel; v = variance rel; t = tidOfArm a
            si = sidx Map.! getStudyId study
            x  = Map.findWithDefault 0 (getStudyId study) covmap
            base = setAt (replicate nP 0) si (-1)
            row  = if t == ref then base
                     else setAt (setAt base (nS + tidx Map.! t) 1) betaCol x
        return (row, y, v)
   in if not hasBeta then Left "nmaRegressionPosterior: covariate does not vary; β unidentified" else do
        arms <- concat <$> mapM (\study -> mapM (armInfo study) (getStudyArms study)) studies
        (_, tau2mode0, _) <- nmaRegressionRE studies covmap (Just 0.1) armEffect
        let tau2hi = max (tau2mode0 * 12) 0.02
            dt     = tau2hi / fromIntegral nGrid
            -- midpoint grid (no τ²=0 node, so a τ-prior Jacobian stays finite)
            ts     = [ tau2hi * (fromIntegral i - 0.5) / fromIntegral nGrid | i <- [1 .. nGrid] ]
            -- GC partition function at τ²: the Gaussian integral over the free
            -- coordinates (μ, d, β). logZ = −½(Σlog(v+τ²/2) + log|XᵀΣ⁻¹X| + RSS).
            eval t2 =
              let t2arm = t2 / 2
                  aMat = [ [ sum [ (r !! i) * (r !! j) / (v + t2arm) | (r, _, v) <- arms ] | j <- [0 .. nP - 1] ]
                         | i <- [0 .. nP - 1] ]
                  bVec = [ sum [ (r !! i) * y / (v + t2arm) | (r, y, v) <- arms ] | i <- [0 .. nP - 1] ]
                  cov  = matInv aMat
                  theta = matVec cov bVec
                  sumLogV = sum [ log (v + t2arm) | (_, _, v) <- arms ]
                  yWy = sum [ y * y / (v + t2arm) | (_, y, v) <- arms ]
                  residSS = yWy - sum (zipWith (*) bVec theta)
                  logZ = -0.5 * (sumLogV + logDetPD aMat + residSS)
              in (logZ, theta !! betaCol, cov !! betaCol !! betaCol)
            evals = map eval ts
            -- posterior log-density on τ² = logZ + log prior(τ²)
            posts = [ l + logPriorT2 prior t | (t, (l, _, _)) <- zip ts evals ]
            bhats = [ b | (_, b, _) <- evals ]
            bvars = [ v | (_, _, v) <- evals ]
            mx    = maximum posts
            raw   = [ exp (p - mx) | p <- posts ]   -- uniform dt cancels in normalisation
            tot   = sum raw
            ws    = map (/ tot) raw
            modeIdx  = snd $ maximum (zip posts [0 :: Int ..])
            -- parabolic refinement of the posterior mode off the equally spaced grid
            tau2mode
              | modeIdx > 0 && modeIdx < nGrid - 1 && denom < 0 =
                  (ts !! modeIdx) + (dt / 2) * (y0 - y2) / denom
              | otherwise = ts !! modeIdx
              where y0 = posts !! (modeIdx - 1); y1 = posts !! modeIdx; y2 = posts !! (modeIdx + 1)
                    denom = y0 - 2 * y1 + y2
            tau2mean = sum (zipWith (*) ts ws)
            (t2lo, t2hi') = weightedCI ts ws
            betaMean = sum (zipWith (*) ws bhats)
            betaVar  = sum [ w * (v + b * b) | (w, b, v) <- zip3 ws bhats bvars ] - betaMean * betaMean
            (blo, bhi) = mixtureCI ws bhats bvars
        return RegPosterior
          { rpBeta = (betaMean, sqrt (max 0 betaVar), blo, bhi)
          , rpTau2 = (tau2mode, tau2mean, t2lo, t2hi')
          , rpGrid = zip4 ts ws bhats bvars }

-- | Covariate-coefficient structure for network meta-regression (cf. gemtc/MetaInsight):
-- a single shared slope, independent per-treatment slopes, or exchangeable slopes
-- β_t ~ N(B, σ_B²). All three are one Gaussian GC partition function with different
-- slope coupling (see test/nmr_gc_beta_model.md).
data BetaModel = SharedB | UnrelatedB | ExchangeableB
  deriving (Show, Eq)

type Summ = (Double, Double, Double, Double)   -- (mean, sd, lo95, hi95)

-- | Joint posterior for any 'BetaModel'. Returns per-treatment slopes (unrelated /
-- exchangeable), the common slope (shared / exchangeable hyper-mean), τ², and the
-- slope variance σ_B² (exchangeable).
data RegPosteriorB = RegPosteriorB
  { rpbBetas      :: [(TreatmentId, Summ)]  -- ^ per non-reference treatment slope
  , rpbCommonB    :: Maybe Summ             -- ^ shared slope (Shared) / hyper-mean (Exchangeable)
  , rpbTau2       :: Summ                   -- ^ heterogeneity (mean, sd, lo, hi)
  , rpbSigmaB2    :: Maybe Summ             -- ^ slope variance (Exchangeable)
  , rpbModeTau2   :: Double                 -- ^ τ² posterior marginal mode (≈ REML point)
  , rpbModeSigmaB2 :: Maybe Double          -- ^ σ_B² posterior marginal mode (Exchangeable; ≈ REML)
  } deriving (Show)

-- | Weighted quantile of (value, weight) pairs.
wquantile :: [(Double, Double)] -> Double -> Double
wquantile pairs p =
  let s = sortOn fst pairs
      tot = sum (map snd pairs)
      go _   []          = fst (last s)
      go acc ((v,w):rest) = if acc + w >= p * tot then v else go (acc + w) rest
  in go 0 s

-- | Network meta-regression joint posterior under any β structure, by exact
-- linear-Gaussian marginalisation (the GC partition function with the slope
-- coordinates included). See 'BetaModel' and test/nmr_gc_beta_model.md.
nmaRegressionPosteriorB ::
  (ArmEffect a b, Gaussian a, Gaussian b) =>
  [Study] ->
  Map.Map StudyId Double ->
  Int ->                                   -- ^ τ² grid resolution
  BetaModel ->
  Tau2PriorR ->                            -- ^ prior on τ² (heterogeneity)
  Tau2PriorR ->                            -- ^ prior on σ_B² (slope dispersion; Exchangeable only)
  (Arm -> Either String a) ->
  Either String RegPosteriorB
nmaRegressionPosteriorB studies covmap nGrid bmodel prior slopePrior armEffect =
  let treatments = sort $ nub $ concatMap (map tidOfArm . getStudyArms) studies
      ref    = head treatments
      nonref = filter (/= ref) treatments
      nT     = length nonref
      sids   = map getStudyId studies
      nS     = length sids
      sidx   = Map.fromList (zip sids [0 ..])
      tidx   = Map.fromList (zip nonref [0 ..])
      nonrefCovs = [ Map.findWithDefault 0 (getStudyId s) covmap
                   | s <- studies, a <- getStudyArms s, tidOfArm a /= ref ]
      hasBeta = length (nub nonrefCovs) > 1
      nBeta   = case bmodel of SharedB -> 1; UnrelatedB -> nT; ExchangeableB -> nT + 1
      betaBase = nS + nT                                   -- first slope column
      dCol t   = nS + tidx Map.! t
      bCol t   = case bmodel of SharedB -> betaBase; _ -> betaBase + tidx Map.! t
      commonCol = betaBase + nT                            -- hyper-mean column (Exchangeable)
      nP = nS + nT + nBeta
      setAt xs i v = take i xs ++ [v] ++ drop (i+1) xs
      armInfo study a = do
        ef <- armEffect a
        let rel = relatedRelative ef; y = expectation rel; v = variance rel; t = tidOfArm a
            si = sidx Map.! getStudyId study
            x  = Map.findWithDefault 0 (getStudyId study) covmap
            base = setAt (replicate nP 0) si (-1)
            row | t == ref  = base
                | otherwise = setAt (setAt base (dCol t) 1) (bCol t) x
        return (row, y, v)
   in if not hasBeta then Left "nmaRegressionPosteriorB: covariate does not vary; β unidentified" else do
        arms <- concat <$> mapM (\s -> mapM (armInfo s) (getStudyArms s)) studies
        (_, tau2mode0, _) <- nmaRegressionRE studies covmap (Just 0.1) armEffect
        let addM m i j d = [ [ if r==i && c==j then (m!!r!!c)+d else m!!r!!c | c<-[0..nP-1] ] | r<-[0..nP-1] ]
            -- Gaussian solve at τ² with optional exchangeable coupling 1/σ_B² (mInvSig)
            solveCell t2 mInvSig =
              let t2arm = t2/2
                  a0 = [ [ sum [ (r!!i)*(r!!j)/(v+t2arm) | (r,_,v)<-arms ] | j<-[0..nP-1] ] | i<-[0..nP-1] ]
                  aMat = case mInvSig of
                           Nothing -> a0
                           Just is -> foldl' (\m t -> let bt=bCol t; bb=commonCol in
                                        addM (addM (addM (addM m bt bt is) bb bb is) bt bb (negate is)) bb bt (negate is))
                                      a0 nonref
                  bVec = [ sum [ (r!!i)*y/(v+t2arm) | (r,y,v)<-arms ] | i<-[0..nP-1] ]
                  cov  = matInv aMat
                  theta = matVec cov bVec
                  sumLogV = sum [ log (v+t2arm) | (_,_,v)<-arms ]
                  yWy = sum [ y*y/(v+t2arm) | (_,y,v)<-arms ]
                  logZ = -0.5*(sumLogV + logDetPD aMat + (yWy - sum (zipWith (*) bVec theta)))
              in (logZ, theta, cov)
            tau2hi = max (tau2mode0*12) 0.02
            ts = [ tau2hi*(fromIntegral i-0.5)/fromIntegral nGrid | i<-[1..nGrid] ]
            cells = case bmodel of
              ExchangeableB ->
                -- σ_B² is often weakly identified with mass near 0, so use a
                -- LOG-spaced grid (with the dσ_B² = σ_B²·dlog Jacobian, i.e. +log s)
                -- reaching down to sigHi/1e4 — a midpoint linear grid floors too high.
                let nSig = 40
                    sigHi = case slopePrior of UniformSD u -> u*u; _ -> 1.0
                    sLo   = sigHi / 1e4
                    ss = [ sLo * (sigHi/sLo) ** ((fromIntegral i-0.5)/fromIntegral nSig) | i<-[1..nSig::Int] ]
                in [ let (lz,th,cv) = solveCell t (Just (1/s))
                         lp = lz - 0.5*fromIntegral nT*log s
                              + logPriorT2 prior t + logPriorT2 slopePrior s + log s  -- +log s = log Jacobian
                     in (lp, t, Just s, th, cv) | t<-ts, s<-ss ]
              _ -> [ let (lz,th,cv) = solveCell t Nothing
                     in (lz + logPriorT2 prior t, t, Nothing, th, cv) | t<-ts ]
            lps = [ lp | (lp,_,_,_,_)<-cells ]
            mx  = maximum lps
            raw = [ exp (lp-mx) | lp<-lps ]; tot = sum raw; ws = map (/tot) raw
            thetas = [ th | (_,_,_,th,_)<-cells ]; covs = [ cv | (_,_,_,_,cv)<-cells ]
            summ c = let m  = sum (zipWith (\w th -> w*(th!!c)) ws thetas)
                         s2 = sum (zipWith3 (\w th cv -> w*((cv!!c!!c)+(th!!c)^2)) ws thetas covs) - m*m
                         (lo,hi) = mixtureCI ws (map (!!c) thetas) (map (\cv -> cv!!c!!c) covs)
                     in (m, sqrt (max 0 s2), lo, hi)
            tcells = zip [ t | (_,t,_,_,_)<-cells ] ws
            tau2summ = let m = sum [ t*w | (t,w)<-tcells ]
                       in (m, sqrt (max 0 (sum [ t*t*w | (t,w)<-tcells ] - m*m)),
                           wquantile tcells 0.025, wquantile tcells 0.975)
            betas = if bmodel == SharedB then []
                    else [ (t, summ (bCol t)) | t <- nonref ]
            commonB = case bmodel of
                        SharedB       -> Just (summ betaBase)
                        ExchangeableB -> Just (summ commonCol)
                        UnrelatedB    -> Nothing
            sigSumm = case bmodel of
                        ExchangeableB ->
                          let sc = [ (s,w) | ((_,_,Just s,_,_),w) <- zip cells ws ]
                              m  = sum [ s*w | (s,w)<-sc ]
                          in Just (m, sqrt (max 0 (sum [ s*s*w | (s,w)<-sc ] - m*m)),
                                   wquantile sc 0.025, wquantile sc 0.975)
                        _ -> Nothing
            -- marginal modes (group grid weights by the fugacity value, argmax) — the
            -- REML-comparable point of the GC posterior, vs the integrated means above.
            marg pairs = fst $ maximumBy (\x y -> compare (snd x) (snd y))
                               (Map.toList (Map.fromListWith (+) pairs))
            modeTau2 = marg [ (t,w) | ((_,t,_,_,_),w) <- zip cells ws ]
            modeSigB2 = if bmodel == ExchangeableB
                          then Just (marg [ (s,w) | ((_,_,Just s,_,_),w) <- zip cells ws ])
                          else Nothing
        return RegPosteriorB { rpbBetas=betas, rpbCommonB=commonB
                             , rpbTau2=tau2summ, rpbSigmaB2=sigSumm
                             , rpbModeTau2=modeTau2, rpbModeSigmaB2=modeSigB2 }

-- | Fixed-effect BINARY network meta-regression (logit link) via IRLS: the same
-- β-regression machinery as 'nmaRegression', wrapped in the iteratively-reweighted
-- least-squares loop that linearises each binomial arm. At each step an arm with
-- events e of n at linear predictor η contributes a Gaussian working-response spring
-- (weight w = n·p(1−p), response z = η + (e − n·p)/w) — exactly @linearize@ applied to
-- a 'BinomialSpring'. Returns (β, log-OR contrasts at x=0).
nmaRegressionBin ::
  [Study] ->
  Map.Map StudyId Double ->
  Either String (Double, Map.Map TreatmentId (Map.Map TreatmentId Double))
nmaRegressionBin studies covmap =
  let treatments = sort $ nub $ concatMap (map tidOfArm . getStudyArms) studies
      ref    = head treatments
      nonref = filter (/= ref) treatments
      nT     = length nonref
      sids   = map getStudyId studies
      nS     = length sids
      sidx   = Map.fromList (zip sids [0 ..])
      tidx   = Map.fromList (zip nonref [0 ..])
      nonrefCovs = [ Map.findWithDefault 0 (getStudyId s) covmap
                   | s <- studies, a <- getStudyArms s, tidOfArm a /= ref ]
      hasBeta = length (nub nonrefCovs) > 1
      nP      = nS + nT + (if hasBeta then 1 else 0)
      betaCol = nS + nT
      setAt xs i v = take i xs ++ [v] ++ drop (i+1) xs
      armInfo study a = case a of
        BinaryArm t ev ns ->
          let si = sidx Map.! getStudyId study
              x  = Map.findWithDefault 0 (getStudyId study) covmap
              base = setAt (replicate nP 0) si (-1)   -- η = −μ_si + d_t + β·x  (same sign convention)
              row | t == ref  = base
                  | hasBeta   = setAt (setAt base (nS + tidx Map.! t) 1) betaCol x
                  | otherwise = setAt base (nS + tidx Map.! t) 1
          in Right (row, fromIntegral ev, fromIntegral ns)
        _ -> Left "nmaRegressionBin: expects binary arms"
   in do
        arms <- concat <$> mapM (\s -> mapM (armInfo s) (getStudyArms s)) studies
        let irls theta it
              | it > (100 :: Int)               = theta
              | maximum (zipWith (\a b -> abs (a-b)) theta theta') < 1e-10 = theta'
              | otherwise                        = irls theta' (it+1)
              where wz = [ let eta = sum (zipWith (*) r theta); p = sigmoid eta
                               w = max 1e-9 (n*p*(1-p)); z = eta + (e - n*p)/w
                           in (w,z) | (r,e,n) <- arms ]
                    aMat = [ [ sum [ w*(r!!i)*(r!!j) | ((r,_,_),(w,_)) <- zip arms wz ] | j<-[0..nP-1] ] | i<-[0..nP-1] ]
                    bVec = [ sum [ w*(r!!i)*z | ((r,_,_),(w,z)) <- zip arms wz ] | i<-[0..nP-1] ]
                    theta' = matVec (matInv aMat) bVec
            thetaF = irls (replicate nP 0) 0
            beta = if hasBeta then thetaF !! betaCol else 0
            dOf t = if t == ref then 0 else thetaF !! (nS + tidx Map.! t)
            contrasts = Map.fromList
              [ (t1, Map.fromList [ (t2, dOf t2 - dOf t1) | t2 <- treatments ]) | t1 <- treatments ]
        return (beta, contrasts)

-- | BINARY joint (β, τ², σ_B²) posterior — the GC for binary, by single-Laplace:
-- linearise the binomial arms once at the shared-β IRLS fit (working response z,
-- variance 1/w), then run the Gaussian exchangeable GC ('nmaRegressionPosteriorB')
-- on the frozen working response. Because the GC *integrates* σ_B² (rather than
-- maximizing it, as the grouped REML does), this lifts σ_B² off the boundary and
-- recovers gemtc's posterior mean. Returns the same 'RegPosteriorB' (with marginal
-- means AND modes). A single-point Laplace — re-linearising per grid cell would be
-- more accurate, but this already matches gemtc's posterior means.
nmaRegressionBinPosteriorB ::
  [Study] -> Map.Map StudyId Double -> Int -> BetaModel -> Tau2PriorR -> Tau2PriorR ->
  Either String RegPosteriorB
nmaRegressionBinPosteriorB studies covmap nGrid bmodel prior slopePrior = do
  pseudo <- linearizeBinaryRegression studies covmap
  nmaRegressionPosteriorB pseudo covmap nGrid bmodel prior slopePrior meanArm

-- | Linearise a binary network meta-regression at its shared-β IRLS fit, returning
-- pseudo-continuous studies (each arm a ContinuousArm carrying the working response z
-- with variance 1/w). The Gaussian β-machinery (FE/RE/GC) then runs on these — this is
-- the bridge that lets every continuous regression function serve binary outcomes.
linearizeBinaryRegression ::
  [Study] -> Map.Map StudyId Double -> Either String [Study]
linearizeBinaryRegression studies covmap =
  let treatments = sort $ nub $ concatMap (map tidOfArm . getStudyArms) studies
      ref = head treatments; nonref = filter (/= ref) treatments; nT = length nonref
      sids = map getStudyId studies; nS = length sids
      sidx = Map.fromList (zip sids [0..]); tidx = Map.fromList (zip nonref [0..])
      nonrefCovs = [ Map.findWithDefault 0 (getStudyId s) covmap
                   | s <- studies, a <- getStudyArms s, tidOfArm a /= ref ]
      hasBeta = length (nub nonrefCovs) > 1
      nP = nS + nT + (if hasBeta then 1 else 0); betaCol = nS + nT
      setAt xs i v = take i xs ++ [v] ++ drop (i+1) xs
      armRec study a = case a of
        BinaryArm t ev ns ->
          let si = sidx Map.! getStudyId study; x = Map.findWithDefault 0 (getStudyId study) covmap
              base = setAt (replicate nP 0) si (-1)
              row | t == ref  = base
                  | hasBeta   = setAt (setAt base (nS + tidx Map.! t) 1) betaCol x
                  | otherwise = setAt base (nS + tidx Map.! t) 1
          in Right (row, fromIntegral ev, fromIntegral ns)
        _ -> Left "linearizeBinaryRegression: expects binary arms"
   in do
        recs <- mapM (\s -> mapM (armRec s) (getStudyArms s)) studies
        let flat = concat recs
            irls theta it
              | it > (100::Int) = theta
              | maximum (zipWith (\a b->abs(a-b)) theta theta') < 1e-10 = theta'
              | otherwise = irls theta' (it+1)
              where wz = [ let eta=sum (zipWith (*) r theta); p=sigmoid eta
                               w=max 1e-9 (n*p*(1-p)); z=eta+(e-n*p)/w in (w,z) | (r,e,n)<-flat ]
                    aMat = [ [ sum [ w*(r!!i)*(r!!j) | ((r,_,_),(w,_))<-zip flat wz ] | j<-[0..nP-1] ] | i<-[0..nP-1] ]
                    bVec = [ sum [ w*(r!!i)*z | ((r,_,_),(w,z))<-zip flat wz ] | i<-[0..nP-1] ]
                    theta' = matVec (matInv aMat) bVec
            thetaF = irls (replicate nP 0) 0
            working = [ let eta=sum (zipWith (*) r thetaF); p=sigmoid eta
                            w=max 1e-9 (n*p*(1-p)) in (eta + (e-n*p)/w, 1/w) | (r,e,n)<-flat ]
            rebuild [] _ = []
            rebuild (s:ss) ws = let k = length (getStudyArms s); (mine, rest) = splitAt k ws
                                    arms' = zipWith (\a (z,v) -> ContinuousArm (tidOfArm a) z (sqrt v) 1)
                                              (getStudyArms s) mine
                                in ContinuousStudy (getStudyId s) arms' : rebuild ss rest
        return (rebuild studies working)

-- | Random-effects BINARY network meta-regression (logit link, shared β) by PQL:
-- the IRLS binomial linearisation interleaved with the REML/EM heterogeneity update.
-- Each iteration linearises every arm to a Gaussian working-response spring (working
-- variance 1/w), adds τ²/2 (the heterogeneity spring in series), solves the GLS, and
-- updates τ² from the BLUPs — the binary analogue of 'nmaRegressionRE'. Returns
-- (β, τ², log-OR contrasts at x=0). (PQL is a linearised approximation to the exact
-- binomial RE that gemtc samples; point estimates agree closely for non-sparse data.)
nmaRegressionBinRE ::
  [Study] -> Map.Map StudyId Double -> Maybe TauSquare ->
  Either String (Double, TauSquare, Map.Map TreatmentId (Map.Map TreatmentId Double))
nmaRegressionBinRE studies covmap mtau0 =
  let treatments = sort $ nub $ concatMap (map tidOfArm . getStudyArms) studies
      ref = head treatments; nonref = filter (/= ref) treatments; nT = length nonref
      sids = map getStudyId studies; nS = length sids
      sidx = Map.fromList (zip sids [0..]); tidx = Map.fromList (zip nonref [0..])
      nonrefCovs = [ Map.findWithDefault 0 (getStudyId s) covmap
                   | s <- studies, a <- getStudyArms s, tidOfArm a /= ref ]
      hasBeta = length (nub nonrefCovs) > 1
      nP = nS + nT + (if hasBeta then 1 else 0); betaCol = nS + nT
      setAt xs i v = take i xs ++ [v] ++ drop (i+1) xs
      armInfo study a = case a of
        BinaryArm t ev ns ->
          let si = sidx Map.! getStudyId study; x = Map.findWithDefault 0 (getStudyId study) covmap
              base = setAt (replicate nP 0) si (-1)
              row | t == ref  = base
                  | hasBeta   = setAt (setAt base (nS + tidx Map.! t) 1) betaCol x
                  | otherwise = setAt base (nS + tidx Map.! t) 1
          in Right (row, fromIntegral ev, fromIntegral ns)
        _ -> Left "nmaRegressionBinRE: expects binary arms"
   in do
        arms <- concat <$> mapM (\s -> mapM (armInfo s) (getStudyArms s)) studies
        let nArm = fromIntegral (length arms)
            step (theta, tau2) =
              let t2arm = tau2/2
                  wz = [ let eta = sum (zipWith (*) r theta); p = sigmoid eta
                             w = max 1e-9 (n*p*(1-p)); z = eta + (e - n*p)/w in (r,z,w)
                       | (r,e,n) <- arms ]
                  wEff w = 1/(1/w + t2arm)
                  aMat = [ [ sum [ wEff w*(r!!i)*(r!!j) | (r,_,w) <- wz ] | j<-[0..nP-1] ] | i<-[0..nP-1] ]
                  bVec = [ sum [ wEff w*(r!!i)*z | (r,z,w) <- wz ] | i<-[0..nP-1] ]
                  cov = matInv aMat; theta' = matVec cov bVec
                  per (r,z,w) = let resid = z - sum (zipWith (*) r theta')
                                    b = w*t2arm/(1+w*t2arm)
                                in ((b*resid)^2, (1-b)*t2arm + b*b*quadForm r cov)
                  ms = map per wz
              in (theta', max 1e-8 $ 2*(sum (map fst ms)/nArm + sum (map snd ms)/nArm))
            loop p it | it > (200::Int) = p
                      | maximum (zipWith (\a b->abs(a-b)) (fst p) (fst p')) < 1e-9 && abs (snd p - snd p') < 1e-9 = p'
                      | otherwise = loop p' (it+1)
              where p' = step p
            (thetaF, tau2F) = loop (replicate nP 0, fromMaybe 0.5 mtau0) 0
            beta = if hasBeta then thetaF !! betaCol else 0
            dOf t = if t == ref then 0 else thetaF !! (nS + tidx Map.! t)
            contrasts = Map.fromList [ (t1, Map.fromList [ (t2, dOf t2 - dOf t1) | t2<-treatments ]) | t1<-treatments ]
        return (beta, tau2F, contrasts)

-- | Baseline-risk network meta-regression fit (shared slope). The covariate is the
-- study's own latent baseline log-odds η_i, so the treatment effect is regressed on the
-- *model's* baseline (not the observed control proportion) — the Achana 2013 / bnma model
-- MetaInsight uses for baseline-risk regression, which avoids regression-to-the-mean.
data BaselineRiskFit = BaselineRiskFit
  { brMx   :: Double                        -- ^ centering constant mx (mean cc control log-odds)
  , brB    :: Double                        -- ^ shared baseline-risk slope B (reference slope = 0)
  , brBse  :: Double                        -- ^ Laplace SE of B (inverse Fisher information)
  , brTau2 :: Double                        -- ^ heterogeneity τ² (contrast scale; 0 for FE)
  , brD    :: Map.Map TreatmentId Double    -- ^ contrasts vs reference at baseline = mx
  , brDse  :: Map.Map TreatmentId Double    -- ^ Laplace SE of each contrast
  , brEta  :: Map.Map StudyId Double        -- ^ fitted latent study baselines η_i (log-odds)
  , brBt   :: Map.Map TreatmentId Double    -- ^ per-treatment slopes (= brB for shared)
  , brBtSe :: Map.Map TreatmentId Double    -- ^ Laplace SE of each per-treatment slope
  , brSigmaB2 :: Maybe Double               -- ^ σ_B² slope dispersion (exchangeable only)
  } deriving (Show)

-- | Slope structure for baseline-risk regression (cf. bnma / MetaInsight):
-- one shared slope; independent per-treatment slopes; or exchangeable slopes
-- b_t ~ N(B, σ_B²) (a slope-spring of hardness 1/σ_B² to a common mean, σ_B² integrated).
data BaselineSlopeModel = BLShared | BLUnrelated | BLExchangeable
  deriving (Show, Eq)

-- | mean continuity-corrected control-arm log-odds (the fixed centering mx, exactly as
-- bnma computes it: r→0.5, n→n+1 for zero-event control arms).
baselineMx :: [(Double, Double)] -> Double
baselineMx ctrl =
  let lo (r, n) = let rd = if r == 0 then 0.5 else if r == n then n - 0.5 else r
                      nd = if r == 0 then n + 1 else n
                      p  = rd / nd
                  in log (p / (1 - p))
  in sum (map lo ctrl) / fromIntegral (length ctrl)

-- | FIXED-effect BINARY baseline-risk network meta-regression (logit link), the
-- Achana 2013 / bnma model used by MetaInsight:
--   logit(p_ik) = η_i + d_t + B·(η_i − mx),   η_i = latent study baseline (control log-odds),
--   mx = mean continuity-corrected observed control log-odds (a FIXED data constant),
--   B  = shared baseline-risk slope, reference slope pinned to 0, d_t = effect at baseline mx.
-- Because the regressor is the parameter η_i itself the predictor carries the bilinear term
-- B·η_i; this is solved by Fisher scoring (Gauss–Newton with Jacobian ∂η/∂η_i = 1+B,
-- ∂η/∂d_t = 1, ∂η/∂B = η_i−mx), which converges to the exact MLE. The Laplace covariance
-- (inverse Fisher information at the mode) gives the SEs. The reference treatment is the
-- control and must appear in every study.
nmaRegressionBaselineRiskBin ::
  [Study] -> TreatmentId -> Either String BaselineRiskFit
nmaRegressionBaselineRiskBin studies ref =
  baselineRiskCoreB studies ref BLShared Nothing

-- | General BINARY baseline-risk regression under any slope structure (shared / unrelated /
-- exchangeable) and FE (Nothing) or RE (Just τ²₀). Unrelated gives an independent slope per
-- treatment; exchangeable shrinks the per-treatment slopes toward a common mean B with
-- dispersion σ_B² (a slope-spring of hardness 1/σ_B², σ_B² estimated by EM) — the bnma
-- coefficient="independent"/"exchangeable" variants.
nmaRegressionBaselineRiskBinB ::
  [Study] -> TreatmentId -> BaselineSlopeModel -> Maybe TauSquare -> Either String BaselineRiskFit
nmaRegressionBaselineRiskBinB studies ref model mtau0 =
  baselineRiskCoreB studies ref model (fmap (\t -> if t <= 0 then 0.1 else t) mtau0)

-- | RANDOM-effects BINARY baseline-risk network meta-regression by PQL: the Fisher-scoring
-- baseline-risk solve interleaved with the REML/EM heterogeneity update (τ²/2 per arm in
-- series, BLUP shrinkage), the baseline-risk analogue of 'nmaRegressionBinRE'. Returns the
-- same 'BaselineRiskFit' with brTau2 the contrast-scale heterogeneity.
nmaRegressionBaselineRiskBinRE ::
  [Study] -> TreatmentId -> Maybe TauSquare -> Either String BaselineRiskFit
nmaRegressionBaselineRiskBinRE studies ref mtau0 =
  baselineRiskCoreB studies ref BLShared (Just (fromMaybe 0.1 mtau0))

-- | General core for shared / unrelated / exchangeable baseline-risk regression,
-- FE (Nothing) or RE (Just τ²₀). Coordinates: η_i (baselines), d_t (contrasts), and slopes —
-- one B (shared), nT independent b_t (unrelated), or nT b_t + common B (exchangeable, with a
-- 1/σ_B² slope-spring tying each b_t to B). Solved by damped Fisher scoring on the bilinear
-- predictor; σ_B² and τ² updated by EM.
baselineRiskCoreB ::
  [Study] -> TreatmentId -> BaselineSlopeModel -> Maybe Double -> Either String BaselineRiskFit
baselineRiskCoreB studies ref model mInitTau =
  let treatments = sort $ nub $ concatMap (map tidOfArm . getStudyArms) studies
      nonref = filter (/= ref) treatments
      nT     = length nonref
      sids   = map getStudyId studies
      nS     = length sids
      sidx   = Map.fromList (zip sids [0 ..])
      tidx   = Map.fromList (zip nonref [0 ..])
      exch   = model == BLExchangeable
      slopeBase = nS + nT
      dCol t  = nS + tidx Map.! t
      slopeCol t = case model of BLShared -> slopeBase; _ -> slopeBase + tidx Map.! t
      commonCol = slopeBase + nT                    -- exchangeable common mean B
      nP = case model of
             BLShared       -> nS + nT + 1
             BLUnrelated    -> nS + nT + nT
             BLExchangeable -> nS + nT + nT + 1
      setAt xs i v = take i xs ++ [v] ++ drop (i + 1) xs
      armRec study a = case a of
        BinaryArm t ev ns ->
          let si = sidx Map.! getStudyId study
          in Right (si, t == ref, t, fromIntegral ev, fromIntegral ns)
        _ -> Left "nmaRegressionBaselineRiskBin: expects binary arms"
   in if ref `notElem` treatments
        then Left "nmaRegressionBaselineRiskBin: reference treatment absent"
      else do
        arms <- concat <$> mapM (\s -> mapM (armRec s) (getStudyArms s)) studies
        -- control-arm (events,n) per study for the centering constant
        ctrl <- mapM (\s -> case filter (\a -> tidOfArm a == ref) (getStudyArms s) of
                              (BinaryArm _ ev ns : _) -> Right (fromIntegral ev, fromIntegral ns)
                              _ -> Left ("nmaRegressionBaselineRiskBin: study " ++ show (getStudyId s)
                                         ++ " lacks the reference arm")) studies
        let mx = baselineMx ctrl
            nArm = fromIntegral (length arms)
            -- linear predictor, p, weight, Jacobian row, and working response for one arm.
            -- Active arm of treatment t uses its own slope column (slopeCol t); the bilinear
            -- term couples it to the baseline node (∂η/∂η_i = 1+b_t, ∂η/∂b_t = η_i−mx).
            pieces theta (si, isRef, t, e, n) =
              let etaI = theta !! si
                  bT   = if isRef then 0 else theta !! slopeCol t
                  eta  | isRef     = etaI
                       | otherwise = etaI + (theta !! dCol t) + bT * (etaI - mx)
                  p    = sigmoid eta
                  w    = max 1e-9 (n * p * (1 - p))
                  jac  | isRef     = setAt (replicate nP 0) si 1
                       | otherwise = setAt (setAt (setAt (replicate nP 0) si (1 + bT)) (dCol t) 1)
                                           (slopeCol t) (etaI - mx)
                  jtheta = sum (zipWith (*) jac theta)
                  zz     = jtheta + (e - n * p) / w
              in (jac, zz, w, e, n)
            -- exchangeable slope-springs: ridge 1/σ_B² tying each b_t to the common mean B
            addAt m i j d = [ [ if r == i && c == j then (m !! r !! c) + d else m !! r !! c
                              | c <- [0 .. nP - 1] ] | r <- [0 .. nP - 1] ]
            -- Exchangeable: slope-spring 1/σ_B² tying each b_t to the common mean B.
            -- Unrelated: a weak ridge (prec 1e-4, SD 100) on each independent slope so an
            -- under-identified treatment (e.g. one appearing in a single study, where a
            -- baseline-risk slope is not identified) yields a large SE rather than a singular
            -- collapse. Negligible for well-identified slopes; for shared there is no ridge.
            ridge invSig a0 = case model of
              BLExchangeable -> foldl' (\m t -> let bt = slopeCol t; bb = commonCol
                                                in addAt (addAt (addAt (addAt m bt bt invSig) bb bb invSig)
                                                                bt bb (negate invSig)) bb bt (negate invSig))
                                       a0 nonref
              BLUnrelated    -> foldl' (\m t -> addAt m (slopeCol t) (slopeCol t) 1e-4) a0 nonref
              BLShared       -> a0
            -- one Fisher-scoring step at heterogeneity t2 (t2=0 FE) and slope dispersion σ_B²
            scoreStep t2 sigB2 theta =
              let t2arm = t2 / 2
                  ps    = map (pieces theta) arms
                  wEff w = if t2 > 0 then 1 / (1 / w + t2arm) else w
                  a0   = [ [ sum [ wEff w * (jac !! i) * (jac !! j) | (jac, _, w, _, _) <- ps ]
                           | j <- [0 .. nP - 1] ] | i <- [0 .. nP - 1] ]
                  aMat = ridge (1 / sigB2) a0
                  bVec = [ sum [ wEff w * (jac !! i) * zz | (jac, zz, w, _, _) <- ps ] | i <- [0 .. nP - 1] ]
                  cov  = matInv aMat
              in (matVec cov bVec, cov, ps)
            -- EM update of τ² from the per-arm BLUPs (contrast scale ⇒ factor 2)
            emTau t2 theta cov ps =
              let t2arm = t2 / 2
                  per (jac, zz, w, _, _) =
                    let resid = zz - sum (zipWith (*) jac theta)
                        b     = w * t2arm / (1 + w * t2arm)
                    in ((b * resid) ^ (2 :: Int), (1 - b) * t2arm + b * b * quadForm jac cov)
                  ms = map per ps
              in max 1e-8 $ 2 * (sum (map fst ms) / nArm + sum (map snd ms) / nArm)
            -- EM update of σ_B² (exchangeable): mean shrunk squared slope-deviation + variance
            emSig theta cov =
              let perT t = let m = (theta !! slopeCol t) - (theta !! commonCol)
                           in m * m + cov !! slopeCol t !! slopeCol t
                                + cov !! commonCol !! commonCol - 2 * cov !! slopeCol t !! commonCol
              in max 1e-8 $ sum (map perT nonref) / fromIntegral nT
            -- joint loop over (θ, τ², σ_B²)
            ctrlLo i = let (e, n) = ctrl !! i
                           rd = if e == 0 then 0.5 else e
                           nd = if e == 0 then n + 1 else n
                           pp = rd / nd
                       in log (pp / (1 - pp))
            theta0 = [ if i < nS then ctrlLo i else 0 | i <- [0 .. nP - 1] ]
            finite x = not (isNaN x || isInfinite x)
            -- binomial log-likelihood of the mean structure
            logLik theta = sum
              [ let etaI = theta !! si
                    eta  | isRef     = etaI
                         | otherwise = etaI + (theta !! dCol t) + (theta !! slopeCol t) * (etaI - mx)
                    p = min (1 - 1e-12) (max 1e-12 (sigmoid eta))
                in e * log p + (n - e) * log (1 - p)
              | (si, isRef, t, e, n) <- arms ]
            -- penalised objective for the line search: logLik − ½·(slope-spring energy)/σ_B².
            -- (For shared/unrelated the ridge is absent, so this is exactly logLik.)
            penOf sigB2 theta = logLik theta - if exch
              then 0.5 / sigB2 * sum [ (theta !! slopeCol t - theta !! commonCol) ^ (2 :: Int) | t <- nonref ]
              else 0
            -- damped Fisher scoring: the bilinear b_t·η_i term makes raw Newton steps
            -- overshoot (esp. near 1+b_t≈0), so backtrack along the Newton direction until
            -- the penalised objective does not decrease.
            loop theta t2 sigB2 it
              | it > (200 :: Int) = (theta, t2, sigB2)
              | converged         = (thetaNext, t2', sigB2')
              | otherwise         = loop thetaNext t2' sigB2' (it + 1)
              where (thetaNewton, _, _) = scoreStep t2 sigB2 theta
                    dir    = zipWith (-) thetaNewton theta
                    penCur = penOf sigB2 theta
                    damp a k
                      | k <= (0 :: Int)                        = theta
                      | finite pTry && pTry >= penCur - 1e-6   = cand
                      | otherwise                              = damp (a / 2) (k - 1)
                      where cand = zipWith (\x dx -> x + a * dx) theta dir
                            pTry = penOf sigB2 cand
                    thetaNext = damp 1.0 40
                    (_, covN, psN) = scoreStep t2 sigB2 thetaNext
                    t2'    = case mInitTau of Nothing -> 0; Just _ -> emTau t2 thetaNext covN psN
                    sigB2' = if exch then emSig thetaNext covN else sigB2
                    converged = maximum (zipWith (\a b -> abs (a - b)) theta thetaNext) < 1e-9
                                  && abs (t2 - t2') < 1e-9 && abs (sigB2 - sigB2') < 1e-9
            (thetaF, tau2F, sigB2F) = loop theta0 (fromMaybe 0 mInitTau) 0.1 0
            -- SEs from the OBSERVED information (Laplace curvature of the log-posterior at
            -- the mode). The bilinear term ∂²η/∂B∂η_i = 1 contributes a residual-weighted
            -- cross term that the expected (Fisher) information drops; including it matches
            -- the exact observed-information SE (e.g. R optim's Hessian).
            (_, fisherCov, _) = scoreStep tau2F sigB2F thetaF
            iFisher = matInv fisherCov                       -- expected info JᵀWJ (+ ridge/heterogeneity)
            -- residual (e − n·p) per active arm; the bilinear cross term ∂²η/∂b_t∂η_i = 1
            -- lands at (si, slopeCol t).
            crossAt = [ (si, slopeCol t, e - n * sigmoid eta)
                      | (si, isRef, t, e, n) <- arms, not isRef
                      , let eta = (thetaF !! si) + (thetaF !! dCol t)
                                    + (thetaF !! slopeCol t) * (thetaF !! si - mx) ]
            iObs = [ [ iFisher !! i !! j
                         - sum [ r | (s, sc, r) <- crossAt, (i == s && j == sc) || (i == sc && j == s) ]
                     | j <- [0 .. nP - 1] ] | i <- [0 .. nP - 1] ]
            covObs = matInv iObs
            seOf i = sqrt (max 0 (covObs !! i !! i))
            dMap  = Map.fromList [ (t, thetaF !! dCol t) | t <- nonref ]
            dSe   = Map.fromList [ (t, seOf (dCol t))    | t <- nonref ]
            etaMap = Map.fromList (zip sids [ thetaF !! i | i <- [0 .. nS - 1] ])
            btMap = Map.fromList [ (t, thetaF !! slopeCol t) | t <- nonref ]
            btSe  = Map.fromList [ (t, seOf (slopeCol t))    | t <- nonref ]
            -- headline slope: the single B (shared), common mean B (exch), or the mean of the
            -- independent slopes (unrelated, with the proper SE of that mean from covObs).
            (bMain, bMainSe) = case model of
              BLShared       -> (thetaF !! slopeBase, seOf slopeBase)
              BLExchangeable -> (thetaF !! commonCol, seOf commonCol)
              BLUnrelated    ->
                let m  = sum [ thetaF !! slopeCol t | t <- nonref ] / fromIntegral nT
                    v  = sum [ covObs !! slopeCol s !! slopeCol t | s <- nonref, t <- nonref ]
                           / fromIntegral (nT * nT)
                in (m, sqrt (max 0 v))
        return BaselineRiskFit
                 { brMx = mx, brB = bMain, brBse = bMainSe
                 , brTau2 = tau2F, brD = dMap, brDse = dSe, brEta = etaMap
                 , brBt = btMap, brBtSe = btSe
                 , brSigmaB2 = if exch then Just sigB2F else Nothing }

-- | CONTINUOUS (mean-difference, identity link) baseline-risk network meta-regression, the
-- Achana / bnma (response="normal") model: y_ik = η_i + d_t + B·(η_i − mx) + ε,
-- η_i = latent study baseline (control-arm mean), mx = mean observed control mean. The
-- regressor is the parameter η_i, so the predictor keeps the bilinear term B·η_i (unlike
-- external-covariate continuous NMR, which is linear); solved by Gauss–Newton (Fisher
-- scoring with weight 1/v), the identity-link analogue of 'nmaRegressionBaselineRiskBin'.
nmaRegressionBaselineRiskMD ::
  [Study] -> TreatmentId -> Either String BaselineRiskFit
nmaRegressionBaselineRiskMD studies ref = baselineRiskCoreMD studies ref BLShared Nothing

-- | General CONTINUOUS baseline-risk regression: any slope structure, FE (Nothing) or
-- RE (Just τ²₀). The identity-link analogue of 'nmaRegressionBaselineRiskBinB'.
nmaRegressionBaselineRiskMDB ::
  [Study] -> TreatmentId -> BaselineSlopeModel -> Maybe TauSquare -> Either String BaselineRiskFit
nmaRegressionBaselineRiskMDB studies ref model mtau0 =
  baselineRiskCoreMD studies ref model (fmap (\t -> if t <= 0 then 0.1 else t) mtau0)

-- | General continuous (identity-link) baseline-risk core. Same coordinate layout, slope
-- structures, damped Gauss–Newton, observed-information SEs, EM for τ²/σ_B² as the binary
-- core 'baselineRiskCoreB'; only the per-arm likelihood differs (Gaussian weight 1/v and
-- working residual y−μ, vs binomial). mx = unweighted mean of the control-arm means.
baselineRiskCoreMD ::
  [Study] -> TreatmentId -> BaselineSlopeModel -> Maybe Double -> Either String BaselineRiskFit
baselineRiskCoreMD studies ref model mInitTau =
  let treatments = sort $ nub $ concatMap (map tidOfArm . getStudyArms) studies
      nonref = filter (/= ref) treatments
      nT     = length nonref
      sids   = map getStudyId studies
      nS     = length sids
      sidx   = Map.fromList (zip sids [0 ..])
      tidx   = Map.fromList (zip nonref [0 ..])
      exch   = model == BLExchangeable
      slopeBase = nS + nT
      dCol t  = nS + tidx Map.! t
      slopeCol t = case model of BLShared -> slopeBase; _ -> slopeBase + tidx Map.! t
      commonCol = slopeBase + nT
      nP = case model of
             BLShared       -> nS + nT + 1
             BLUnrelated    -> nS + nT + nT
             BLExchangeable -> nS + nT + nT + 1
      setAt xs i v = take i xs ++ [v] ++ drop (i + 1) xs
      armRec study a = case a of
        ContinuousArm t m sd n ->
          let si = sidx Map.! getStudyId study
              v  = sd * sd / fromIntegral n
          in Right (si, t == ref, t, m, max 1e-12 v)
        _ -> Left "nmaRegressionBaselineRiskMD: expects continuous arms"
   in if ref `notElem` treatments
        then Left "nmaRegressionBaselineRiskMD: reference treatment absent"
      else do
        arms <- concat <$> mapM (\s -> mapM (armRec s) (getStudyArms s)) studies
        ctrl <- mapM (\s -> case filter (\a -> tidOfArm a == ref) (getStudyArms s) of
                              (ContinuousArm _ m _ _ : _) -> Right m
                              _ -> Left ("nmaRegressionBaselineRiskMD: study " ++ show (getStudyId s)
                                         ++ " lacks the reference arm")) studies
        let mx = sum ctrl / fromIntegral (length ctrl)
            nArm = fromIntegral (length arms)
            -- linear predictor μ, Gaussian weight 1/v, Jacobian, working response (Gauss–Newton)
            pieces theta (si, isRef, t, y, v) =
              let etaI = theta !! si
                  bT   = if isRef then 0 else theta !! slopeCol t
                  mu   | isRef     = etaI
                       | otherwise = etaI + (theta !! dCol t) + bT * (etaI - mx)
                  w    = 1 / v
                  jac  | isRef     = setAt (replicate nP 0) si 1
                       | otherwise = setAt (setAt (setAt (replicate nP 0) si (1 + bT)) (dCol t) 1)
                                           (slopeCol t) (etaI - mx)
                  jtheta = sum (zipWith (*) jac theta)
                  zz     = jtheta + (y - mu)           -- identity link ⇒ working residual = y−μ
              in (jac, zz, w, y, mu)
            addAt m i j d = [ [ if r == i && c == j then (m !! r !! c) + d else m !! r !! c
                              | c <- [0 .. nP - 1] ] | r <- [0 .. nP - 1] ]
            ridge invSig a0 = case model of
              BLExchangeable -> foldl' (\m t -> let bt = slopeCol t; bb = commonCol
                                                in addAt (addAt (addAt (addAt m bt bt invSig) bb bb invSig)
                                                                bt bb (negate invSig)) bb bt (negate invSig))
                                       a0 nonref
              BLUnrelated    -> foldl' (\m t -> addAt m (slopeCol t) (slopeCol t) 1e-4) a0 nonref
              BLShared       -> a0
            scoreStep t2 sigB2 theta =
              let t2arm = t2 / 2
                  ps    = map (pieces theta) arms
                  wEff w = if t2 > 0 then 1 / (1 / w + t2arm) else w
                  a0   = [ [ sum [ wEff w * (jac !! i) * (jac !! j) | (jac, _, w, _, _) <- ps ]
                           | j <- [0 .. nP - 1] ] | i <- [0 .. nP - 1] ]
                  aMat = ridge (1 / sigB2) a0
                  bVec = [ sum [ wEff w * (jac !! i) * zz | (jac, zz, w, _, _) <- ps ] | i <- [0 .. nP - 1] ]
                  cov  = matInv aMat
              in (matVec cov bVec, cov, ps)
            emTau t2 theta cov ps =
              let t2arm = t2 / 2
                  per (jac, zz, w, _, _) =
                    let resid = zz - sum (zipWith (*) jac theta)
                        b     = w * t2arm / (1 + w * t2arm)
                    in ((b * resid) ^ (2 :: Int), (1 - b) * t2arm + b * b * quadForm jac cov)
                  ms = map per ps
              in max 1e-8 $ 2 * (sum (map fst ms) / nArm + sum (map snd ms) / nArm)
            emSig theta cov =
              let perT t = let m = (theta !! slopeCol t) - (theta !! commonCol)
                           in m * m + cov !! slopeCol t !! slopeCol t
                                + cov !! commonCol !! commonCol - 2 * cov !! slopeCol t !! commonCol
              in max 1e-8 $ sum (map perT nonref) / fromIntegral nT
            theta0 = [ if i < nS then ctrl !! i else 0 | i <- [0 .. nP - 1] ]
            finite x = not (isNaN x || isInfinite x)
            -- Gaussian log-likelihood (−½ Σ (y−μ)²/v); line-search criterion
            logLik theta = sum
              [ let etaI = theta !! si
                    mu   | isRef     = etaI
                         | otherwise = etaI + (theta !! dCol t) + (theta !! slopeCol t) * (etaI - mx)
                in negate (0.5 * (y - mu) ^ (2 :: Int) / v)
              | (si, isRef, t, y, v) <- arms ]
            penOf sigB2 theta = logLik theta - if exch
              then 0.5 / sigB2 * sum [ (theta !! slopeCol t - theta !! commonCol) ^ (2 :: Int) | t <- nonref ]
              else 0
            loop theta t2 sigB2 it
              | it > (200 :: Int) = (theta, t2, sigB2)
              | converged         = (thetaNext, t2', sigB2')
              | otherwise         = loop thetaNext t2' sigB2' (it + 1)
              where (thetaNewton, _, _) = scoreStep t2 sigB2 theta
                    dir    = zipWith (-) thetaNewton theta
                    penCur = penOf sigB2 theta
                    damp a k
                      | k <= (0 :: Int)                      = theta
                      | finite pTry && pTry >= penCur - 1e-9 = cand
                      | otherwise                            = damp (a / 2) (k - 1)
                      where cand = zipWith (\x dx -> x + a * dx) theta dir
                            pTry = penOf sigB2 cand
                    thetaNext = damp 1.0 40
                    (_, covN, psN) = scoreStep t2 sigB2 thetaNext
                    t2'    = case mInitTau of Nothing -> 0; Just _ -> emTau t2 thetaNext covN psN
                    sigB2' = if exch then emSig thetaNext covN else sigB2
                    converged = maximum (zipWith (\a b -> abs (a - b)) theta thetaNext) < 1e-10
                                  && abs (t2 - t2') < 1e-9 && abs (sigB2 - sigB2') < 1e-9
            (thetaF, tau2F, sigB2F) = loop theta0 (fromMaybe 0 mInitTau) 0.1 0
            (_, fisherCov, _) = scoreStep tau2F sigB2F thetaF
            iFisher = matInv fisherCov
            -- observed-info cross term w·(y−μ) at (si, slopeCol t) for each active arm
            crossAt = [ (si, slopeCol t, (y - mu) / v)
                      | (si, isRef, t, y, v) <- arms, not isRef
                      , let mu = (thetaF !! si) + (thetaF !! dCol t)
                                   + (thetaF !! slopeCol t) * (thetaF !! si - mx) ]
            iObs = [ [ iFisher !! i !! j
                         - sum [ r | (s, sc, r) <- crossAt, (i == s && j == sc) || (i == sc && j == s) ]
                     | j <- [0 .. nP - 1] ] | i <- [0 .. nP - 1] ]
            covObs = matInv iObs
            seOf i = sqrt (max 0 (covObs !! i !! i))
            dMap  = Map.fromList [ (t, thetaF !! dCol t) | t <- nonref ]
            dSe   = Map.fromList [ (t, seOf (dCol t))    | t <- nonref ]
            etaMap = Map.fromList (zip sids [ thetaF !! i | i <- [0 .. nS - 1] ])
            btMap = Map.fromList [ (t, thetaF !! slopeCol t) | t <- nonref ]
            btSe  = Map.fromList [ (t, seOf (slopeCol t))    | t <- nonref ]
            (bMain, bMainSe) = case model of
              BLShared       -> (thetaF !! slopeBase, seOf slopeBase)
              BLExchangeable -> (thetaF !! commonCol, seOf commonCol)
              BLUnrelated    ->
                let m = sum [ thetaF !! slopeCol t | t <- nonref ] / fromIntegral nT
                    v = sum [ covObs !! slopeCol s !! slopeCol t | s <- nonref, t <- nonref ]
                          / fromIntegral (nT * nT)
                in (m, sqrt (max 0 v))
        return BaselineRiskFit
                 { brMx = mx, brB = bMain, brBse = bMainSe
                 , brTau2 = tau2F, brD = dMap, brDse = dSe, brEta = etaMap
                 , brBt = btMap, brBtSe = btSe
                 , brSigmaB2 = if exch then Just sigB2F else Nothing }

-- | 2-D grand-canonical posterior for EXCHANGEABLE BINARY baseline-risk: integrates BOTH
-- variance components — heterogeneity τ² and slope dispersion σ_B² — instead of maximising
-- σ_B² (as the grouped-REML EM does). The slope-springs are "free, coming and going" exactly
-- like the τ-springs: Ξ(τ²,σ_B²) = ∫∫ Z(τ²,σ_B²) π(τ²) π(σ_B²). Single-point Laplace: the
-- binomial arms are linearised once at the exchangeable mode (working response z, weight w,
-- bilinear Jacobian frozen); then the (τ²,σ_B²) grid integration is the exact Gaussian
-- partition function with the slope prior's normalisation −½·n_T·log σ_B². σ_B² uses a
-- log-spaced grid. This lifts σ_B² off the REML boundary and recovers the Bayesian posterior
-- mean (cf. 'nmaRegressionPosteriorB', the covariate analogue).
data BaselineExchGC = BaselineExchGC
  { begMx        :: Double                                            -- ^ centering constant
  , begSigmaB2   :: (Double, Double, Double, Double)                  -- ^ σ_B²: (mode, mean, lo95, hi95)
  , begTau2      :: (Double, Double, Double, Double)                  -- ^ τ²:  (mode, mean, lo95, hi95)
  , begB         :: (Double, Double, Double, Double)                  -- ^ common slope B: (mean, sd, lo95, hi95)
  , begBt        :: Map.Map TreatmentId (Double, Double, Double, Double) -- ^ per-treatment slope b_t
  , begEMSigmaB2 :: Double                                            -- ^ the grouped-REML/EM σ_B² point (for contrast)
  } deriving (Show)

nmaRegressionBaselineRiskBinExchGC ::
  [Study] -> TreatmentId ->
  Int ->                                  -- ^ τ² grid resolution
  Tau2PriorR ->                           -- ^ prior on τ² (heterogeneity)
  Tau2PriorR ->                           -- ^ prior on σ_B² (slope dispersion)
  Either String BaselineExchGC
nmaRegressionBaselineRiskBinExchGC studies ref nGrid prior slopePrior = do
  fit <- baselineRiskCoreB studies ref BLExchangeable (Just 0.1)
  let treatments = sort $ nub $ concatMap (map tidOfArm . getStudyArms) studies
      nonref = filter (/= ref) treatments
      nT     = length nonref
      sids   = map getStudyId studies
      nS     = length sids
      sidx   = Map.fromList (zip sids [0 ..])
      tidx   = Map.fromList (zip nonref [0 ..])
      betaBase  = nS + nT
      dCol t    = nS + tidx Map.! t
      bCol t    = betaBase + tidx Map.! t
      commonCol = betaBase + nT
      nP        = nS + 2 * nT + 1
      mx        = brMx fit
      setAt xs i v = take i xs ++ [v] ++ drop (i + 1) xs
      -- the exchangeable mode θ̂ = (η, d, b_t, B), the single linearisation point
      thetaHat = [ coord i | i <- [0 .. nP - 1] ]
        where coord i
                | i < nS        = brEta fit Map.! (sids !! i)
                | i < nS + nT   = brD   fit Map.! (nonref !! (i - nS))
                | i < nS + 2*nT = brBt  fit Map.! (nonref !! (i - nS - nT))
                | otherwise     = brB   fit
      -- freeze the binomial linearisation at θ̂: (Jacobian row, working response z, variance 1/w)
      lin study a = case a of
        BinaryArm t ev ns ->
          let si   = sidx Map.! getStudyId study
              isR  = t == ref
              etaI = thetaHat !! si
              bT   = if isR then 0 else thetaHat !! bCol t
              ell  = if isR then etaI else etaI + thetaHat !! dCol t + bT * (etaI - mx)
              p    = sigmoid ell
              w    = max 1e-9 (fromIntegral ns * p * (1 - p))
              jac  | isR       = setAt (replicate nP 0) si 1
                   | otherwise = setAt (setAt (setAt (replicate nP 0) si (1 + bT)) (dCol t) 1)
                                       (bCol t) (etaI - mx)
              z    = sum (zipWith (*) jac thetaHat) + (fromIntegral ev - fromIntegral ns * p) / w
          in Right (jac, z, 1 / w)
        _ -> Left "nmaRegressionBaselineRiskBinExchGC: expects binary arms"
  arms <- concat <$> mapM (\s -> mapM (lin s) (getStudyArms s)) studies
  let addM m i j d = [ [ if r == i && c == j then (m!!r!!c) + d else m!!r!!c | c <- [0..nP-1] ] | r <- [0..nP-1] ]
      -- Gaussian partition function at (τ², σ_B²): A = JᵀW_eff J + slope-spring ridge 1/σ_B²
      solveCell t2 s2 =
        let t2arm = t2 / 2
            a0   = [ [ sum [ (r!!i) * (r!!j) / (v + t2arm) | (r, _, v) <- arms ] | j <- [0..nP-1] ] | i <- [0..nP-1] ]
            invS = 1 / s2
            aMat = foldl' (\m t -> let bt = bCol t; bb = commonCol in
                            addM (addM (addM (addM m bt bt invS) bb bb invS) bt bb (negate invS)) bb bt (negate invS))
                          a0 nonref
            bVec = [ sum [ (r!!i) * z / (v + t2arm) | (r, z, v) <- arms ] | i <- [0..nP-1] ]
            cov   = matInv aMat
            theta = matVec cov bVec
            sumLogV = sum [ log (v + t2arm) | (_, _, v) <- arms ]
            zWz     = sum [ z*z / (v + t2arm) | (_, z, v) <- arms ]
            logZ    = -0.5 * (sumLogV + logDetPD aMat + (zWz - sum (zipWith (*) bVec theta)))
        in (logZ, theta, cov)
      tau2hi = max (brTau2 fit * 12) 0.02
      ts = [ tau2hi * (fromIntegral i - 0.5) / fromIntegral nGrid | i <- [1 .. nGrid] ]
      nSig  = 40 :: Int
      sigHi = case slopePrior of UniformSD u -> u*u; _ -> 1.0
      sLo   = sigHi / 1e4
      ss    = [ sLo * (sigHi/sLo) ** ((fromIntegral i - 0.5) / fromIntegral nSig) | i <- [1 .. nSig] ]
      cells = [ let (lz, th, cv) = solveCell t s
                    -- −½ n_T log σ_B² is the slope-prior normalisation; +log s is the
                    -- log-grid (dσ_B² = σ_B² d log σ_B²) Jacobian.
                    lp = lz - 0.5 * fromIntegral nT * log s
                         + logPriorT2 prior t + logPriorT2 slopePrior s + log s
                in (lp, t, s, th, cv)
              | t <- ts, s <- ss ]
      lps = [ lp | (lp,_,_,_,_) <- cells ]
      mxlp = maximum lps
      raw = [ exp (lp - mxlp) | lp <- lps ]; tot = sum raw; ws = map (/ tot) raw
      thetas = [ th | (_,_,_,th,_) <- cells ]; covs = [ cv | (_,_,_,_,cv) <- cells ]
      summ c = let m  = sum (zipWith (\w th -> w * (th!!c)) ws thetas)
                   s2 = sum (zipWith3 (\w th cv -> w * ((cv!!c!!c) + (th!!c)^(2::Int))) ws thetas covs) - m*m
                   (lo,hi) = mixtureCI ws (map (!!c) thetas) (map (\cv -> cv!!c!!c) covs)
               in (m, sqrt (max 0 s2), lo, hi)
      scells = [ (s,w) | ((_,_,s,_,_),w) <- zip cells ws ]
      tcells = [ (t,w) | ((_,t,_,_,_),w) <- zip cells ws ]
      momentSumm pairs = let m = sum [ v*w | (v,w) <- pairs ]
                         in (m, sqrt (max 0 (sum [ v*v*w | (v,w) <- pairs ] - m*m)),
                             wquantile pairs 0.025, wquantile pairs 0.975)
      marg pairs = fst $ maximumBy (\x y -> compare (snd x) (snd y))
                         (Map.toList (Map.fromListWith (+) pairs))
      (sMean, sSd, sLo95, sHi95) = momentSumm scells
      (tMean, _,   tLo95, tHi95) = momentSumm tcells
  return BaselineExchGC
    { begMx = mx
    , begSigmaB2 = (marg scells, sMean, sLo95, sHi95)
    , begTau2    = (marg tcells, tMean, tLo95, tHi95)
    , begB       = summ commonCol
    , begBt      = Map.fromList [ (t, summ (bCol t)) | t <- nonref ]
    , begEMSigmaB2 = fromMaybe 0 (brSigmaB2 fit)
    }

-- | EXCHANGEABLE-β BINARY network meta-regression by PQL: the IRLS binomial
-- linearisation interleaved with the two-tau-group EM of 'nmaRegressionGroupedREML'
-- (heterogeneity τ² = group 0, slope dispersion σ_B² = group 1). Returns
-- (τ² per group, common slope B, per-treatment slopes, log-OR contrasts at x=0).
nmaRegressionBinGroupedREML ::
  [Study] -> Map.Map StudyId Double ->
  Either String ( Map.Map TauGroup TauSquare, Double
                , Map.Map TreatmentId Double, Map.Map TreatmentId (Map.Map TreatmentId Double) )
nmaRegressionBinGroupedREML studies covmap =
  let treatments = sort $ nub $ concatMap (map tidOfArm . getStudyArms) studies
      ref = head treatments; nonref = filter (/= ref) treatments; nT = length nonref
      sids = map getStudyId studies; nS = length sids
      sidx = Map.fromList (zip sids [0..]); tidx = Map.fromList (zip nonref [0..])
      betaBase = nS + nT; dCol t = nS + tidx Map.! t; bCol t = betaBase + tidx Map.! t
      commonCol = betaBase + nT; nP = nS + 2*nT + 1
      setAt xs i v = take i xs ++ [v] ++ drop (i+1) xs
      armInfo study a = case a of
        BinaryArm t ev ns ->
          let si = sidx Map.! getStudyId study; x = Map.findWithDefault 0 (getStudyId study) covmap
              base = setAt (replicate nP 0) si (-1)
              row | t == ref  = base
                  | otherwise = setAt (setAt base (dCol t) 1) (bCol t) x
          in Right (row, fromIntegral ev, fromIntegral ns)
        _ -> Left "nmaRegressionBinGroupedREML: expects binary arms"
   in do
        arms <- concat <$> mapM (\s -> mapM (armInfo s) (getStudyArms s)) studies
        let nArm = fromIntegral (length arms)
            addM m i j d = [ [ if r==i && c==j then (m!!r!!c)+d else m!!r!!c | c<-[0..nP-1] ] | r<-[0..nP-1] ]
            step (theta, t0, t1) =
              let t2arm = t0/2; invSig = 1/t1
                  wz = [ let eta = sum (zipWith (*) r theta); p = sigmoid eta
                             w = max 1e-9 (n*p*(1-p)); z = eta + (e - n*p)/w in (r,z,w)
                       | (r,e,n) <- arms ]
                  wEff w = 1/(1/w + t2arm)
                  a0 = [ [ sum [ wEff w*(r!!i)*(r!!j) | (r,_,w) <- wz ] | j<-[0..nP-1] ] | i<-[0..nP-1] ]
                  aMat = foldl' (\m t -> let bt=bCol t; bb=commonCol in
                            addM (addM (addM (addM m bt bt invSig) bb bb invSig) bt bb (negate invSig)) bb bt (negate invSig))
                          a0 nonref
                  bVec = [ sum [ wEff w*(r!!i)*z | (r,z,w) <- wz ] | i<-[0..nP-1] ]
                  cov = matInv aMat; theta' = matVec cov bVec
                  per (r,z,w) = let resid = z - sum (zipWith (*) r theta')
                                    b = w*t2arm/(1+w*t2arm)
                                in ((b*resid)^2, (1-b)*t2arm + b*b*quadForm r cov)
                  ms = map per wz
                  t0' = max 1e-8 $ 2*(sum (map fst ms)/nArm + sum (map snd ms)/nArm)
                  perT t = let m = theta'!!bCol t - theta'!!commonCol
                           in m*m + cov!!(bCol t)!!(bCol t) + cov!!commonCol!!commonCol - 2*cov!!(bCol t)!!commonCol
                  t1' = max 1e-8 $ sum (map perT nonref) / fromIntegral nT
              in (theta', t0', t1')
            loop p@(th,a,b) it
              | it > (300::Int) = p
              | maximum (zipWith (\x y->abs(x-y)) th th') < 1e-8 && abs (a-a')<1e-8 && abs (b-b')<1e-8 = p'
              | otherwise = loop p' (it+1)
              where p'@(th',a',b') = step p
            (thetaF, tau2, sigB2) = loop (replicate nP 0, 0.1, 0.1) 0
            dOf t = if t==ref then 0 else thetaF!!dCol t
            betaMap = Map.fromList [ (t, thetaF!!bCol t) | t<-nonref ]
            contrasts = Map.fromList [ (t1, Map.fromList [ (t2, dOf t2 - dOf t1) | t2<-treatments ]) | t1<-treatments ]
        return (Map.fromList [(0,tau2),(1,sigB2)], thetaF!!commonCol, betaMap, contrasts)

-- | Grouped-τ² REML for exchangeable-β network meta-regression: estimates the
-- heterogeneity τ² (tau-group 0) and the slope-dispersion σ_B² (tau-group 1) in
-- ONE grouped EM, sharing the heterogeneity machinery. σ_B² folds in exactly as a
-- second tau-group — the slope-springs β_t↔B are tau edges like the StudyEnd↔TauEnd
-- heterogeneity springs (see test/nmr_gc_beta_model.md, test/nmr_exchangeable_springs.hs).
-- (The covariate lever β_t·x_i is a non-±1 incidence, so the solve is dense rather
-- than the spanning-tree 'nma'.) Returns (τ² per group, common slope B, per-treatment
-- slopes, contrasts at x=0).
nmaRegressionGroupedREML ::
  (ArmEffect a b, Gaussian a, Gaussian b) =>
  [Study] ->
  Map.Map StudyId Double ->
  (Arm -> Either String a) ->
  Either String ( Map.Map TauGroup TauSquare      -- ^ 0 ↦ τ² (heterogeneity), 1 ↦ σ_B²
                , Double                           -- ^ common slope B
                , Map.Map TreatmentId Double       -- ^ per-treatment slopes β_t
                , Map.Map TreatmentId (Map.Map TreatmentId Double) )  -- ^ contrasts at x=0
nmaRegressionGroupedREML studies covmap armEffect =
  let treatments = sort $ nub $ concatMap (map tidOfArm . getStudyArms) studies
      ref    = head treatments
      nonref = filter (/= ref) treatments
      nT     = length nonref
      sids   = map getStudyId studies
      nS     = length sids
      sidx   = Map.fromList (zip sids [0 ..])
      tidx   = Map.fromList (zip nonref [0 ..])
      betaBase = nS + nT
      dCol t   = nS + tidx Map.! t
      bCol t   = betaBase + tidx Map.! t
      commonCol = betaBase + nT
      nP = nS + 2*nT + 1
      setAt xs i v = take i xs ++ [v] ++ drop (i+1) xs
      armInfo study a = do
        ef <- armEffect a
        let rel = relatedRelative ef; y = expectation rel; v = variance rel; t = tidOfArm a
            si = sidx Map.! getStudyId study
            x  = Map.findWithDefault 0 (getStudyId study) covmap
            base = setAt (replicate nP 0) si (-1)
            row | t == ref  = base
                | otherwise = setAt (setAt base (dCol t) 1) (bCol t) x
        return (row, y, v)
   in do
        arms <- concat <$> mapM (\s -> mapM (armInfo s) (getStudyArms s)) studies
        let nArm = fromIntegral (length arms)
            addM m i j d = [ [ if r==i && c==j then (m!!r!!c)+d else m!!r!!c | c<-[0..nP-1] ] | r<-[0..nP-1] ]
            solve t0 t1 =
              let t2arm = t0/2
                  a0 = [ [ sum [ (r!!i)*(r!!j)/(v+t2arm) | (r,_,v)<-arms ] | j<-[0..nP-1] ] | i<-[0..nP-1] ]
                  invSig = 1/t1
                  aMat = foldl' (\m t -> let bt=bCol t; bb=commonCol in
                            addM (addM (addM (addM m bt bt invSig) bb bb invSig) bt bb (negate invSig)) bb bt (negate invSig))
                          a0 nonref
                  bVec = [ sum [ (r!!i)*y/(v+t2arm) | (r,y,v)<-arms ] | i<-[0..nP-1] ]
                  cov  = matInv aMat
              in (matVec cov bVec, cov)
            emStep (t0, t1) =
              let t2arm = t0/2
                  (theta, cov) = solve t0 t1
                  -- heterogeneity (group 0): per-arm BLUP δ̂ = b·resid
                  perArm (r,y,v) =
                    let fitted = sum (zipWith (*) r theta); resid = y - fitted
                        b = (1/v)*t2arm/(1 + (1/v)*t2arm)
                        varST = quadForm r cov
                    in ((b*resid)^2, (1-b)*t2arm + b*b*varST)
                  ms = map perArm arms
                  t0' = max 1e-8 $ 2*(sum (map fst ms)/nArm + sum (map snd ms)/nArm)
                  -- slope dispersion (group 1): per-treatment BLUP (β_t − B)
                  perT t = let m = theta!!bCol t - theta!!commonCol
                               vv = cov!!(bCol t)!!(bCol t) + cov!!commonCol!!commonCol
                                    - 2*cov!!(bCol t)!!commonCol
                           in m*m + vv
                  t1' = max 1e-8 $ sum (map perT nonref) / fromIntegral nT
              in (t0', t1')
            loop p it | it > (500::Int) = p
                      | abs (a'-a)<1e-7 && abs (b'-b)<1e-7 = (a',b')
                      | otherwise = loop (a',b') (it+1)
              where (a,b)=p; (a',b')=emStep p
            (tau2, sigB2) = loop (0.1, 0.1) 0
            (theta, _) = solve tau2 sigB2
            dOf t = if t==ref then 0 else theta!!dCol t
            betaMap = Map.fromList [ (t, theta!!bCol t) | t<-nonref ]
            contrasts = Map.fromList [ (t1, Map.fromList [ (t2, dOf t2 - dOf t1) | t2<-treatments ]) | t1<-treatments ]
        return ( Map.fromList [(0,tau2),(1,sigB2)], theta!!commonCol, betaMap, contrasts )

-- | makeBinomialSprings constructs a spring network using exact binomial
-- springs for binary arm data, instead of Gaussian inverse-variance springs.
-- Each arm contributes a BinomialSpring carrying raw (events, sampleSize).
-- Tau springs remain linear (Gaussian heterogeneity on the logit scale).
makeBinomialSprings ::
  [Study] ->
  Maybe G.Vertex ->
  Maybe TauSquare ->
  Either String SpringNetwork
makeBinomialSprings studies mpinnedVertex mtau2' =
  let mtau2 = case mtau2' of
               Just 0 -> Nothing
               Nothing -> Nothing
               Just t -> if t < 0
                            then Nothing
                            else Just t
      !(designs', vs) =
        foldl'
          ( \(des, acvs) study ->
              let sid = getStudyId study
                  arms = getStudyArms study
                  narms = length arms
                  des' = case IM.lookup narms des of
                    Nothing -> IM.insert narms 1 des
                    Just ns -> IM.insert narms (ns+1) des
                  labels = case mtau2 of
                    Nothing ->
                      Set.insert (StudyEnd  sid) $
                        Set.union acvs $
                          Set.fromList $ map (TreatmentEnd . tidOfArm) arms
                    Just _ ->
                      let tauvs =
                            Set.fromList $
                              map
                                ( \a ->
                                    (TauEnd (TauId sid (tidOfArm a)))
                                )
                                arms
                          tidvs = Set.fromList $ map (TreatmentEnd . tidOfArm) arms
                       in Set.insert (StudyEnd sid) $ Set.union acvs $ Set.union tauvs tidvs
               in (des', labels)
             )
          (IM.empty, Set.empty)
          studies :: (IM.IntMap Int, Set.Set SpringVertex)
      vssts = IM.fromList $ zip [1 ..] $ Set.toList vs
      stsvs = Map.fromList $ map swap $ IM.toList vssts
      esprings =
        foldl'
          ( \eaces study ->
              let sid = getStudyId study
                  arms = getStudyArms study
                  studysprings =
                    foldl'
                      ( \acc a -> case a of
                          ContinuousArm {} -> Left "makeBinomialSprings: expected BinaryArm" : acc
                          BinaryArm tid ev ns ->
                            let vs = stsvs Map.! StudyEnd sid
                                vt = stsvs Map.! TreatmentEnd tid
                                spr = BinomialSpring ev ns 0.0
                             in case mtau2 of
                                  Nothing ->
                                    let sttrEdge = G.Edge vs vt
                                     in Right (Right sttrEdge, spr) : acc
                                  Just tau2 ->
                                    let vtau = stsvs Map.! (TauEnd (TauId sid tid))
                                        sttauEdge = G.Edge vs vtau
                                        sttauspr = Spring (2 / tau2) 0 0
                                        tautrEdge = G.Edge vtau vt
                                     in [ Right (Left sttauEdge, sttauspr),
                                          Right (Right tautrEdge, spr)
                                        ]
                                          ++ acc
                      )
                      []
                      arms
                  studysprings' = sequence studysprings
               in case eaces of
                    Left err -> Left err
                    Right aces ->
                      case studysprings' of
                        Left err -> Left err
                        Right sprs -> Right (sprs ++ aces)
          )
          (Right [])
          studies ::
          Either String [(Either G.Edge G.Edge, Spring)]
   in case esprings of
        Left err -> Left err
        Right springs' ->
          let tauEdges = foldl' (\acc (ee, _) ->
                           case ee of
                             Right _ -> acc
                             Left e -> Set.insert e acc
                         ) Set.empty springs'
              springs = map (\(ee, spr) ->
                 case ee of
                   Right e -> (e, spr)
                   Left e -> (e, spr)
                   ) springs'
           in Right SpringNetwork { springs = Map.fromList springs
                                  , vs2ts = vssts
                                  , pinned = mpinnedVertex
                                  , tauEdges = tauEdges
                                  , tauGroupMap = Map.fromSet (const 0) tauEdges
                                  , tau2 = fromMaybe 0.0 mtau2
                                  , tau2s = Map.singleton 0 (fromMaybe 0.0 mtau2)
                                  , qqTau = 0
                                  , qqTau' = 0
                                  , designs = designs'
                                  , dof = degreesOfFreedom vssts designs'
                                  }

-- | nma from springs
nma ::
  SpringNetwork ->
  NetworkEffects
nma springsnet =
  let !vssts = vs2ts springsnet
      !stsvs = Map.fromList $ map swap $ IM.toList vssts
      Just vsts = sequence -- only treatment vertices
        $ IM.map getTreatmentId $ IM.filter isTreatmentEnd vssts 
      tsvs = Map.fromList $ map swap $ IM.toList vsts
      !pinnedV = fromMaybe 1 $ pinned springsnet
      !springsMap = springs springsnet
      !tauEdges' = tauEdges springsnet
      !tau2' = tau2 springsnet 
      !es = Map.keys springsMap
      !numEdges = length es
      !directedSG = G.graphFromEdges es
      !undirectedSG = G.makeUndirected directedSG
      getSpring :: G.Edge -> Spring -- from undirected edge to spring
      getSpring e =
        case Map.lookup e springsMap of
          Nothing -> reverseSpring $ springsMap Map.! G.reverseEdge e
          Just sp -> sp
      -- get the Matrices corresponding to defined pinned Vertex
      getMatrices :: G.Vertex -> Matrices
      getMatrices pinnedVertex =
        -- Networks spanning tree
        let netbfs = BFS.bfs undirectedSG pinnedVertex
            !spnTr = Set.fromList $ BFS.spanningTree netbfs
            !parents = BFS.parent netbfs
            !vss = map (\(G.Edge u v) -> v) (Set.toList spnTr)
            llmap =
              Map.fromList $
                map
                  ( \e ->
                      if Set.member e spnTr then (e, Right $ getSpring e) else (
                        let re = G.reverseEdge e
                         in if Set.member re spnTr
                           then (re, Right $ getSpring re)
                           else (e, Left $ getSpring e))
                  )
                  es
            -- L vector (E><1) with the natural lengths of springs which respect
            -- the spanning tree orientation
            directEdges = Set.toList $ Set.fromList $ Map.keys llmap
            llMatrix =
              map
                ( \de ->
                    let ms = llmap Map.! de
                     in case ms of
                          Left sp -> naturalLength sp
                          Right sp -> naturalLength sp
                )
                directEdges
            -- weight matrix (E x E)
            kMatrix = diagl $ map (hardness . getSpring) directEdges
            -- cMatrix (V-1 x E)
            cMatrix =
              let vertexToRow :: G.Vertex -> [Double]
                  vertexToRow x =
                    map
                      ( \(G.Edge u v) ->
                          if x == u
                            then 1.0
                            else
                              if x == v
                                then (-1)
                                else 0
                      )
                      directEdges
               in fromLists $ map vertexToRow vss
            -- D matrix (V-1 x E)
            dMatrix = cMatrix `mulFast` kMatrix
            -- X matrix (E x (V-1)) consistency matrix
            -- Definition: X <> L* = L
            edgeToRow :: G.Edge -> [Double]
            edgeToRow (G.Edge u v) =
              let r = replicate (length vss) 0
                  r' = IM.fromList $ zip vss r
                  followEnd ::
                    G.Vertex ->
                    IM.IntMap Double ->
                    IM.IntMap Double
                  followEnd k r =
                    if k == pinnedVertex
                      then r
                      else
                        let r' = IM.adjust (+ 1) k r
                            k' = BFS.parent netbfs IM.! k
                         in followEnd k' r'
                  followStart k r =
                    if k == pinnedVertex
                      then r
                      else
                        let r' = IM.adjust (+ (-1)) k r
                            k' = BFS.parent netbfs IM.! k
                         in followStart k' r'
                  r'' = followEnd v $ followStart u r'
               in map (r'' IM.!) vss -- has to respect vss ordering
            !xMatrix = fromLists $ map edgeToRow directEdges
            !dxInv = invFast (dMatrix `mulFast` xMatrix)
            lstar = (dxInv `mulFast` dMatrix) `mulFast` (reshape numEdges 1 llMatrix)
            ls = xMatrix `mulFast` lstar
         in Matrices
              { kMatrix,
                llMatrix,
                cMatrix,
                dMatrix,
                xMatrix,
                dxInv,
                parents,
                spnTr,
                vss,
                directEdges = Set.fromList directEdges,
                lstar,
                ls
              }
      -- l* (V-1 >< 1) where V-1 are the end vertices of the spanning
      -- tree
      !mtrcs = getMatrices pinnedV
      kMat = kMatrix mtrcs
      llMat = reshape numEdges 1 (llMatrix mtrcs)
      cMat = cMatrix mtrcs
      dMat = dMatrix mtrcs
      xMat = xMatrix mtrcs
      parents' = parents mtrcs
      spnTr' = spnTr mtrcs
      vss' = vss mtrcs
      lstar' = lstar mtrcs -- the solution basically :)
      lss = ls mtrcs
      !directEdges' = Set.toList $ directEdges mtrcs
      lstarMap = Map.fromList $ zip (Set.toList spnTr') (flatten lstar')
      -- | rMatrix : R <> Ls = Lτ : numTaus x numEdges
      rMatrix = fromLists (map (\te -> map
                                    (\de -> if te == de
                                        then 1.0
                                        else if G.reverseEdge te == de 
                                          then -1.0 
                                          else 0.0
                                    ) (directEdges'))
                              (Set.toList tauEdges'))
      ltau = rMatrix <> lss
      ltauTr = tr ltau
      qqTau =  atIndex ((scale (1/tau2') ltauTr ) <> ltau ) (0, 0)
      kk' =  -- derivative of kMatrix
        diagl $ map (\de -> 
          let ki = hardness $ getSpring de
           in if Set.member de tauEdges'
                 || Set.member (G.reverseEdge de) tauEdges'
                 then -1/(tau2'^2)
                 else ki
        ) directEdges'
      dd' = cMat <> kk'
      !ddxxInv = dxInv mtrcs
      ltau' = -- derivative of lτ
        rMatrix <> xMat <>
          ((ddxxInv <> dd' <> llMat)
          `mSub` (ddxxInv <> dd' <> xMat <> ddxxInv <> dMat <> llMat))
      qqTau' = -- derivative of Qτ
        atIndex (((scale (-1 / (tau2'^2)) ltauTr)
          `mAdd` (scale (1/tau2') ltau')) <> ltau
        `mAdd` ((scale (1/tau2') ltauTr) <> ltau')
          ) (0, 0)
      getLength ::
        G.Edge ->
        G.Vertex -> -- pinned Vertex
        Map.Map G.Edge Double -> -- lstarMap
        IM.IntMap G.Vertex -> -- parents
        NaturalLength
      -- follows back the spanning tree adding parent edges' length
      getLength (G.Edge u v) pinnedV lstarMap parents =
        let followEnd ::
              G.Vertex ->
              Double ->
              Double
            followEnd k l =
              if k == pinnedV
                then l
                else
                  let k' = parents IM.! k
                      l' = lstarMap Map.! G.Edge k' k
                   in followEnd k' (l + l')
            followStart k l =
              if k == pinnedV
                then l
                else
                  let k' = parents IM.! k
                      l' = lstarMap Map.! G.Edge k' k
                   in followStart k' (l - l')
         in if u == v
              then 0
              else followEnd v $ followStart u 0
      !netests =
        foldl'
          ( \acc utid ->
              let vmap =
                    foldl'
                      ( \mac vtid ->
                          let u = tsvs Map.! utid
                              v = tsvs Map.! vtid
                              l = getLength (G.Edge u v) pinnedV lstarMap parents'
                           in Map.insert vtid l mac
                      )
                      Map.empty
                      $ Map.keys tsvs
               in Map.insert utid vmap acc
          )
          Map.empty
          $ Map.keys tsvs
      effectiveVariance ::
        G.Edge ->
        G.Vertex -> -- pinned
        [G.Vertex] -> -- vss
        Set.Set G.Edge -> -- spanning tree
        Matrix -> -- (DX)^{-1} cached
        IM.IntMap G.Vertex -> -- parents
        Hardness
      effectiveVariance (G.Edge u v) pinnedV vss spnTr dxInvMat parents =
        if u == v
          then 0
          else
            let r = IM.fromList $ zip vss $ replicate (length vss) 0
                r' = case IM.lookup u r of
                  Nothing -> r
                  Just _ -> IM.insert u (-1.0) r
                r'' = case IM.lookup v r' of
                  Nothing -> r'
                  Just _ -> IM.insert v 1.0 r'
                forceVector = tr $ fromLists [map (r'' IM.!) vss] -- has to respect vss ordering
                eflstar = dxInvMat <> forceVector
                eflstarMap =
                  Map.fromList $
                    zip (Set.toList spnTr) (flatten eflstar)
             in abs $ getLength (G.Edge u v) pinnedV eflstarMap parents
      netvars =
        foldl'
          ( \acc utid ->
              let vmap =
                    foldl'
                      ( \mac vtid ->
                          let u' = tsvs Map.! utid
                              v' = tsvs Map.! vtid
                              u = min u' v'
                              v = max u' v'
                              mtc = getMatrices u
                              l = effectiveVariance
                                    (G.Edge u v)
                                    pinnedV
                                    vss'
                                    spnTr'
                                    ddxxInv
                                    parents'
                           in Map.insert vtid l mac
                      )
                      Map.empty
                      $ Map.keys tsvs
               in Map.insert utid vmap acc
          )
          Map.empty
          $ Map.keys tsvs
      sg =
        StudyGraph
          { studyGraph = G.graphFromEdges [],
            directGraph = G.graphFromEdges $ Set.toList spnTr',
            vsts = vsts,
            tsvs = tsvs
          }
      -- Var(δ_i) for each study from the transfer matrix.
      -- The tau spring lengths are: ltau = T × L where T = R × X × (DX)⁻¹ × D
      -- Var(ltau) = T × Var(L) × T'
      -- Var(L) = diag(v_j) for direct springs, 0 for tau springs
      -- (natural lengths of tau springs are fixed at 0, no data variance)
      transferMatrix = rMatrix <> xMat <> ddxxInv <> dMat -- numTaus × numEdges
      -- Variance of natural lengths: 1/k_j for all springs.
      -- Direct springs: Var(y_ij) = v_ij = 1/k_j
      -- Tau springs: Var = τ²/2 = 1/k_τ (the random effect fluctuation)
      varL = diagl $ map (( 1 /) . hardness . getSpring) directEdges'
      -- Var(ltau) = T × Var(L) × T'  (numTaus × numTaus)
      varLtau = transferMatrix <> varL <> tr transferMatrix
      -- Per-study Var(δ_i): for 2-arm, δ_i = ltau_1 - ltau_2
      -- Var(δ_i) = Var(ltau_1) + Var(ltau_2) - 2 Cov(ltau_1, ltau_2)
      -- For multi-arm: sum of all pairwise contrast variances
      tauEdgesList = Set.toList tauEdges'
      studyVsList = IM.keys $ IM.filter isStudyEnd vssts
      studyDeltaVars = IM.fromList $ map (\sv ->
        let -- indices of tau edges for this study in tauEdgesList
            studyTauIdxs = [idx | (idx, G.Edge u v) <- zip [0..] tauEdgesList,
                            u == sv || v == sv]
            -- Per-arm Var(δ_ij): diagonal elements of Var(ltau)
            armVars = [atIndex varLtau (i, i) | i <- studyTauIdxs]
         in (sv, armVars)
        ) studyVsList
      updatedSpringsNet = springsnet {
          springs = Map.mapWithKey (\e spr ->
                      let l = getLength e pinnedV lstarMap parents'
                       in updateLength spr l
                  ) springsMap
        , qqTau = qqTau
        , qqTau' = qqTau'
      }
   in NetworkEffects
          { directs = sg,
            networkEstimates = netests,
            networkVariances = netvars,
            studyDeltaVariances = studyDeltaVars,
            springNetwork = updatedSpringsNet,
            debugmsg =
              unlines [
               --"springsNet" <> show (updatedSpringsNet)
                --, "spnTr " <> show ( Set.toList spnTr')
                --, "vss " <> show ( map (\(G.Edge u v) -> v) (Set.toList spnTr'))
                --, "netvars" <> show netvars
                 --"rMatrix" <> show rMatrix
                 --,"kMatrix" <> show kMat
                 --,"tauEdges" <> show ltau
                 --,"ts" <> show tauEdges'
                 --,"tauEdges^2" <> show ((tr ltau) <> (ident numEdges) <> ltau)
                  "Vdis" ++ "tba"
                  --"Qt" <> show qqTau
                --, "directEdges" <> show directEdges'
                ]
          }

-- | updateSprings update tau springs' hardness
updateSprings :: SpringNetwork -> TauSquare -> SpringNetwork
updateSprings oldSpringsNet newTau2 = 
  let oldSprings = springs oldSpringsNet
      !newSprings = 
        foldl' (\acc e -> Map.adjust (\s -> updateHardness s  (newTau2 / 2)) e acc)
          oldSprings (Set.toList $ tauEdges oldSpringsNet)
   in oldSpringsNet { tau2 = newTau2
                    , springs = newSprings
                    }

-- | Update tau springs using per-group τ² values and per-edge multipliers.
-- Each tau edge looks up its group (default 0) and the group's τ², then a
-- per-edge multiplier (default 2 = symmetric, 1 = Dias anchored).
updateSpringsGrouped ::
  SpringNetwork ->
  Map.Map TauGroup TauSquare ->  -- τ² per group (contrast-level)
  Map.Map G.Edge Double ->       -- per-edge multiplier
  SpringNetwork
updateSpringsGrouped oldSpringsNet newTau2s edgeMults =
  let oldSprings = springs oldSpringsNet
      grpMap = tauGroupMap oldSpringsNet
      !newSprings =
        foldl' (\acc e ->
          let grp = Map.findWithDefault 0 e grpMap
              t2 = Map.findWithDefault 0.5 grp newTau2s
              mult = Map.findWithDefault 2.0 e edgeMults
           in Map.adjust (\s -> updateHardness s (t2 / mult)) e acc)
          oldSprings (Set.toList $ tauEdges oldSpringsNet)
      defaultTau2 = Map.findWithDefault 0.5 0 newTau2s
   in oldSpringsNet { tau2 = defaultTau2
                    , tau2s = newTau2s
                    , springs = newSprings
                    }

-- | Set tau groups based on a classification function.
-- The function receives (StudyId, TreatmentId) for each tau spring
-- and returns the group assignment.
setTauGroups :: (StudyId -> TreatmentId -> TauGroup) -> SpringNetwork -> SpringNetwork
setTauGroups classify net =
  let vssts = vs2ts net
      newGrpMap = Map.fromList
        [ (e, classify sid tid)
        | e@(G.Edge u v) <- Set.toList (tauEdges net)
        , let tauVertex = case IM.lookup v vssts of
                Just (TauEnd _) -> v
                _ -> u  -- try the other end
              mTauId = case IM.lookup tauVertex vssts of
                Just (TauEnd (TauId sid tid)) -> Just (sid, tid)
                _ -> Nothing
        , Just (sid, tid) <- [mTauId]
        ]
   in net { tauGroupMap = newGrpMap }

-- | unsafely remove study given the index is that of a study. Use with
-- netStudyies
removeStudy :: SpringNetwork -> Int -> SpringNetwork
removeStudy oldspringsnet studyVertex =
  let oldvssts = vs2ts oldspringsnet
      oldsprings = springs oldspringsnet
      tauedgestoremove = Set.filter (\(G.Edge u v) -> u == studyVertex || v == studyVertex) 
                       $ tauEdges oldspringsnet 
      newtauedges = tauEdges oldspringsnet Set.\\ tauedgestoremove
      tauvertices = map (\(G.Edge u v)
                             -> if u == studyVertex then v
                                                    else u
                            ) $ Set.toList tauedgestoremove
      newvertices = IM.withoutKeys oldvssts (IS.fromList tauvertices) 
      newsprings = Map.withoutKeys oldsprings tauedgestoremove 
      updatedsprings = Map.fromList $ 
                       map (\((G.Edge u v), spring) -> 
                         let newedge = if elem u tauvertices 
                                          then  
                                            G.Edge studyVertex v
                                          else 
                                            if elem v tauvertices 
                                              then 
                                                G.Edge u studyVertex
                                              else 
                                                G.Edge u v 
                          in (newedge, spring)
                       ) 
                     $ Map.toList newsprings
   in oldspringsnet { vs2ts = newvertices
                    , springs = updatedsprings
                    , tauEdges = newtauedges
                    }

springNMA ::
  (ArmEffect a b, Gaussian a) =>
  [Study] ->
  Maybe G.Vertex -> 
  Maybe TauSquare ->
  (Arm -> Either String a) ->
  Either String NetworkEffects
springNMA studies mpinned mtau2 armEffect =
  let espringnet = makeSprings studies Nothing mtau2 armEffect
   in case espringnet of
      Left err -> Left err
      Right springsnet -> Right $ nma springsnet

