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

