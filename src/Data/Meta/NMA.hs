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
    TauId (..),
    makeSprings,
    makeBinomialSprings,
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
                , tau2 :: TauSquare
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
                              , tau2 = fromMaybe 0.0 mtau2
                              , qqTau = 0
                              , qqTau' = 0
                              , designs = designs'
                              , dof = degreesOfFreedom vssts designs'
                              }

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
                                  , tau2 = fromMaybe 0.0 mtau2
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

