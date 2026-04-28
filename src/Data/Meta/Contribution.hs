{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Contribution matrix via the Papakonstantinou (2018) shortest-path
-- streams decomposition.
--
-- Self-contained port of the shortest-path arm of
-- @../nma-contribution/src/Data/NMA/Contribution.hs@. We only support
-- the BFS (shortest-edge) stream finder; @findAStream@ and
-- @longestStream@ are intentionally absent.
--
-- API:
--
-- * 'shortestStream' — BFS, fewest-edge augmenting path.
-- * 'contributionRow' / 'contributionMatrix' — repeatedly extract
--   shortest streams until none remain; return contributions, the
--   stream list, and the residual graph.
-- * 'studyContributions' — redistribute contrast contributions across
--   studies using per-study IV weights.

module Data.Meta.Contribution
  ( -- * Hat matrix shape (re-exported from "Data.Meta.Hatmatrix")
    Hat.HatMatrix
  , Hat.HatMatrixRow
  , Hat.HatMatrixRaw
  , Hat.hatMatrixFromList

    -- * Streams + contribution rows
  , Stream(..)
  , ContributionRow
  , ContributionMatrix(..)
  , StreamMatrix(..)
  , HatRowGraph(..)
  , mkHatRowGraph
  , minCapacity
  , residualFlows
  , shortestStream
  , contributionRow
  , contributionMatrix
  , contributionRowShortestPath
  , contributionMatrixShortestPath
  , sumContributionRow

    -- * Per-study contributions
  , PairwiseWeight(..)
  , StudyContrib(..)
  , studyContributions
  ) where

import           Data.Aeson           (ToJSON (..), object, (.=))
import qualified Data.Map.Strict      as Map
import           Data.Maybe           (fromMaybe)
import qualified Data.Sequence        as Seq
import           Data.List            (foldl', sortBy)
import           GHC.Generics         (Generic)

import qualified Data.Meta.Effects    as MetaEff
import qualified Data.Meta.Hatmatrix  as Hat

-- | Capacity below which an edge is treated as removed. Matches the
-- @1e-8@ cutoff in the @nma-contribution@ reference's @updateFlow@.
eps :: Double
eps = 1e-8

-- ─────────────────────────────────────────────────────────────────────
-- Streams + residual graph
-- ─────────────────────────────────────────────────────────────────────

-- | A directed source→sink path through the residual graph plus the
-- minimum capacity along it (the flow contributed by this stream).
data Stream =
  Stream { path :: ![(MetaEff.TreatmentId, MetaEff.TreatmentId)]
         , flow :: !Double
         }
  deriving (Eq, Show, Generic)

-- | Per-row contributions, keyed by direct-comparison id (using whichever
-- direction the original hat row registered — typically @a:b@ with
-- @a < b@).
type ContributionRow = Map.Map MetaEff.ComparisonId Double

newtype ContributionMatrix =
  ContributionMatrix (Map.Map MetaEff.ComparisonId ContributionRow)
  deriving (Eq, Show)

-- | Wide-form JSON output identical to the @nma-contribution@ binary so
-- existing CINeMA pipelines stay compatible.
instance ToJSON ContributionMatrix where
  toJSON (ContributionMatrix cm) =
    object
      [ "contributionmatrix" .=
          [ object [ "row"        .= show rid
                   , "comparison" .= show cid
                   , "value"      .= v
                   ]
          | (rid, cr) <- Map.toList cm
          , (cid, v) <- Map.toList cr
          ]
      ]

-- | Per-row stream list — the path contributions emitted by a single
-- decomposition.
newtype StreamMatrix =
  StreamMatrix (Map.Map MetaEff.ComparisonId [Stream])
  deriving (Eq, Show)

instance ToJSON StreamMatrix where
  toJSON (StreamMatrix sm) =
    object
      [ "streammatrix" .=
          [ object [ "row"          .= show rid
                   , "length"       .= length (path s)
                   , "path"         .= map showEdge (path s)
                   , "contribution" .= flow s
                   ]
          | (rid, ss) <- Map.toList sm
          , s <- ss
          ]
      ]
    where
      showEdge (u, v) = show u ++ "->" ++ show v

-- | Internal directed-graph representation for one hat row.
data HatRowGraph =
  HatRowGraph
    { source       :: !MetaEff.TreatmentId
    , sink         :: !MetaEff.TreatmentId
    , caps         :: !(Map.Map MetaEff.TreatmentId
                          (Map.Map MetaEff.TreatmentId Double))
    , contribution :: !ContributionRow
    }
  deriving (Eq, Show)

-- | Canonical orientation matching whichever key the contribution map
-- originally registered (typically @a:b@ with @a < b@).
canonicalKey :: ContributionRow
             -> MetaEff.TreatmentId
             -> MetaEff.TreatmentId
             -> Maybe MetaEff.ComparisonId
canonicalKey contrib u v =
  let fwd = MetaEff.ComparisonId u v
      rev = MetaEff.ComparisonId v u
  in if Map.member fwd contrib then Just fwd
     else if Map.member rev contrib then Just rev
     else Nothing

-- | Build the directed residual graph for one hat row. Entries with
-- @|h| ≤ eps@ are dropped (would never be selected anyway).
mkHatRowGraph :: MetaEff.ComparisonId
              -> Hat.HatMatrixRow
              -> HatRowGraph
mkHatRowGraph (MetaEff.ComparisonId src dst) row =
  let contrib = Map.map (const 0.0) row
      addEdge accCaps cid h =
        let MetaEff.ComparisonId a b = cid
            mag                       = abs h
            (from, to)                = if h > 0 then (a, b) else (b, a)
        in if mag <= eps
              then accCaps
              else Map.insertWith Map.union from (Map.singleton to mag) accCaps
      capsMap = Map.foldlWithKey' addEdge Map.empty row
  in HatRowGraph
       { source       = src
       , sink         = dst
       , caps         = capsMap
       , contribution = contrib
       }

-- | Minimum residual capacity along a path. Empty path → 0.
minCapacity :: HatRowGraph
            -> [(MetaEff.TreatmentId, MetaEff.TreatmentId)]
            -> Double
minCapacity _ []   = 0
minCapacity hgr xs =
  let look (u, v) = fromMaybe 0 (Map.lookup u (caps hgr) >>= Map.lookup v)
  in minimum (map look xs)

-- | Flatten remaining capacities — used for residue diagnostics.
residualFlows :: HatRowGraph -> [Double]
residualFlows hgr =
  [ c | neis <- Map.elems (caps hgr), c <- Map.elems neis ]

-- | Apply one stream: subtract its flow from each path edge, drop edges
-- below @eps@, and accumulate @φ / L@ to the canonical column.
applyStream :: HatRowGraph -> Stream -> HatRowGraph
applyStream hgr (Stream pth phi)
  | null pth || phi <= eps = hgr
  | otherwise =
      let l        = fromIntegral (length pth) :: Double
          newCaps  = foldl' drainEdge (caps hgr) pth
          newCont  = foldl' (addEdgeContrib (phi / l) (contribution hgr))
                            (contribution hgr) pth
      in hgr { caps = newCaps, contribution = newCont }
  where
    drainEdge accCaps (u, v) =
      Map.alter (fmap (drainNeighbour v)) u accCaps

    drainNeighbour v neis =
      Map.update
        (\c -> let nc = c - phi
               in if nc < eps then Nothing else Just nc)
        v neis

    addEdgeContrib incr origContrib accContrib (u, v) =
      case canonicalKey origContrib u v of
        Nothing  -> accContrib
        Just key -> Map.adjust (+ incr) key accContrib

-- ─────────────────────────────────────────────────────────────────────
-- Stream finder
-- ─────────────────────────────────────────────────────────────────────

-- | BFS shortest stream — port of upstream @shortestStream@.
shortestStream :: HatRowGraph -> Maybe Stream
shortestStream hgr
  | source hgr == sink hgr = Nothing
  | otherwise =
      let initParent = Map.singleton (source hgr) (source hgr)
          (parents, found) =
            bfs (Seq.singleton (source hgr)) initParent Nothing
      in if not found
           then Nothing
           else let pth = reconstruct parents (sink hgr) []
                    phi = minCapacity hgr pth
                in if phi <= eps
                      then Nothing
                      else Just (Stream pth phi)
  where
    bfs queue parents found =
      case Seq.viewl queue of
        Seq.EmptyL    -> (parents, fromMaybe False found)
        u Seq.:< rest ->
          let neis = fromMaybe Map.empty (Map.lookup u (caps hgr))
              (q', p', f') =
                Map.foldlWithKey' (visit u) (rest, parents, found) neis
          in case f' of
               Just True  -> (p', True)
               _          -> bfs q' p' f'

    visit _ (q, p, Just True) _ _ = (q, p, Just True)
    visit u (q, p, _)         v c
      | c <= eps        = (q, p, Just False)
      | Map.member v p  = (q, p, Just False)
      | otherwise       =
          let p' = Map.insert v u p
          in if v == sink hgr
                then (q, p', Just True)
                else (q Seq.|> v, p', Just False)

    reconstruct p cur acc
      | cur == source hgr = acc
      | otherwise =
          case Map.lookup cur p of
            Just u | u /= cur ->
              reconstruct p u ((u, cur) : acc)
            _ -> acc

-- ─────────────────────────────────────────────────────────────────────
-- Row / matrix decomposition
-- ─────────────────────────────────────────────────────────────────────

-- | Per-row decomposition. Iterates 'shortestStream' until no
-- augmenting path remains. Returns the contributions, the streams
-- collected, and the residual graph (so callers can inspect remaining
-- flows).
contributionRow :: MetaEff.ComparisonId
                -> Hat.HatMatrixRow
                -> (ContributionRow, [Stream], HatRowGraph)
contributionRow rid row =
  let g0 = mkHatRowGraph rid row
  in loop 0 g0 []
  where
    loop !n hgr streams
      | n >= 100000 = (contribution hgr, reverse streams, hgr)
      | otherwise =
          case shortestStream hgr of
            Nothing -> (contribution hgr, reverse streams, hgr)
            Just s
              | null (path s) || flow s <= eps ->
                  (contribution hgr, reverse streams, hgr)
              | otherwise ->
                  loop (n + 1) (applyStream hgr s) (s : streams)

-- | Apply 'contributionRow' to every row of the hat matrix.
contributionMatrix :: Hat.HatMatrix
                   -> ( ContributionMatrix
                      , StreamMatrix
                      , Map.Map MetaEff.ComparisonId HatRowGraph
                      )
contributionMatrix hm =
  let rows = [ (rid, contributionRow rid row)
             | (rid, row) <- Map.toList hm ]
      cm = Map.fromList [ (rid, cr) | (rid, (cr, _, _)) <- rows ]
      sm = Map.fromList [ (rid, ss) | (rid, (_, ss, _)) <- rows ]
      gs = Map.fromList [ (rid, g)  | (rid, (_, _, g))  <- rows ]
  in (ContributionMatrix cm, StreamMatrix sm, gs)

-- ── Convenience wrappers ──────────────────────────────────────────────

contributionRowShortestPath :: MetaEff.ComparisonId
                            -> Hat.HatMatrixRow
                            -> ContributionRow
contributionRowShortestPath rid row =
  let (cr, _, _) = contributionRow rid row in cr

contributionMatrixShortestPath :: Hat.HatMatrix -> ContributionMatrix
contributionMatrixShortestPath hm =
  let (cm, _, _) = contributionMatrix hm in cm

-- | Sum of contributions in a row.
sumContributionRow :: ContributionRow -> Double
sumContributionRow = sum . Map.elems

-- ─────────────────────────────────────────────────────────────────────
-- Per-study contributions
-- ─────────────────────────────────────────────────────────────────────

-- | One study's IV weight on one direct comparison.
data PairwiseWeight =
  PairwiseWeight
    { pwStudy      :: !String
    , pwComparison :: !MetaEff.ComparisonId
    , pwWeight     :: !Double
    }
  deriving (Eq, Show)

-- | One row of the per-study contribution table:
-- @(networkComparison, study, contribution)@. Mirrors the dataframe
-- shape returned by netmeta's @netcontrib(study = TRUE)@.
data StudyContrib =
  StudyContrib
    { scComparison   :: !MetaEff.ComparisonId
    , scStudy        :: !String
    , scContribution :: !Double
    }
  deriving (Eq, Show)

-- | Redistribute the per-comparison contributions across studies in
-- proportion to each study's IV weight on that comparison. The returned
-- table aggregates @(networkComparison, study)@ — each pair appears at
-- most once with the contribution summed across the comparisons that
-- study informs.
studyContributions :: ContributionMatrix
                   -> [PairwiseWeight]
                   -> [StudyContrib]
studyContributions (ContributionMatrix cm) ws =
  let total :: Map.Map MetaEff.ComparisonId Double
      total = foldl'
        (\m w -> Map.insertWith (+) (pwComparison w) (pwWeight w) m)
        Map.empty ws
      per   :: Map.Map (MetaEff.ComparisonId, String) Double
      per   = foldl'
        (\m w -> Map.insertWith (+) (pwComparison w, pwStudy w) (pwWeight w) m)
        Map.empty ws
      step acc (rid, cr) =
        Map.foldlWithKey' (rowStep rid) acc cr
      rowStep rid acc cid cval =
        case Map.lookup cid total of
          Nothing  -> acc
          Just tot
            | tot <= 0  -> acc
            | otherwise ->
                Map.foldlWithKey'
                  (\a (cmp, st) w ->
                     if cmp /= cid
                        then a
                        else Map.insertWith (+) (rid, st)
                                            (cval * w / tot) a)
                  acc per
      table = foldl' step Map.empty (Map.toList cm)
      rows  = [ StudyContrib rid st v
              | ((rid, st), v) <- Map.toList table ]
  in sortBy (\a b ->
       compare (scComparison a) (scComparison b)
       <> compare (scStudy a) (scStudy b)) rows
