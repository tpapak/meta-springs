{-|
Module      : Data.Meta.Multiarm
Description : Matrix function for graphs
Copyright   : (c) Thodoris Papakonstantinou, 2019
License     : GPL-3
Maintainer  : mail@tpapak.com
Stability   : experimental
Portability : POSIX

-}

module Data.Meta.Matrices
  ( createBMatrix
  , createAMatrix
  , createCMatrix
  )
where

import           Control.Applicative
import           Data.List
import qualified Data.Map.Strict               as Map
import qualified Data.Set                      as Set
import           Data.Maybe
import           Data.Either
import           Prelude hiding ((<>))
import           Data.Meta.Effects
import           Data.Meta.Matrix
import qualified Data.Graph.AdjacencyList      as G
import qualified Data.Graph.AdjacencyList.BFS      as BFS

-- | create the edge vertex adjacency matrix given a graph
createBMatrix :: G.Graph -> Matrix
createBMatrix g =
  let vs  = G.vertices g
      nvs = length vs
      es  = G.edges g
      nes = length es
      rowfromEdge e =
          let
            (u, v) = G.toTuple e
            r      = replicate nvs 0.0 :: Vector
            r' =
              mapVectorWithIndex
                (\i _ ->
                  if i == u - 1 then 1.0 else if i == v - 1 then (-1.0) else 0.0
                )
                r :: Vector
          in
            r'
      rs = map rowfromEdge es
      b    = fromRows rs
  in  b

createAMatrix :: G.Graph -> Matrix
createAMatrix g =
  let vs  = G.vertices g
      nvs = length vs
      es  = G.edges g
      nes = length es
      rowfromEdge e =
          let
            (u, v) = G.toTuple e
            r      = replicate (nvs - 1) 0.0 :: Vector
            r' =
              mapVectorWithIndex
                (\i _ ->
                  if i == u - 2 then 1.0 else if i == v - 2 then (-1.0) else 0.0
                )
                r :: Vector
           in r'
      rs = map rowfromEdge es
      a    = fromRows rs
  in  a

-- | create the Vertex Edge adjacency matrix given a graph

-- | create the Vertex Edge adjacency matrix given a graph
createCMatrix :: G.Graph -> Matrix
createCMatrix g =
  let vs  = G.vertices g
      nvs = length vs
      es  = G.edges g
      nes = length es
      rowfromVertex v =
        let r = map (\e ->
              let (u, v') = G.toTuple e
                  fillcell x | x==u = (-1.0)
                  fillcell x | x==v' = (1.0)
                  fillcell _ = (0.0)
               in  fillcell v
                  ) es
         in r :: Vector
      rs = map rowfromVertex vs
      c    = fromRows rs
  in  c

-- | create the Vertex Edge adjacency matrix given a graph
createXMatrix :: G.Graph -> Matrix
createXMatrix g =
  let vs  = G.vertices g
      nvs = length vs
      es  = G.edges g
      nes = length es
      firstVertex = head vs
      bfsNet = BFS.bfs g firstVertex
      spanningtree = BFS.spanningTree bfsNet
      rowfromVertex v =
        let r = map (\e ->
              let (u, v') = G.toTuple e
                  fillcell x | x==u = (-1.0)
                  fillcell x | x==v' = (1.0)
                  fillcell _ = (0.0)
               in  fillcell v
                  ) es
         in r :: Vector
      rs = map rowfromVertex vs
      c    = fromRows rs
  in  c
