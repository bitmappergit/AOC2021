{-# LANGUAGE BlockArguments, OverloadedLists, OverloadedRecordDot, NoFieldSelectors #-}

module Graph where

import Data.Map as Map

data Hypergraph index node
  = Hypergraph { nodes :: Map index node
               , edges :: Map index [index]
               } deriving (Show, Eq)

data Node index value 
  = Node { index :: index
         , value :: value
         } deriving (Show, Eq)

data Edge index
  = Edge { indices :: [index]
         } deriving (Show, Eq)

emptyHypergraph :: Hypergraph index value 
emptyHypergraph =
  Hypergraph { nodes = Map.empty
             , edges = Map.empty
             }

addNode :: Ord index
        => Node index value 
        -> Hypergraph index value 
        -> Hypergraph index value
addNode node graph =
  graph { nodes = Map.insert node.index node.value graph }

addEdge :: Ord index
        => Edge index
        -> Hypergraph index value
        -> Hypergraph index value
addEdge edge graph =
  graph { edges = Map.insert edge.indices
addToHypergraph :: Ord index
                => Node index node
                -> Edge index
                -> Hypergraph index node
                -> Hypergraph index node
addToHypergraph node edge graph =
  graph { nodes = Map.insert index node graph.nodes
        , edges = Map.insert index adj graph.edges
        }

-- graphFromList :: Ord index => [index] -> [node] -> Graph
