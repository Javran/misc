module Javran.MaxFlow.Types
  ( NetworkRep (..)
  )
where

data NetworkRep = NetworkRep
  { nrNodeCount :: Int
  , nrArcCount :: Int
  , nrSource :: Int
  , nrSink :: Int
  , nrArcs :: [((Int, Int), Int)]
  }
  deriving (Show)
