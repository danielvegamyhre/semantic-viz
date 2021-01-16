{-# LANGUAGE ScopedTypeVariables #-}
module BFS where
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import qualified Data.Map as M
import qualified Data.Set as Set
import Debug.Trace

import Utils (merge)

-- Breadth-first search
breadthFirstSearch
  :: forall a. Ord a => Map a [a]  -- Adjacency list
  -> a               -- Target node
  -> Set.Set a       -- Visited nodes
  -> [(a, [a])]      -- Queue of nodes to visit [(node, [path])]
  -> Maybe [a]       -- Returns path (list of nodes) if exists
breadthFirstSearch graph target visited queue = search visited queue
  where
    -- search :: Set.Set a -> [a] -> [a] -> Maybe [a]
    search visited queue = case (last queue) of
      (vertex, path)
        | vertex == target            -> Just $ vertex : path            -- base case 1: found target
        | vertex `Set.member` visited -> search visited (init queue)     -- base case 2: visited node
        | otherwise                   -> search visitedWithNode withNext
        where
          visitedWithNode = Set.insert vertex visited
          nextQueue :: [(a, [a])]
          nextQueue = zip (fromMaybe [] (M.lookup vertex graph)) $ repeat (vertex: path)
          withNext :: [(a, [a])]
          withNext = merge nextQueue (init queue)
