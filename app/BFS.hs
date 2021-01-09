{-# LANGUAGE ScopedTypeVariables #-}
module BFS where
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import qualified Data.Map as M
import qualified Data.Set as Set
import Utils (merge)

-- Breadth-first search
breadthFirstSearch
  :: forall a. Ord a => Map a [a]  -- Adjacency list
  -> a               -- Target node
  -> Set.Set a       -- Visited nodes
  -> [a]             -- Queue of nodes to visit
  -> [a]             -- Current path
  -> Maybe [a]       -- Returns path (list of nodes) if exists
breadthFirstSearch graph target visited queue path = search visited queue path
  where
    -- search :: Set.Set a -> [a] -> [a] -> Maybe [a]
    search visited queue path = case (last queue) of
      vertex
        | vertex == target            -> Just $ vertex : path                  -- base case 1: found target
        | vertex `Set.member` visited -> search visited (init queue) path     -- base case 2: visited node
        | otherwise                   -> search visitedWithNode withNext (vertex: path)
        where
          visitedWithNode = Set.insert vertex visited
          withNext :: [a]
          withNext = merge (fromMaybe [] (M.lookup vertex graph)) (init queue)
