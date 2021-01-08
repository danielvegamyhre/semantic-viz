-- https://en.wikipedia.org/wiki/Shortest_Path_Faster_Algorithm
module Path where
    import qualified Data.Set as Set

    findShortestPath
        :: (Ord cost , Ord node)
        => ((cost , node) -> [(cost , node)]) -- Function defining where we can go from a node and the cost of that
        -> node                               -- Where we want to get to
        -> (cost , node)                      -- The start position
        -> Maybe (cost , node)                -- Maybe the answer. Maybe it doesn't exist
    findShortestPath next target start = search mempty (Set.singleton start)
        where
            search visited toBeVisited = case Set.minView toBeVisited of -- get next nearest node
                Nothing -> Nothing
                Just ((cost , vertex) , withoutVertex)
                    | vertex == target            -> Just (cost , vertex)               -- base case 1: found target
                    | vertex `Set.member` visited -> search visited withoutVertex       -- base case 2: visited node
                    | otherwise                   -> search visitedWithNode withNext
                    where
                        visitedWithNode = Set.insert vertex visited
                        withNext = foldr Set.insert withoutVertex $ next (cost , vertex)
