-- Haskell libs
import System.IO
import System.Process
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set

-- Project modules
import BFS (breadthFirstSearch)
import AdjacencyList (buildAdjacencyList)
import Utils (getLines, getPairs, getFirstList, visualize)

-- Main function with the following steps
-- 1. Runs Wordnet with user specified input word
-- 2. Parses Wordnet output into an adjacency list representing an undirected graph
--    of semantic relationships between the related words
-- 3. Display the undirected graph and visualize the shortest distance between the 2 input words
main = do
    -- get user input
    putStrLn "Enter a word/category to visualize the semantic relationships with related words:"
    category <- getLine
    putStrLn "Enter word 1 (source) for calculating semantic distance:"
    word1 <- getLine
    putStrLn "Enter word 1 (target) for calculating semantic distance:"
    word2 <- getLine

    -- query wordnet for hyponym of given word
--    let cmd = "app/wc-bash.sh"
--        args = [category]
--        input = ""
--    (rc, out, err) <- readProcessWithExitCode cmd args input


    let inputLines = getLines "app/wn_output2.txt"
    nonIOLines <- inputLines

    -- parse wordnet output into list of tuples [(word, numberOfLeadingSpaces)..]
    let zippedPairs = getPairs nonIOLines
        pairs = getFirstList zippedPairs
        pairsWithRoot = ((category, 0): pairs)

         -- use stack to track parent hierarchy
        parents = [category]

        -- use hashmap for adjacency list
        emptyMap = M.empty
        hashmapWithRoot = M.insert category [] emptyMap

        -- populate adjacency list recursively
        adjacencyList = buildAdjacencyList pairs parents 0 hashmapWithRoot
        adjacencyListString = show adjacencyList

        visited = Set.empty
        queue = [word1]
        path = breadthFirstSearch adjacencyList word2 visited queue []
        pathString = show (fromMaybe [] path)
    print path

--    writeFile "app/adjacency_list.txt" adjacencyListString
--
--    -- display undirected graph and visualize shortest distance between input words
--    visualize "app/adjacency_list.txt" word1 word2
