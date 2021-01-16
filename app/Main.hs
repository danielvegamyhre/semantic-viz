-- Haskell libs
import System.IO
import System.Process
import Data.Maybe (fromMaybe, fromJust)
import qualified Data.Map as M
import qualified Data.Set as Set
import Data.Graph
import Data.GraphViz
import Data.GraphViz.Printing
import Data.Graph.Inductive.Graph
import Data.Text.Lazy (unpack)

-- Project modules
import BFS (breadthFirstSearch)
import Graph (buildAdjacencyList, makeUnlabelledGraph, makeLabelledGraph)
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

        -- perform BFS to find shortest path between source and target
        visited = Set.empty
        queue = [(word1, [])]
        path = breadthFirstSearch adjacencyList word2 visited queue
        pathString = show (fromMaybe [] path)

        -- convert to inductive graph
        edgeList = map (\(k,ks) -> (k,k,ks)) $ M.toList adjacencyList
        (graph, nodeFromVertex, vertexFromKey) = graphFromEdges edgeList

        -- get nodes (String, String, [String]) from vertices (Int)
        nodeList = map nodeFromVertex (vertices graph)

        -- get the vertex (Int) mapped to each node
        vertexList = map vertexFromKey (map (\(k,ks) -> k) $ M.toList adjacencyList)

        -- tuple vertex with node to create LNodes
        lnodeList = zip (map (\v -> (fromMaybe 0 v)) vertexList) nodeList
        ledgeList = map (\(j,k,ks) -> ((fromMaybe 0 (vertexFromKey j)), (fromMaybe 0 (vertexFromKey k)), ks)) $ edgeList

        -- make labelled and unlabelled graphs
        unlabelledGraph = makeUnlabelledGraph (Data.Graph.vertices graph) (Data.Graph.edges graph)
        labelledGraph = makeLabelledGraph lnodeList ledgeList

        -- convert to dot format
        unlabelledGraphInDotFormat = graphToDot nonClusteredParams unlabelledGraph
        labelledgraphInDotFormat = graphToDot nonClusteredParams labelledGraph

        unlabelledDotData = unpack (renderDot $ toDot unlabelledGraphInDotFormat)
        labelledDotData = unpack (renderDot $ toDot labelledgraphInDotFormat)

    -- save unlabelled graph
    let dot_cmd = "dot"
        dot_args = ["-Tsvg","-oUnlabelledSemanticGraph.svg"]
    (rc, out, err) <- readProcessWithExitCode dot_cmd dot_args unlabelledDotData

    -- save labelled graph
    let dot_cmd = "dot"
        dot_args = ["-Tsvg","-oLabelledSemanticGraph.svg"]
    (rc, out, err) <- readProcessWithExitCode dot_cmd dot_args labelledDotData

--    print lnodeList
--    print edgeList
--    print ledgeList
    putStrLn $ "Saved semantic graph to UnlabelledSemanticGraph.svg"
    putStrLn $ "Saved semantic graph to LabelledSemanticGraph.svg"

--    writeFile "app/adjacency_list.txt" adjacencyListString

    -- display undirected graph and visualize shortest distance between input words
--    visualize "app/adjacency_list.txt" word1 word2
