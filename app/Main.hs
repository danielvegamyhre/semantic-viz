import System.IO
import System.Process
import Data.Maybe
import Data.Map (Map)
import Data.List.Unique
import qualified Data.Map as M


-- Recursive(-ish) function for building the adjacency list. Uses number of leading spaces of each line of
-- output from Wordnet to determine node placement in graph.
-- NOTE: I say recursive-ish because the helper functions it calls all call this function again after
--       completing their respective operations (i.e. tail recursion)
buildAdjacencyList :: [(String, Int)] -> [String] -> Int -> Map String [String] -> Map String [String]
buildAdjacencyList [] _ _ hashmap = hashmap
buildAdjacencyList ((word, spaces) : tail_pairs) parents currNumSpaces hashmap
    | spaces > currNumSpaces = addNeighborNewParent tail_pairs parents currNumSpaces word spaces hashmap
    | spaces < currNumSpaces = addNeighborOldParent tail_pairs parents currNumSpaces word spaces hashmap
    | otherwise              = addNeighborSameParent tail_pairs parents currNumSpaces word spaces hashmap

-- Add neighbor to current node without updating current parent node pointer
addNeighborSameParent :: [(String, Int)] -> [String] -> Int -> String -> Int -> Map String [String] -> Map String [String]
addNeighborSameParent pairs parents currNumSpaces word spaces hashmap = do
    let (_, oldParents) = pop parents -- pop last item on parent hiearchy (if spaces didn't change, it has no children)
        currParent = head oldParents

        -- add parent -> node relationship
        vals = M.lookup currParent hashmap
        newVals = uniq (word: (fromMaybe [] vals))
        updatedHashmap = M.insert currParent newVals hashmap

        -- add node -> parent relationship
        vals2 = M.lookup word updatedHashmap
        newVals2 = uniq (currParent: (fromMaybe [] vals2))
        updatedHashmap2 = M.insert word newVals2 updatedHashmap

        -- add word to parents in case next line has more spaces and it becomes a parent
        (__, newParents) = push word oldParents
    buildAdjacencyList pairs newParents currNumSpaces updatedHashmap2


-- Update current node to new word and add neighbor
addNeighborNewParent :: [(String, Int)] -> [String] -> Int -> String -> Int -> Map String [String] -> Map String [String]
addNeighborNewParent pairs parents currNumSpaces word spaces hashmap = do
    let currParent = head parents
        (_, newParents) = push word parents
        currNumSpaces = spaces

        -- add node -> parent relationship
        vals = M.lookup word hashmap
        newVals = uniq (currParent: (fromMaybe [] vals))
        updatedHashmap = M.insert word newVals hashmap

        -- add parent -> node relationship
        vals2 = M.lookup currParent updatedHashmap
        newVals2 = uniq (word: (fromMaybe [] vals2))
        updatedHashmap2 = M.insert currParent newVals2 updatedHashmap
    buildAdjacencyList pairs newParents currNumSpaces updatedHashmap2


-- Update current node to old parent and add neighbor
addNeighborOldParent :: [(String, Int)] -> [String] -> Int -> String -> Int -> Map String [String] -> Map String [String]
addNeighborOldParent pairs parents currNumSpaces word spaces hashmap = do
    let (_, newParents) = pop parents -- pop last parent from stack and use new top element as parent
        currParent = head newParents
        currNumSpaces = spaces

        -- add parent -> node relationship
        vals = M.lookup currParent hashmap
        newVals = uniq (word: (fromMaybe [] vals))
        updatedHashmap = M.insert currParent newVals hashmap

        -- add node -> parent relationship
        vals2 = M.lookup word hashmap
        newVals2 = uniq (currParent: (fromMaybe [] vals2))
        updatedHashmap2 = M.insert word newVals2 updatedHashmap
    buildAdjacencyList pairs newParents currNumSpaces updatedHashmap2


-- Counts spaces
isSpace s = s == ' '
countNumSpaces str = length (filter isSpace str)


-- Splits a line on the '=' character. Used by the getPairs function.
splitOnEqualSign :: [Char] -> [String]
splitOnEqualSign "" = [""]
splitOnEqualSign ('=':cs) = "" : splitOnEqualSign cs
splitOnEqualSign (c:cs) = (c:cellCompletion) : otherCells
 where cellCompletion : otherCells = splitOnEqualSign cs

-- Splits a line on the ',' character. Used by the getPairs function.
splitOnCommas :: [Char] -> [String]
splitOnCommas "" = [""]
splitOnCommas (',':cs) = "" : splitOnCommas cs
splitOnCommas (c:cs) = (c:cellCompletion) : otherCells
 where cellCompletion : otherCells = splitOnCommas cs


-- Gets lines from input file containing wordnet output, and removes headers from top
getLines :: String -> IO [String]
getLines fileName = do
    let inputLines = drop 4 . lines <$> readFile fileName
    outLines <- inputLines
    return outLines


-- Inputs lines of Wordnet output and returns pairs of words + number of leading spaces for that input line
getPairs :: [String] -> [[(String, Int)]]
getPairs inputLines = do
    let splitLines = map splitOnEqualSign inputLines  -- split input lines on '=' character
        spaces = map head splitLines -- get leading spaces on each line
        numSpaces = map length spaces -- measure number of leading spaces on each line
        wordStrings = map removeCommas (map getFirstWord (map words inputLines)) -- map removeCommas (map getFirstWord (map words inputLines))-- get words on each line
        pairs = zip wordStrings numSpaces
    return pairs


-- Get first tuple from list of tuples
getFirstTuple :: [(String,Int)] -> (String,Int)
getFirstTuple (tuple: tuples) = tuple


-- Get first item from list of lists
getFirstList :: [[(String,Int)]] -> [(String,Int)]
getFirstList (list: lists) = list


-- Get second element of each list if length is > 1, else return first element
getFirstWord [] = []
getFirstWord (x:y:xs) = do
    if (null y) then x
    else y


-- Check if item already exists in list
isDuplicate _ [] = False
isDuplicate x (y : ys) = if x == y then True else isDuplicate x ys


-- Remove commas from string
removeCommas xs = [ x | x <- xs, not (x `elem` ",") ]


-- Push element to stack
push :: a -> [a] -> ((),[a])  -- return a tuple containing a 'nothing' and a new stack
push elem stack = ((), (:) elem stack)


-- Pop element from stack
pop :: [a] -> (a, [a])  -- return a tuple containing the popped element and the new stack
pop [] = error "Can't pop from an empty stack!"
pop ((:) x stack) = (x, stack)


-- Use matplotlib to visualize graph
visualize filename word1 word2 = do
    let cmd = "python3"
        args = ["app/visualize.py", filename, word1, word2]
        input = ""
    (rc, out, err) <- readProcessWithExitCode cmd args input
    putStrLn "Visualizing semantic graph with Matplotlib..."


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
    let cmd = "app/wc-bash.sh"
        args = [category]
        input = ""
    (rc, out, err) <- readProcessWithExitCode cmd args input


    let inputLines = getLines "app/wn_output.txt"
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


    writeFile "app/adjacency_list.txt" adjacencyListString

    -- display undirected graph and visualize shortest distance between input words
    visualize "app/adjacency_list.txt" word1 word2
