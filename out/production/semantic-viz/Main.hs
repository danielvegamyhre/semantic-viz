--import Lib
import System.IO
import System.Process
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as M

-- Returns value of the current parent
getCurrParent :: [String] -> Maybe String
getCurrParent [] = Nothing
getCurrParent (x:xs) = Just x

-- add neighbor to current node without updating current parent node pointer
addNeighborSameParent :: [(String, Int)] -> [String] -> Int -> String -> Int -> Map String [String] -> Map String [String]
addNeighborSameParent pairs parents currNumSpaces word spaces hashmap = do
    let currParent = getCurrParent parents
        vals = M.lookup Just -> currParent hashmap   -- Checks if value is in hashmap returns 0 "Hi" for testing purposes
        newVals = Just vals

        newParents = (currParent: parents)
    M.update Just(currParent) newVals hashmap
    buildAdjacencyList pairs newParents currNumSpaces hashmap


-- update current node to new word and add neighbor
addNeighborNewParent :: [(String, Int)] -> [String] -> Int -> String -> Int -> Map String Int -> Map String Int
addNeighborNewParent pairs parents currNumSpaces word spaces hashmap = do
    let newParents = (word:parents)
        currParent = word
        currNumSpaces = spaces
        prevParent = currParent
    M.insert word [] hashmap
    buildAdjacencyList pairs newParents currNumSpaces hashmap


-- update current node to old parent and add neighbor
addNeighborOldParent :: [(String, Int)] -> [String] -> Int -> String -> Int -> Map String Int -> Map String Int
addNeighborOldParent pairs parents currNumSpaces word spaces hashmap = do
    let currNumSpaces = spaces
        newParents = init parents
        currParent = last newParents
        vals = M.lookup currParent hashmap
        newVals = (word:vals)
    M.update currParent newVals hashmap
    buildAdjacencyList pairs newParents currNumSpaces hashmap


-- main function for building the adjacency list
buildAdjacencyList :: [(String, Int)] -> [String] -> Int -> Map String Int -> Map String Int
buildAdjacencyList [] parents currNumSpaces hashmap = hashmap
buildAdjacencyList pairs parents currNumSpaces hashmap = do
    let next = head pairs
        word = head next
        spaces = last next
        currParent = head parents
    if spaces > currNumSpaces then addNeighborNewParent (init pairs) currParent currNumSpaces word spaces hashmap
    else if spaces < currNumSpaces then addNeighborOldParent (init pairs) currParent currNumSpaces word spaces hashmap
    else addNeighborSameParent (init pairs) currParent currNumSpaces word spaces hashmap


-- Counts spaces
isSpace s = s == ' '
countNumSpaces str = length (filter isSpace str)


-- Splits a line on the '=' character. Used by the getPairs function.
splitOnEqualSign :: [Char] -> [String]
splitOnEqualSign "" = [""]
splitOnEqualSign ('=':cs) = "" : splitOnEqualSign cs
splitOnEqualSign (c:cs) = (c:cellCompletion) : otherCells
 where cellCompletion : otherCells = splitOnEqualSign cs


-- Gets lines from input file containing wordnet output, and removes headers from top
getLines :: String -> IO [String]
getLines fileName = do
    let inputLines = drop 7 . lines <$> readFile fileName
    outLines <- inputLines
    return outLines


-- Inputs lines of Wordnet output and returns pairs of words + number of leading spaces for that input line
getPairs :: [String] -> [(String, Int)]
getPairs inputLines = do
    let splitLines = map splitOnEqualSign inputLines  -- split input lines on '=' character
        spaces = map head splitLines -- get leading spaces on each line
        numSpaces = map length spaces -- measure number of leading spaces on each line
        wordStrings = map last (map words inputLines) -- get words on each line
        pairs = zip wordStrings numSpaces
    return pairs


main = do
    category <- getLine
    let cmd = "app/wc-bash.sh"
        args = [category]
        input = ""
    (rc, out, err) <- readProcessWithExitCode cmd args input

    let inputLines = getLines "app/wn_output.txt"
    nonIOLines <- inputLines
    let pairs = getPairs nonIOLines
        pairsWithRoot = ((category, 0): pairs)
        emptyMap = M.empty
        hashmapWithRoot = M.insert category 0 emptyMap
--        hashmapFromPairs = M.fromList (head pairs) -- construct hashmap of (word, num_spaces) pairs
--        hashmapFromPairs =  M.insert n 0 hashmapFromPairs -- insert root node
        adjacencyList = buildAdjacencyList pairsWithRoot [category] 0 hashmapWithRoot
--    print $ M.toList hashmap
    print pairs