module Utils where
import System.Process

-- Merge 2 lists
merge :: [a] -> [a] -> [a]
merge xs     []     = xs
merge []     ys     = ys
merge (x:xs) (y:ys) = x : y : merge xs ys

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
-- pop :: [a] -> (a, [a])  -- return a tuple containing the popped element and the new stack
pop [] = ("", []) --error "Can't pop from an empty stack!"
pop ((:) x stack) = (x, stack)


-- Use matplotlib to visualize graph
visualize filename word1 word2 = do
    let cmd = "python3"
        args = ["app/visualize.py", filename, word1, word2]
        input = ""
    (rc, out, err) <- readProcessWithExitCode cmd args input
    putStrLn "Visualizing semantic graph with Matplotlib..."
