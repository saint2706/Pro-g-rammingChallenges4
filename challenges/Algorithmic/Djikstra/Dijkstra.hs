{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Control.Monad (when)
import Data.Char (isDigit, isSpace)
import Data.Foldable (foldl')
import Data.Function (on)
import Data.List (intercalate, minimumBy, sortOn)
import qualified Data.Map.Strict as M
import Data.Maybe (listToMaybe)
import qualified Data.Set as S
import GHC.Generics (Generic)
import Numeric (showFFloat)
import System.Console.GetOpt
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Text.ParserCombinators.ReadP
import Text.Read (readMaybe)

--------------------------------------------------------------------------------
-- Data structures
--------------------------------------------------------------------------------

type Node = String
type Weight = Double
type Graph = M.Map Node (M.Map Node Weight)

data StepState
  = Initializing
  | Processing
  | Updating
  | Completed
  deriving (Eq, Show, Generic)

data Step = Step
  { stepNumber :: !Int
  , stepState :: !StepState
  , stepCurrent :: !(Maybe Node)
  , stepDistances :: !(M.Map Node Weight)
  , stepPrevious :: !(M.Map Node (Maybe Node))
  , stepFrontier :: ![(Node, Weight)]
  , stepVisited :: !(S.Set Node)
  , stepMessage :: !String
  }
  deriving (Eq, Show, Generic)

--------------------------------------------------------------------------------
-- Command line options
--------------------------------------------------------------------------------

data Options = Options
  { optGraphFiles :: ![FilePath]
  , optSources :: ![Node]
  , optVerbose :: !Bool
  , optTrace :: !Bool
  , optJson :: !(Maybe FilePath)
  }
  deriving (Show)

defaultOptions :: Options
defaultOptions = Options
  { optGraphFiles = []
  , optSources = []
  , optVerbose = False
  , optTrace = False
  , optJson = Nothing
  }

data Flag
  = FlagGraph FilePath
  | FlagSource Node
  | FlagVerbose
  | FlagTrace
  | FlagJson FilePath
  | FlagHelp
  deriving (Eq)

optionsSpec :: [OptDescr Flag]
optionsSpec =
  [ Option ['g'] ["graph"] (ReqArg FlagGraph "FILE") "Graph description file (JSON or adjacency list)"
  , Option ['s'] ["source"] (ReqArg FlagSource "NODE") "Source node to start from (can be repeated)"
  , Option ['v'] ["verbose"] (NoArg FlagVerbose) "Enable verbose logging"
  , Option ['t'] ["trace"] (NoArg FlagTrace) "Print generator-style algorithm trace"
  , Option ['j'] ["json"] (ReqArg FlagJson "FILE") "Write JSON summary to FILE"
  , Option ['h'] ["help"] (NoArg FlagHelp) "Show this help message"
  ]

usageHeader :: String -> String
usageHeader prog =
  "Usage: " ++ prog ++ " [OPTIONS]\n\n" ++
  "Run Dijkstra's algorithm on example graphs or custom inputs."

--------------------------------------------------------------------------------
-- Graph parsing utilities
--------------------------------------------------------------------------------

parseGraphFile :: Options -> FilePath -> IO Graph
parseGraphFile opts path = do
  logVerbose opts $ "Reading graph file: " ++ path
  content <- readFile path
  case parseGraph content of
    Left err -> do
      hPutStrLn stderr $ "Failed to parse graph (" ++ path ++ "): " ++ err
      exitFailure
    Right g -> do
      logVerbose opts $
        "Loaded graph with " ++ show (M.size g) ++ " nodes from " ++ path
      pure g

parseGraph :: String -> Either String Graph
parseGraph txt =
  case parseJsonGraph txt of
    Just g
      | M.null g -> Left "Parsed graph is empty"
      | otherwise -> Right g
    Nothing ->
      case parseAdjacencyGraph txt of
        Right g
          | M.null g -> Left "Parsed graph is empty"
          | otherwise -> Right g
        err -> err

-- Minimal JSON parser for the expected graph structure
parseJsonGraph :: String -> Maybe Graph
parseJsonGraph txt =
  case readP_to_S (skipSpaces *> jsonGraphParser <* skipSpaces <* eof) txt of
    [(result, _)] -> Just result
    _ -> Nothing

jsonGraphParser :: ReadP Graph
jsonGraphParser = do
  _ <- char '{'
  skipSpaces
  entries <- sepBy jsonNodeEntry (skipSpaces *> char ',' *> skipSpaces)
  skipSpaces
  _ <- char '}'
  pure $ M.fromList entries

jsonNodeEntry :: ReadP (Node, M.Map Node Weight)
jsonNodeEntry = do
  key <- jsonString
  skipSpaces
  _ <- char ':'
  skipSpaces
  neighbors <- jsonNeighborMap
  pure (key, neighbors)

jsonNeighborMap :: ReadP (M.Map Node Weight)
jsonNeighborMap = do
  _ <- char '{'
  skipSpaces
  entries <- sepBy jsonNeighborEntry (skipSpaces *> char ',' *> skipSpaces)
  skipSpaces
  _ <- char '}'
  pure $ M.fromList entries

jsonNeighborEntry :: ReadP (Node, Weight)
jsonNeighborEntry = do
  key <- jsonString
  skipSpaces
  _ <- char ':'
  skipSpaces
  value <- jsonNumber
  pure (key, value)

jsonString :: ReadP String
jsonString = do
  _ <- char '"'
  str <- many $ satisfy (/= '"')
  _ <- char '"'
  pure str

jsonNumber :: ReadP Weight
jsonNumber = do
  num <- munch1 (\c -> c == '-' || c == '.' || isDigit c)
  case readMaybe num of
    Nothing -> pfail
    Just x -> pure x

-- Simple adjacency list parser
type LineNo = Int

parseAdjacencyGraph :: String -> Either String Graph
parseAdjacencyGraph txt = foldl' step (Right M.empty) (zip [1 ..] (lines txt))
  where
    step acc (ln, rawLine) =
      acc >>= \graph ->
        let line = takeWhile (/= '#') rawLine
            trimmed = trim line
         in if null trimmed
              then Right graph
              else case break (== ':') trimmed of
                (lhs, ':' : rhs) -> do
                  let node = trim lhs
                  if null node
                    then Left $ "Line " ++ show ln ++ ": missing node label"
                    else do
                      let neighborChunks = filter (not . null) (map trim (splitComma rhs))
                      neighborPairs <- traverse parseNeighbor neighborChunks
                      let updatedGraph = M.insert node (M.fromList neighborPairs) graph
                          withDangling = foldl' ensureNode updatedGraph neighborPairs
                      pure withDangling
                _ -> Left $ "Line " ++ show ln ++ ": expected 'node: neighbor(weight)' syntax"

    ensureNode g (neighbor, _) =
      if M.member neighbor g
        then g
        else M.insert neighbor M.empty g

    parseNeighbor entry =
      let (namePart, rest) = span (\c -> c /= '(' && not (isSpace c) && c /= '=') entry
          nodeName = trim namePart
          cleanedRest = dropWhile isSpace rest
       in if null nodeName
            then Left $ "Invalid neighbor entry: '" ++ entry ++ "'"
            else finalize nodeName cleanedRest

    finalize nodeName ('(' : xs) =
      case break (== ')') xs of
        (numText, ')' : remainder) ->
          if not (null (trim remainder))
            then Left $ "Unexpected trailing characters after weight for '" ++ nodeName ++ "'"
            else parseWeight nodeName (trim numText)
        _ -> Left $ "Missing closing ')' for neighbor '" ++ nodeName ++ "'"
    finalize nodeName rest =
      let stripped = dropWhile (\c -> isSpace c || c == '=') rest
          (numText, remainder) = span (\c -> isDigit c || c == '.' || c == '-') stripped
          leftovers = trim remainder
       in if null numText
            then Left $ "Missing weight for neighbor '" ++ nodeName ++ "'"
            else
              if null leftovers
                then parseWeight nodeName numText
                else Left $ "Unexpected trailing characters after weight for '" ++ nodeName ++ "'"

    parseWeight nodeName numText =
      case readMaybe numText of
        Nothing -> Left $ "Could not parse weight for '" ++ nodeName ++ "'"
        Just w ->
          if w < 0
            then Left $ "Negative weights are not supported for edge to '" ++ nodeName ++ "'"
            else Right (nodeName, w)

    trim = f . f
      where
        f = reverse . dropWhile isSpace

    splitComma = splitOn ','

splitOn :: Char -> String -> [String]
splitOn _ [] = [[]]
splitOn delim str = go str
  where
    go [] = [[]]
    go (c : cs)
      | c == delim = [] : go cs
      | otherwise =
          let (current : rest) = go cs
           in (c : current) : rest

--------------------------------------------------------------------------------
-- Dijkstra's algorithm with tracing
--------------------------------------------------------------------------------

infinity :: Double
infinity = 1 / 0

dijkstraWithTrace :: Graph -> Node -> Either String ([Step], M.Map Node Weight, M.Map Node (Maybe Node), [Node])
dijkstraWithTrace graph start
  | M.null graph = Left "Graph is empty"
  | otherwise = case M.lookup start graph of
      Nothing -> Left $ "Source node '" ++ start ++ "' not present in graph"
      Just _ -> Right (steps, finalDistances, finalPrevious, visitOrder)
  where
    allNodes = M.keysSet graph
    baseDistances = M.fromSet (const infinity) allNodes
    distances0 = M.insert start 0 baseDistances
    basePrevious = M.fromSet (const Nothing) allNodes
    initialStep = Step
      { stepNumber = 0
      , stepState = Initializing
      , stepCurrent = Nothing
      , stepDistances = distances0
      , stepPrevious = basePrevious
      , stepFrontier = frontierList distances0 S.empty
      , stepVisited = S.empty
      , stepMessage = "Initialized distances and predecessor maps"
      }

    (steps, finalDistances, finalPrevious, visitOrder) =
      go 1 [initialStep] distances0 basePrevious S.empty []

    go idx acc dist prev visited order =
      case nextCandidate dist visited of
        Nothing ->
          let completion = Step
                { stepNumber = idx
                , stepState = Completed
                , stepCurrent = Nothing
                , stepDistances = dist
                , stepPrevious = prev
                , stepFrontier = []
                , stepVisited = visited
                , stepMessage =
                    "Completed after visiting " ++ show (S.size visited) ++
                    " nodes"
                }
           in (acc ++ [completion], dist, prev, order)
        Just (current, currentDistance) ->
          let visited' = S.insert current visited
              order' = order ++ [current]
              processingStep = Step
                { stepNumber = idx
                , stepState = Processing
                , stepCurrent = Just current
                , stepDistances = dist
                , stepPrevious = prev
                , stepFrontier = frontierList dist visited'
                , stepVisited = visited'
                , stepMessage =
                    "Processing node '" ++ current ++ "' with tentative distance " ++
                    formatDistance currentDistance
                }
              (idx', acc', dist', prev') =
                processNeighbors (idx + 1) (acc ++ [processingStep]) current currentDistance visited' dist prev (neighborsOf current)
           in go idx' acc' dist' prev' visited' order'

    neighborsOf node =
      maybe [] M.toList (M.lookup node graph)

    processNeighbors idx acc current currentDistance visited dist prev [] =
      (idx, acc, dist, prev)
    processNeighbors idx acc current currentDistance visited dist prev ((neighbor, weight) : rest) =
      let alt = currentDistance + weight
          old = M.findWithDefault infinity neighbor dist
          better = alt < old
          (dist', prev', msg) =
            if better
              then ( M.insert neighbor alt dist
                   , M.insert neighbor (Just current) prev
                   , "Updated distance for '" ++ neighbor ++ "' to " ++ formatDistance alt
                   )
              else (dist, prev, "No update needed for '" ++ neighbor ++ "'")
          step = Step
            { stepNumber = idx
            , stepState = Updating
            , stepCurrent = Just current
            , stepDistances = dist'
            , stepPrevious = prev'
            , stepFrontier = frontierList dist' visited
            , stepVisited = visited
            , stepMessage = msg
            }
       in processNeighbors (idx + 1) (acc ++ [step]) current currentDistance visited dist' prev' rest

    nextCandidate dist visited =
      let unvisited = [ (node, d) | (node, d) <- M.toList dist, S.notMember node visited ]
       in if null unvisited
            then Nothing
            else Just $ minimumBy (compare `on` snd) unvisited

frontierList :: M.Map Node Weight -> S.Set Node -> [(Node, Weight)]
frontierList dist visited =
  let entries = [ (node, weight)
                | (node, weight) <- M.toList dist
                , S.notMember node visited
                ]
   in sortOn snd entries

formatDistance :: Double -> String
formatDistance x
  | isInfinite x = "∞"
  | isNaN x = "NaN"
  | otherwise =
      let raw = showFFloat (Just 6) x ""
       in stripZeros raw
  where
    stripZeros s =
      let (whole, fracWithDot) = span (/= '.') s
       in case fracWithDot of
            [] -> s
            (_ : frac) ->
              let trimmed = reverse (dropWhile (== '0') (reverse frac))
               in if null trimmed
                    then whole
                    else whole ++ "." ++ trimmed

--------------------------------------------------------------------------------
-- Reporting utilities
--------------------------------------------------------------------------------

printRunSummary :: Options -> Maybe FilePath -> Node -> [Step] -> M.Map Node Weight -> M.Map Node (Maybe Node) -> IO ()
printRunSummary opts graphSource start steps dist prev = do
  putStrLn separator
  putStrLn $ "Graph source: " ++ maybe "embedded example" id graphSource
  putStrLn $ "Start node: " ++ start
  putStrLn separator
  mapM_ (putStrLn . formatDistanceLine) (sortedNodes dist)
  putStrLn ""
  mapM_ putStrLn (renderPaths start dist prev)
  when (optTrace opts) $ do
    putStrLn ""
    putStrLn "Trace:"
    mapM_ printStep steps
  where
    separator = replicate 60 '-'
    sortedNodes = sortOn fst . M.toList
    formatDistanceLine (node, weight)
      | isInfinite weight = "  " ++ node ++ ": unreachable"
      | otherwise = "  " ++ node ++ ": " ++ formatDistance weight

printStep :: Step -> IO ()
printStep Step {..} =
  putStrLn $ "[" ++ show stepNumber ++ "] " ++ show stepState ++
    maybe "" (\n -> " @ " ++ n) stepCurrent ++ ": " ++ stepMessage

renderPaths :: Node -> M.Map Node Weight -> M.Map Node (Maybe Node) -> [String]
renderPaths start dist prev =
  [ "  Path to " ++ target ++ ": " ++ maybe "not reachable" (intercalate " -> ") (reconstruct target)
  | (target, weight) <- sortOn fst (M.toList dist)
  , target /= start
  , not (isInfinite weight)
  ]
  where
    reconstruct target = reverse <$> build target []
    build node acc
      | node == start = Just (start : acc)
      | otherwise =
          case M.lookup node prev of
            Just (Just p) -> build p (node : acc)
            _ -> Nothing

--------------------------------------------------------------------------------
-- JSON encoding
--------------------------------------------------------------------------------

data RunSummary = RunSummary
  { summaryGraphSource :: !(Maybe FilePath)
  , summarySource :: !Node
  , summaryDistances :: !(M.Map Node Weight)
  , summaryPrevious :: !(M.Map Node (Maybe Node))
  , summarySteps :: ![Step]
  , summaryVisited :: ![Node]
  }

runSummaryToJson :: [RunSummary] -> String
runSummaryToJson summaries =
  "[\n" ++ intercalate ",\n" (map ("  " ++) (map renderSummary summaries)) ++ "\n]\n"

renderSummary :: RunSummary -> String
renderSummary RunSummary {..} =
  "{\n"
    ++ maybe "" (\src -> "    \"graph\": " ++ jsonStringLiteral src ++ ",\n") summaryGraphSource
    ++ "    \"source\": " ++ jsonStringLiteral summarySource ++ ",\n"
    ++ "    \"distances\": " ++ renderMap summaryDistances ++ ",\n"
    ++ "    \"previous\": " ++ renderMaybeMap summaryPrevious ++ ",\n"
    ++ "    \"visitedOrder\": " ++ renderStringList summaryVisited ++ ",\n"
    ++ "    \"steps\": " ++ renderSteps summarySteps ++ "\n"
    ++ "  }"

renderSteps :: [Step] -> String
renderSteps steps =
  "[\n"
    ++ intercalate ",\n" (map ("      " ++) (map renderStep steps))
    ++ "\n    ]"

renderStep :: Step -> String
renderStep Step {..} =
  "{\"number\": " ++ show stepNumber
    ++ ", \"state\": " ++ jsonStringLiteral (show stepState)
    ++ ", \"current\": " ++ maybe "null" jsonStringLiteral stepCurrent
    ++ ", \"message\": " ++ jsonStringLiteral stepMessage
    ++ ", \"distances\": " ++ renderMap stepDistances
    ++ ", \"previous\": " ++ renderMaybeMap stepPrevious
    ++ ", \"frontier\": " ++ renderFrontier stepFrontier
    ++ ", \"visited\": " ++ renderStringList (S.toList stepVisited)
    ++ "}"

renderMap :: M.Map Node Weight -> String
renderMap mp =
  "{" ++ intercalate ", " (map renderPair (M.toList mp)) ++ "}"
  where
    renderPair (k, v)
      | isInfinite v = jsonStringLiteral k ++ ": \"infinity\""
      | otherwise = jsonStringLiteral k ++ ": " ++ formatDistance v

renderMaybeMap :: M.Map Node (Maybe Node) -> String
renderMaybeMap mp =
  "{" ++ intercalate ", " (map renderPair (M.toList mp)) ++ "}"
  where
    renderPair (k, mv) =
      jsonStringLiteral k ++ ": " ++ maybe "null" jsonStringLiteral mv

renderFrontier :: [(Node, Weight)] -> String
renderFrontier xs =
  "[" ++ intercalate ", " (map render xs) ++ "]"
  where
    render (n, w)
      | isInfinite w = "{\"node\": " ++ jsonStringLiteral n ++ ", \"distance\": \"infinity\"}"
      | otherwise = "{\"node\": " ++ jsonStringLiteral n ++ ", \"distance\": " ++ formatDistance w ++ "}"

renderStringList :: [String] -> String
renderStringList xs =
  "[" ++ intercalate ", " (map jsonStringLiteral xs) ++ "]"

jsonStringLiteral :: String -> String
jsonStringLiteral str = "\"" ++ concatMap escape str ++ "\""
  where
    escape '\\' = "\\\\"
    escape '"' = "\\\""
    escape '\n' = "\\n"
    escape '\t' = "\\t"
    escape c = [c]

--------------------------------------------------------------------------------
-- CLI entry point
--------------------------------------------------------------------------------

main :: IO ()
main = do
  prog <- getProgName
  args <- getArgs
  case getOpt Permute optionsSpec args of
    (flags, _, [])
      | FlagHelp `elem` flags -> putStrLn (usageInfo (usageHeader prog) optionsSpec)
      | otherwise -> runWithOptions (applyFlags flags)
    (_, _, errs) -> do
      mapM_ (hPutStrLn stderr) errs
      hPutStrLn stderr $ usageInfo (usageHeader prog) optionsSpec
      exitFailure

applyFlags :: [Flag] -> Options
applyFlags = foldl' step defaultOptions
  where
    step opts flag = case flag of
      FlagGraph fp -> opts { optGraphFiles = optGraphFiles opts ++ [fp] }
      FlagSource node -> opts { optSources = optSources opts ++ [node] }
      FlagVerbose -> opts { optVerbose = True }
      FlagTrace -> opts { optTrace = True }
      FlagJson fp -> opts { optJson = Just fp }
      FlagHelp -> opts

runWithOptions :: Options -> IO ()
runWithOptions opts = do
  graphs <-
    if null (optGraphFiles opts)
      then pure [(Nothing, exampleGraph)]
      else traverse (\fp -> (,) (Just fp) <$> parseGraphFile opts fp) (optGraphFiles opts)
  summaries <- fmap concat $ mapM (runForGraph opts) graphs
  case optJson opts of
    Nothing -> pure ()
    Just jsonFile -> do
      logVerbose opts $ "Writing JSON summary to " ++ jsonFile
      writeFile jsonFile (runSummaryToJson summaries)
      logVerbose opts "JSON summary written"

runForGraph :: Options -> (Maybe FilePath, Graph) -> IO [RunSummary]
runForGraph opts (graphSource, graph) = do
  let sources = chooseSources graph
  when (null sources) $ do
    hPutStrLn stderr "No source node specified and graph is empty"
    exitFailure
  mapM (runForSource opts graphSource graph) sources
  where
    chooseSources g =
      let requested = optSources opts
       in if null requested
            then maybe [] pure (listToMaybe (M.keys g))
            else requested

runForSource :: Options -> Maybe FilePath -> Graph -> Node -> IO RunSummary
runForSource opts graphSource graph start = do
  logVerbose opts $ "Running Dijkstra from " ++ start ++ maybe "" (" using graph " ++) graphSource
  case dijkstraWithTrace graph start of
    Left err -> do
      hPutStrLn stderr err
      exitFailure
    Right (steps, dist, prev, visitedOrder) -> do
      printRunSummary opts graphSource start steps dist prev
      pure RunSummary
        { summaryGraphSource = graphSource
        , summarySource = start
        , summaryDistances = dist
        , summaryPrevious = prev
        , summarySteps = steps
        , summaryVisited = visitedOrder
        }

--------------------------------------------------------------------------------
-- Example graph (shared with Python implementation)
--------------------------------------------------------------------------------

exampleGraph :: Graph
exampleGraph = M.fromList
  [ ("A", M.fromList [("B", 1), ("C", 4)])
  , ("B", M.fromList [("A", 1), ("C", 2), ("D", 5)])
  , ("C", M.fromList [("A", 4), ("B", 2), ("D", 1)])
  , ("D", M.fromList [("B", 5), ("C", 1), ("E", 3)])
  , ("E", M.fromList [("D", 3)])
  , ("F", M.fromList [("G", 2)])
  , ("G", M.fromList [("F", 2)])
  ]

--------------------------------------------------------------------------------
-- Helper utilities
--------------------------------------------------------------------------------

logVerbose :: Options -> String -> IO ()
logVerbose opts msg =
  when (optVerbose opts) (hPutStrLn stderr msg)

--------------------------------------------------------------------------------
-- End of file
--------------------------------------------------------------------------------
