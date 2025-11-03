{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : ToH
-- Description : Towers of Hanoi solvers (recursive and iterative) with CLI/JSON helpers.
--
-- The implementation mirrors the Python utilities in ToH.py, offering both
-- recursive and stack-based generators alongside a peg-state stream for
-- visualization integrations.  The executable exposes flags for selecting the
-- solver, limiting output, exporting JSON, and tracing intermediate peg
-- configurations.  All pure helpers remain easily reusable from GHCi or other
-- Haskell modules.
module Main
  ( Move(..)
  , PegState
  , towersOfHanoi
  , towersOfHanoiIterative
  , hanoiStateGenerator
  , CLIConfig(..)
  , SolverType(..)
  , parseArgs
  , main
  ) where

import Data.Char (isDigit)
import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

-- | Representation of a single move.
data Move = Move
  { moveDisk :: !Int
  , moveFrom :: !String
  , moveTo :: !String
  }
  deriving (Eq, Ord, Show)

type PegState = Map.Map String [Int]

-- | Render a move in the classic textual form.
renderMove :: Move -> String
renderMove Move {..} =
  "Move disk "
    <> show moveDisk
    <> " from "
    <> moveFrom
    <> " to "
    <> moveTo

-- | Recursive Towers of Hanoi solver producing a lazy move list.
towersOfHanoi :: Int -> String -> String -> String -> [Move]
towersOfHanoi disks source target auxiliary
  | disks <= 0 = []
  | otherwise =
      towersOfHanoi (disks - 1) source auxiliary target
        ++ [Move disks source target]
        ++ towersOfHanoi (disks - 1) auxiliary target source

-- | Iterative solver that emulates recursion via an explicit stack.
towersOfHanoiIterative :: Int -> String -> String -> String -> [Move]
towersOfHanoiIterative disks source target auxiliary = go [(disks, source, target, auxiliary, False)] []
  where
    go [] acc = reverse acc
    go ((n, src, tgt, aux, _stage) : rest) acc | n <= 0 = go rest acc
    go ((n, src, tgt, aux, False) : rest) acc =
      go ((n, src, tgt, aux, True) : (n - 1, src, aux, tgt, False) : rest) acc
    go ((n, src, tgt, aux, True) : rest) acc =
      go ((n - 1, aux, tgt, src, False) : rest) (Move n src tgt : acc)

-- | Produce peg states for visualization, starting with the initial layout.
hanoiStateGenerator :: Int -> String -> String -> String -> [PegState]
hanoiStateGenerator numDisks source target auxiliary =
  let initial =
        Map.fromList
          [ (source, [1 .. numDisks])
          , (auxiliary, [])
          , (target, [])
          ]
   in initial : snd (foldl applyMove (initial, [initial]) moves)
  where
    moves = towersOfHanoi numDisks source target auxiliary

    applyMove (state, acc) Move {..} =
      let disk : rest = Map.findWithDefault [] moveFrom state
          updated =
            Map.insert moveFrom rest $
              Map.insert moveTo (disk : Map.findWithDefault [] moveTo state) state
       in (updated, acc ++ [updated])

-- | Supported solver strategies.
data SolverType = Recursive | Iterative
  deriving (Eq, Show)

-- | Runtime configuration parsed from the CLI.
data CLIConfig = CLIConfig
  { cfgDisks :: !Int
  , cfgSolver :: !SolverType
  , cfgJSON :: !Bool
  , cfgTrace :: !Bool
  , cfgCountOnly :: !Bool
  , cfgMaxSteps :: !(Maybe Int)
  }
  deriving (Eq, Show)

-- | Command-line argument parsing with helpful error reporting.
parseArgs :: [String] -> Either String CLIConfig
parseArgs args = finalize =<< go args defaultState
  where
    defaultState = CLIConfigState Nothing Recursive False False False Nothing

    finalize (CLIConfigState Nothing _ _ _ _ _) = Left "Missing required --disks/-n value"
    finalize (CLIConfigState (Just n) solver json trace countOnly maxSteps)
      | n < 1 = Left "disks must be >= 1"
      | maybe False (<= 0) maxSteps = Left "--max-steps must be positive"
      | otherwise =
          Right
            CLIConfig
              { cfgDisks = n
              , cfgSolver = solver
              , cfgJSON = json
              , cfgTrace = trace
              , cfgCountOnly = countOnly
              , cfgMaxSteps = maxSteps
              }

    go [] st = Right st
    go (flag : rest) st =
      case flag of
        "--help" -> Left usage
        "-h" -> Left usage
        "--disks" -> pickNumber rest $ \n -> go (drop 1 rest) (st {disks = Just n})
        "-n" -> pickNumber rest $ \n -> go (drop 1 rest) (st {disks = Just n})
        "--iterative" -> go rest (st {solver = Iterative})
        "--recursive" -> go rest (st {solver = Recursive})
        "--json" -> go rest (st {json = True})
        "--trace" -> go rest (st {trace = True})
        "--count-only" -> go rest (st {countOnly = True})
        "--max-steps" -> pickNumber rest $ \n -> go (drop 1 rest) (st {maxSteps = Just n})
        _ -> Left $ "Unrecognized argument: " <> flag <> "\n" <> usage

    pickNumber xs k =
      case xs of
        (val : _) | all isDigit val -> k (read val)
        (val : _) -> Left $ "Expected numeric value, got " <> val
        [] -> Left "Expected value after numeric flag"

    usage = unlines
      [ "Usage: ToH [--disks N] [--iterative|--recursive] [--json] [--trace]"
      , "            [--count-only] [--max-steps N]"
      , "  --disks/-n N     Number of disks (required, >= 1)"
      , "  --iterative      Use the stack-based solver"
      , "  --recursive      Use the recursive solver (default)"
      , "  --json           Emit JSON summary instead of plain text"
      , "  --trace          Print peg states after each move"
      , "  --count-only     Only report the minimal move count"
      , "  --max-steps N    Limit the number of moves emitted"
      ]

data CLIConfigState = CLIConfigState
  { disks :: Maybe Int
  , solver :: SolverType
  , json :: Bool
  , trace :: Bool
  , countOnly :: Bool
  , maxSteps :: Maybe Int
  }
  deriving (Eq, Show)

-- | Clamp the emitted move list when --max-steps is provided.
capMoves :: Maybe Int -> [Move] -> [Move]
capMoves Nothing moves = moves
capMoves (Just limit) moves = take limit moves

-- | Render peg states in a compact textual representation.
renderState :: PegState -> String
renderState state =
  let entries =
        [ peg <> ":" <> show (reverse disks)
        | (peg, disks) <- Map.toList state
        ]
   in intercalate " " entries

-- | Manually emit JSON to avoid introducing external dependencies.
renderJSON :: CLIConfig -> Int -> [Move] -> Maybe [PegState] -> String
renderJSON CLIConfig {..} totalMoves moves mStates =
  let moveObjs =
        [ "    {\"disk\":"
            <> show moveDisk
            <> ",\"from\":\""
            <> moveFrom
            <> "\",\"to\":\""
            <> moveTo
            <> "\"}"
        | Move {..} <- moves
        ]
      moveArray =
        case moveObjs of
          [] -> "[]"
          _ -> "[\n" <> intercalate ",\n" moveObjs <> "\n  ]"
      statesArray =
        case mStates of
          Nothing -> "null"
          Just states ->
            case states of
              [] -> "[]"
              _ ->
                "[\n"
                  <> intercalate ",\n"
                    [ "    {"
                        <> intercalate ", "
                          [ "\"" <> peg <> "\": " <> show (reverse stack)
                          | (peg, stack) <- Map.toList st
                          ]
                        <> "}"
                    | st <- states
                    ]
                  <> "\n  ]"
   in "{\n"
        <> "  \"disks\": "
        <> show cfgDisks
        <> ",\n  \"solver\": \""
        <> (case cfgSolver of Recursive -> "recursive"; Iterative -> "iterative")
        <> "\",\n  \"total_moves\": "
        <> show totalMoves
        <> ",\n  \"emitted_moves\": "
        <> show (length moves)
        <> ",\n  \"moves\": "
        <> moveArray
        <> ",\n  \"states\": "
        <> statesArray
        <> "\n}"

-- | Entry point mirroring the Python utility's behaviour.
main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    Left msg -> do
      hPutStrLn stderr msg
      exitFailure
    Right cfg@CLIConfig {..} -> do
      let solverFn = case cfgSolver of
            Recursive -> towersOfHanoi
            Iterative -> towersOfHanoiIterative
          allMoves = solverFn cfgDisks "A" "C" "B"
          moves = if cfgCountOnly then [] else capMoves cfgMaxSteps allMoves
          totalMoves = (2 ^ cfgDisks) - 1
          tracedStates =
            if cfgTrace
              then Just $ capStates cfgMaxSteps $ hanoiStateGenerator cfgDisks "A" "C" "B"
              else Nothing
      mapM_ (hPutStrLn stderr . renderState) (maybe [] id tracedStates)
      if cfgJSON
        then putStrLn $ renderJSON cfg totalMoves moves tracedStates
        else if cfgCountOnly
          then putStrLn $ "Total moves: " <> show totalMoves
          else do
            mapM_ putStrLn (zipWith annotate [1 :: Int ..] moves)
            putStrLn $ "\nTotal moves required: " <> show totalMoves
  where
    annotate idx mv = show idx <> ". " <> renderMove mv

    capStates :: Maybe Int -> [PegState] -> [PegState]
    capStates Nothing states = states
    capStates (Just limit) states = take (limit + 1) states
