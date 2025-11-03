{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : Conway
-- Description : Headless-friendly Conway's Game of Life simulator with pattern seeding
--
-- This module provides a lightweight command line interface for evolving Game of Life
-- grids. It mirrors the Python implementation in this directory by exposing the same
-- pattern names and a headless mode for statistics focused runs.
module Main (main) where

import Control.Monad (foldM_, unless, when)
import Data.Char (toLower)
import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import System.Console.GetOpt
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)

-- | A coordinate inside the finite simulation grid.
type Coord = (Int, Int)

type Pattern = Set Coord

data Config = Config
    { cfgWidth :: !Int
    , cfgHeight :: !Int
    , cfgIterations :: !Int
    , cfgPatternName :: !(Maybe String)
    , cfgPatternFile :: !(Maybe FilePath)
    , cfgHeadless :: !Bool
    , cfgWrap :: !Bool
    }
    deriving (Show)

defaultConfig :: Config
defaultConfig = Config
    { cfgWidth = 80
    , cfgHeight = 40
    , cfgIterations = 100
    , cfgPatternName = Just "glider"
    , cfgPatternFile = Nothing
    , cfgHeadless = False
    , cfgWrap = True
    }

-- | Command line flags produced by 'getOpt'.
data Flag
    = FlagWidth String
    | FlagHeight String
    | FlagIterations String
    | FlagPattern String
    | FlagPatternFile String
    | FlagHeadless
    | FlagWrap
    | FlagNoWrap
    | FlagHelp
    deriving (Eq, Show)

options :: [OptDescr Flag]
options =
    [ Option [] ["width"] (ReqArg FlagWidth "INT") "Simulation width in cells"
    , Option [] ["height"] (ReqArg FlagHeight "INT") "Simulation height in cells"
    , Option [] ["iterations"] (ReqArg FlagIterations "INT") "Number of generations to evolve"
    , Option [] ["pattern"] (ReqArg FlagPattern "NAME") "Seed pattern name (glider, gosper_glider_gun)"
    , Option [] ["pattern-file"] (ReqArg FlagPatternFile "PATH") "Seed pattern from a text asset"
    , Option [] ["headless"] (NoArg FlagHeadless) "Suppress grid snapshots and print summary statistics"
    , Option [] ["wrap"] (NoArg FlagWrap) "Enable toroidal wrapping (default)"
    , Option [] ["no-wrap"] (NoArg FlagNoWrap) "Disable wrapping; edges are treated as always dead"
    , Option ["h"] ["help"] (NoArg FlagHelp) "Show help"
    ]

usage :: String -> String
usage prog = usageInfo header options
  where
    header = unlines
        [ "Usage: " <> prog <> " [OPTIONS]"
        , "Simulate Conway's Game of Life with optional pattern seeding."
        , "Examples:"
        , "  " <> prog <> " --width 120 --height 60 --pattern gosper_glider_gun"
        , "  " <> prog <> " --headless --iterations 500 --pattern-file assets/gosper.txt"
        ]

updateConfig :: Config -> [Flag] -> Either String Config
updateConfig = foldl step . Right
  where
    step (Left err) _ = Left err
    step (Right cfg) flag =
        case flag of
            FlagWidth raw -> parsePositive "width" raw <&> \n -> cfg {cfgWidth = n}
            FlagHeight raw -> parsePositive "height" raw <&> \n -> cfg {cfgHeight = n}
            FlagIterations raw ->
                let n = parsePositive "iterations" raw
                 in n <&> \val -> cfg {cfgIterations = val}
            FlagPattern name -> Right cfg {cfgPatternName = Just (map toLower name)}
            FlagPatternFile path -> Right cfg {cfgPatternFile = Just path}
            FlagHeadless -> Right cfg {cfgHeadless = True}
            FlagWrap -> Right cfg {cfgWrap = True}
            FlagNoWrap -> Right cfg {cfgWrap = False}
            FlagHelp -> Right cfg

    parsePositive :: String -> String -> Either String Int
    parsePositive label raw =
        case reads raw of
            [(n, "")] | n > 0 -> Right n
            _ -> Left ("Invalid " <> label <> " value: " <> raw)

-- '(<&>)' was introduced in base 4.11; re-define locally for backwards compatibility.
(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap

-- | ASCII derived pattern definitions shared with the Python implementation.
patternLibrary :: Map String Pattern
patternLibrary = Map.fromList
    [ ("glider", parsePattern
        [ ".#."
        , "..#"
        , "###"
        ])
    , ("gosper_glider_gun", parsePattern
        [ "........................#..........."
        , "......................#.#..........."
        , "............##......##............##"
        , "...........#...#....##............##"
        , "##........#.....#...##.............."
        , "##........#...#.##....#.#..........."
        , "..........#.....#.......#..........."
        , "...........#...#...................."
        , "............##......................"
        ])
    ]

parsePattern :: [String] -> Pattern
parsePattern rows = Set.fromList
    [ (x, y)
    | (y, row) <- zip [0 ..] rows
    , (x, c) <- zip [0 ..] row
    , c `elem` aliveChars
    ]
  where
    aliveChars = "#OoX"

-- | Load a custom pattern from disk using the same ASCII format as the Python tool.
loadPatternFile :: FilePath -> IO Pattern
loadPatternFile path = do
    contents <- fmap stripCR . lines <$> readFile path
    let trimmed = dropWhileEnd nullLine $ dropWhile nullLine contents
    pure (parsePattern trimmed)
  where
    stripCR line = case reverse line of
        '\r' : rest -> reverse rest
        _ -> line
    nullLine = all (`elem` [' ', '\t'])

    dropWhileEnd p = reverse . dropWhile p . reverse

patternDimensions :: Pattern -> (Int, Int)
patternDimensions pattern
    | Set.null pattern = (0, 0)
    | otherwise =
        let xs = Set.map fst pattern
            ys = Set.map snd pattern
         in (Set.findMax xs - Set.findMin xs + 1, Set.findMax ys - Set.findMin ys + 1)

centrePattern :: Config -> Pattern -> Either String Pattern
centrePattern Config{..} pattern
    | pw > cfgWidth || ph > cfgHeight =
        Left "Pattern does not fit inside the chosen grid dimensions"
    | otherwise = Right (Set.map shift pattern)
  where
    (pw, ph) = patternDimensions pattern
    offsetX = (cfgWidth - pw) `div` 2
    offsetY = (cfgHeight - ph) `div` 2
    shift (x, y) = (x + offsetX, y + offsetY)

-- | Step the grid forward by one generation using Conway's rules.
stepGrid :: Config -> Set Coord -> Set Coord
stepGrid cfg@Config{..} alive = Set.fromList
    [ cell
    | (cell, count) <- Map.toList neighbourCounts
    , survives cell count
    ]
  where
    neighbourCounts :: Map Coord Int
    neighbourCounts = Map.fromListWith (+)
        [ (neigh, 1)
        | cell <- Set.toList alive
        , neigh <- neighbours cfg cell
        ]

    survives cell count
        | cell `Set.member` alive = count == 2 || count == 3
        | otherwise = count == 3

neighbours :: Config -> Coord -> [Coord]
neighbours Config{..} (x, y) =
    [ wrap (x + dx, y + dy)
    | dx <- [-1, 0, 1]
    , dy <- [-1, 0, 1]
    , (dx, dy) /= (0, 0)
    , within (x + dx, y + dy)
    ]
  where
    within (nx, ny)
        | cfgWrap = True
        | otherwise = nx >= 0 && ny >= 0 && nx < cfgWidth && ny < cfgHeight

    wrap (nx, ny)
        | cfgWrap =
            ( (nx `mod` cfgWidth + cfgWidth) `mod` cfgWidth
            , (ny `mod` cfgHeight + cfgHeight) `mod` cfgHeight
            )
        | otherwise = (nx, ny)

renderGrid :: Config -> Set Coord -> String
renderGrid Config{..} alive = unlines
    [ [ if (x, y) `Set.member` alive then '█' else ' '
      | x <- [0 .. cfgWidth - 1]
      ]
    | y <- [0 .. cfgHeight - 1]
    ]

data SimulationStats = SimulationStats
    { statGenerations :: !Int
    , statFinalAlive :: !Int
    , statPeakAlive :: !Int
    , statLoopDetected :: !(Maybe Int)
    }
    deriving (Show)

simulate :: Config -> Set Coord -> SimulationStats
simulate cfg@Config{..} seed = go 0 seed Set.empty (Set.size seed) Nothing
  where
    go gen current seen peak loopInfo
        | gen >= cfgIterations =
            SimulationStats cfgIterations (Set.size current) peak loopInfo
        | otherwise =
            let next = stepGrid cfg current
                nextSeen = Set.insert current seen
                newPeak = max peak (Set.size next)
                loopDetected = case loopInfo of
                    Just _ -> loopInfo
                    Nothing -> if next `Set.member` nextSeen then Just (gen + 1) else Nothing
             in go (gen + 1) next nextSeen newPeak loopDetected

simulateWithSnapshots :: Config -> Set Coord -> IO ()
simulateWithSnapshots cfg@Config{..} seed =
    foldM_ step seed [0 .. cfgIterations]
  where
    step current generation = do
        putStrLn ("Generation " <> show generation)
        putStrLn (renderGrid cfg current)
        when (generation < cfgIterations) $ putStrLn ""
        pure (stepGrid cfg current)

main :: IO ()
main = do
    args <- getArgs
    prog <- getProgName
    let (flags, _, errors) = getOpt Permute options args

    when (FlagHelp `elem` flags) $ do
        putStrLn (usage prog)
        exitSuccess

    unless (null errors) $ do
        mapM_ (hPutStrLn stderr) errors
        hPutStrLn stderr (usage prog)
        exitFailure

    case updateConfig defaultConfig flags of
        Left err -> do
            hPutStrLn stderr err
            hPutStrLn stderr (usage prog)
            exitFailure
        Right cfg -> runProgram cfg

runProgram :: Config -> IO ()
runProgram cfg@Config{..} = do
    pattern <- resolvePattern cfg
    case centrePattern cfg pattern of
        Left err -> do
            hPutStrLn stderr err
            exitFailure
        Right centred ->
            if cfgHeadless
                then printStats (simulate cfg centred)
                else simulateWithSnapshots cfg centred

resolvePattern :: Config -> IO Pattern
resolvePattern Config{..} =
    case cfgPatternFile of
        Just path -> loadPatternFile path
        Nothing ->
            case cfgPatternName of
                Nothing -> pure Set.empty
                Just name ->
                    case Map.lookup name patternLibrary of
                        Nothing -> do
                            hPutStrLn stderr $ "Unknown pattern '" <> name <> "'. Available: " <> intercalate ", " (Map.keys patternLibrary)
                            exitFailure
                        Just pattern -> pure pattern

printStats :: SimulationStats -> IO ()
printStats SimulationStats{..} = do
    putStrLn "Simulation complete"
    putStrLn $ "Generations simulated: " <> show statGenerations
    putStrLn $ "Final live cells:     " <> show statFinalAlive
    putStrLn $ "Peak live cells:      " <> show statPeakAlive
    case statLoopDetected of
        Nothing -> pure ()
        Just gen -> putStrLn $ "Loop detected at generation " <> show gen
