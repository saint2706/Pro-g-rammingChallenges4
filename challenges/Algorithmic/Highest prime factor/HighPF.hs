{-# LANGUAGE RecordWildCards #-}

{-%
Highest Prime Factor (Haskell)
==============================

Command line utility mirroring the Python implementation in the same directory.
Computes the largest prime factor for one or more integers, supporting positional
arguments, batch file inputs, stdin streaming, JSON output, and optional timing.
-}

module Main (main, highestPrimeFactor) where

import Control.Exception (IOException, try)
import Data.List (intercalate)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitWith)
import System.IO (hPutStrLn, stderr)
import Text.Printf (printf)
import Text.Read (readMaybe)

-- | Compute the largest prime factor of @n@ using a 6k ± 1 stride trial division.
highestPrimeFactor :: Integer -> Integer
highestPrimeFactor n
  | n < 2 = error "Input must be an integer greater than 1."
  | otherwise = loop after3 best3 5
  where
    (after2, best2) = stripPrime n 2 1
    (after3, best3) = stripPrime after2 3 best2

    loop :: Integer -> Integer -> Integer -> Integer
    loop value best f
      | value == 1 = best
      | f * f > value = max value best
      | otherwise =
          let (value1, best1) = stripPrime value f best
              (value2, best2) = stripPrime value1 (f + 2) best1
           in loop value2 best2 (f + 6)

    stripPrime :: Integer -> Integer -> Integer -> (Integer, Integer)
    stripPrime value factor best =
      let (reduced, found) = stripFactor value factor
          newBest = if found then max best factor else best
       in (reduced, newBest)

    stripFactor :: Integer -> Integer -> (Integer, Bool)
    stripFactor value factor = go value False
      where
        go current seen
          | current `mod` factor == 0 = go (current `div` factor) True
          | otherwise = (current, seen)

-- | Result for one integer, optionally carrying elapsed seconds.
data FactorResult = FactorResult
  { frValue :: Integer
  , frHighest :: Integer
  , frElapsed :: Maybe Double
  }

-- | Options parsed from the CLI.
data Options = Options
  { optNumbers :: [Integer]
  , optUseStdin :: Bool
  , optBatchFiles :: [FilePath]
  , optJSON :: Bool
  , optTiming :: Bool
  }
  deriving (Show)

usageMessage :: String
usageMessage = unlines
  [ "Usage: HighPF [--stdin] [--batch FILE ...] [--json] [--timing] [NUMBERS ...]"
  , "Compute highest prime factors for integers >= 2." ]

emptyOptions :: Options
emptyOptions = Options [] False [] False False

parseArgs :: [String] -> Either String Options
parseArgs = go emptyOptions
  where
    go opts [] = Right opts
    go opts ("--stdin" : rest) = go opts {optUseStdin = True} rest
    go opts ("--json" : rest) = go opts {optJSON = True} rest
    go opts ("--timing" : rest) = go opts {optTiming = True} rest
    go opts ("--batch" : path : rest) = go opts {optBatchFiles = path : optBatchFiles opts} rest
    go _ ("--batch" : []) = Left "--batch expects a file path argument."
    go opts (flag@('-' : '-') : _) = Left $ "Unrecognised option: " ++ flag
    go opts (value : rest) =
      case readMaybe value of
        Just n -> go opts {optNumbers = optNumbers opts ++ [n]} rest
        Nothing -> Left $ "Invalid integer argument: " ++ value

loadBatchFile :: FilePath -> IO (Either String [Integer])
loadBatchFile path = do
  result <- try (readFile path) :: IO (Either IOException String)
  case result of
    Left err -> pure . Left $ "Unable to read batch file " ++ path ++ ": " ++ show err
    Right contents -> pure $ parseNumberStream ("file: " ++ path) contents

parseNumberStream :: String -> String -> Either String [Integer]
parseNumberStream label raw = traverse parseToken tokens
  where
    normalised = map replaceComma raw
    tokens = words normalised
    parseToken token =
      case readMaybe token of
        Just n -> Right n
        Nothing -> Left $ "Invalid integer in " ++ label ++ ": " ++ token
    replaceComma ',' = ' '
    replaceComma c = c

processNumbers :: Bool -> [Integer] -> IO [FactorResult]
processNumbers timing numbers = traverse compute numbers
  where
    compute value = do
      start <- if timing then Just <$> getCurrentTime else pure Nothing
      let highest = highestPrimeFactor value
      end <- if timing then Just <$> getCurrentTime else pure Nothing
      let elapsed = case (start, end) of
            (Just s, Just e) -> Just (realToFrac (diffUTCTime e s))
            _ -> Nothing
      pure FactorResult {frValue = value, frHighest = highest, frElapsed = elapsed}

renderResults :: Bool -> [FactorResult] -> String
renderResults True [] = "[]"
renderResults True results =
  let renderItem FactorResult {..} =
        "  {" ++ intercalate ", " (baseFields ++ timingField) ++ " }"
        where
          baseFields =
            [ "\"value\": " ++ show frValue
            , "\"highest_prime_factor\": " ++ show frHighest
            ]
          timingField =
            case frElapsed of
              Just t -> ["\"elapsed_seconds\": " ++ showFFloat t]
              Nothing -> []
      body = intercalate ",\n" (map renderItem results)
   in "[\n" ++ body ++ "\n]"
renderResults False results = unlines (map renderLine results)
  where
    renderLine FactorResult {..} =
      case frElapsed of
        Just t -> printf "%d: %d (elapsed %.3f ms)" frValue frHighest (t * 1000)
        Nothing -> printf "%d: %d" frValue frHighest

showFFloat :: Double -> String
showFFloat = printf "%.9f"

main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    Left err -> do
      hPutStrLn stderr usageMessage
      hPutStrLn stderr $ "[error] " ++ err
      exitWith (ExitFailure 2)
    Right opts -> runWith opts

runWith :: Options -> IO ()
runWith opts@Options {..} = do
  stdinNumbers <-
    if optUseStdin
      then do
        contents <- getContents
        case parseNumberStream "stdin" contents of
          Left err -> quit 2 err
          Right nums -> pure nums
      else pure []
  batchNumbers <- traverse loadBatchFile (reverse optBatchFiles)
  case sequence batchNumbers of
    Left err -> quit 2 err
    Right batches -> do
      let numbers = optNumbers ++ stdinNumbers ++ concat batches
      if null numbers
        then do
          hPutStrLn stderr usageMessage
          hPutStrLn stderr "No numbers provided (use positional args, --batch, or --stdin)."
          exitWith (ExitFailure 1)
        else
          case filter (< 2) numbers of
            (bad : _) -> quit 3 $ "All numbers must be >= 2 (got " ++ show bad ++ ")."
            [] -> do
              results <- processNumbers optTiming numbers
              putStrLn $ renderResults optJSON results
              exitWith ExitSuccess
  where
    quit code message = do
      hPutStrLn stderr $ "[error] " ++ message
      exitWith (ExitFailure code)
