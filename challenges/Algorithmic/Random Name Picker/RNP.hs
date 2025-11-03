{-# LANGUAGE RecordWildCards #-}
-- |
-- Module      : RNP
-- Description : Random name picker executable written in Haskell.
--
-- Mirrors the behaviour of the Python implementation (rnp.py) by supporting
-- weighted draws, deterministic seeding, optional JSON output, and a simple
-- statistics block that downstream visualisers can consume.
-- The script intentionally avoids external dependencies so it can be executed
-- with "runghc" on a stock GHC installation.
module Main (main) where

import Control.Exception (IOException, try)
import Control.Monad (foldM, when)
import Data.Char (isSpace)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import System.Random (StdGen, mkStdGen, newStdGen, randomR)

-- | Runtime configuration parsed from CLI arguments.
data Config = Config
  { cfgPath :: FilePath
  , cfgCount :: Int
  , cfgWithReplacement :: Bool
  , cfgJSON :: Bool
  , cfgSeed :: Maybe Int
  , cfgStats :: Bool
  }
  deriving (Show, Eq)

defaultConfig :: Config
defaultConfig =
  Config
    { cfgPath = "names.txt"
    , cfgCount = 1
    , cfgWithReplacement = False
    , cfgJSON = False
    , cfgSeed = Nothing
    , cfgStats = False
    }

-- | Parsed input data. The weight vector is present when at least one
-- weight was found in the file. When absent, all names have an implicit
-- weight of 1.0.
data NameData = NameData
  { ndNames :: [String]
  , ndWeights :: Maybe [Double]
  }
  deriving (Show, Eq)

-- | Known default names for quick bootstrapping.
defaultNames :: [String]
defaultNames =
  [ "Alice"
  , "Bob"
  , "Charlie"
  , "David"
  , "Eve"
  , "Frank"
  , "Grace"
  , "Heidi"
  , "Ivan"
  , "Judy"
  ]

-- | Command line usage text shown for -h/--help or parsing failures.
usage :: String
usage =
  unlines
    [ "Random Name Picker (Haskell)"
    , ""
    , "Usage: runghc RNP.hs [options]"
    , ""
    , "Options:"
    , "  -c, --count N            Number of names to pick (default 1)"
    , "  -f, --file PATH          Path to names file (default names.txt)"
    , "      --with-replacement  Allow selecting the same name multiple times"
    , "      --json              Emit JSON output"
    , "      --stats             Include visualiser-ready statistics"
    , "      --seed N            Seed for deterministic runs"
    , "  -h, --help              Show this help text"
    ]

main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    Left err -> do
      hPutStrLn stderr err
      putStrLn usage
      exitFailure
    Right cfg -> runWithConfig cfg

-- | Parse CLI arguments using a hand-rolled recursive descent. The parser
-- keeps the dependency footprint minimal and mirrors the Python CLI flags.
parseArgs :: [String] -> Either String Config
parseArgs = go defaultConfig
 where
  go cfg [] = Right cfg
  go cfg (flag:rest) =
    case flag of
      "-c" -> expectInt "-c" rest $ \n -> go (cfg {cfgCount = n})
      "--count" -> expectInt "--count" rest $ \n -> go (cfg {cfgCount = n})
      "-f" -> expectValue "-f" rest $ \path -> go (cfg {cfgPath = path})
      "--file" -> expectValue "--file" rest $ \path -> go (cfg {cfgPath = path})
      "--with-replacement" -> go (cfg {cfgWithReplacement = True}) rest
      "--json" -> go (cfg {cfgJSON = True}) rest
      "--stats" -> go (cfg {cfgStats = True}) rest
      "--seed" -> expectInt "--seed" rest $ \n -> go (cfg {cfgSeed = Just n})
      "-h" -> Left usage
      "--help" -> Left usage
      _ -> Left $ "Unknown flag: " ++ flag

  expectInt name xs cont =
    case xs of
      (val:rest) ->
        case safeReadInt val of
          Nothing -> Left $ "Expected integer after " ++ name ++ ", got '" ++ val ++ "'"
          Just n -> cont n rest
      [] -> Left $ "Missing integer after " ++ name

  expectValue name xs cont =
    case xs of
      (val:rest) -> cont val rest
      [] -> Left $ "Missing argument after " ++ name

safeReadInt :: String -> Maybe Int
safeReadInt s = case reads s of
  [(n, "")] -> Just n
  _ -> Nothing

runWithConfig :: Config -> IO ()
runWithConfig cfg@Config {..} = do
  contentExists <- safeReadFile cfgPath
  (names, weights) <- case contentExists of
    Right val -> do
      parsed <- parseNames (lines val)
      let ns = ndNames parsed
      when (null ns) $ do
        hPutStrLn stderr "Name file is empty"
        exitFailure
      pure (ns, ndWeights parsed)
    Left err -> do
      hPutStrLn stderr err
      hPutStrLn stderr "Creating a default names.txt with sample entries."
      writeFile cfgPath (unlines defaultNames)
      pure (defaultNames, Nothing)

  validateConfig cfg names

  gen <- case cfgSeed of
    Just s -> pure (mkStdGen s)
    Nothing -> newStdGen

  let (picks, _) = pickNames gen names weights cfgCount cfgWithReplacement
      statsBlock = if cfgStats then Just (buildStats names weights) else Nothing
  if cfgJSON
    then putStrLn $ renderJSON cfg picks names weights statsBlock
    else do
      putStrLn $ "Picked " ++ show (length picks) ++ " name(s):"
      mapM_ (putStrLn . ("  - " ++)) picks
      maybe (pure ()) (\s -> putStrLn $ "(Deterministic seed " ++ show s ++ ")") cfgSeed
      case statsBlock of
        Nothing -> pure ()
        Just statsJSON -> do
          putStrLn ""
          putStrLn "Statistics:"
          putStrLn statsJSON

-- | Attempt to read a file, returning a Left error message when not possible.
safeReadFile :: FilePath -> IO (Either String String)
safeReadFile path = do
  result <- try (readFile path) :: IO (Either IOException String)
  pure $ case result of
    Right contents -> Right contents
    Left _ -> Left $ "Unable to read " ++ path

-- | Parse the names file, skipping invalid lines with warnings.
parseNames :: [String] -> IO NameData
parseNames = foldM step (NameData [] Nothing)
 where
  step acc rawLine =
    case trim rawLine of
      "" -> pure acc
      line ->
        case break (== ',') line of
          (name, '') -> pure $ appendName acc name 1.0 False
          (name, _:weightStr) ->
            case safeReadDouble (trim weightStr) of
              Just w | w > 0 -> pure $ appendName acc name w True
              _ -> do
                hPutStrLn stderr $ "Warning: invalid weight for '" ++ name ++ "' – skipping line"
                pure acc

  appendName (NameData ns Nothing) name w sawWeight =
    let newWeights = if sawWeight then Just (replicate (length ns) 1.0 ++ [w]) else Nothing
     in NameData (ns ++ [trim name]) newWeights
  appendName (NameData ns (Just ws)) name w _ =
    NameData (ns ++ [trim name]) (Just (ws ++ [w]))

safeReadDouble :: String -> Maybe Double
safeReadDouble s = case reads s of
  [(n, "")] -> Just n
  _ -> Nothing

trim :: String -> String
trim = f . f
 where
  f = reverse . dropWhile isSpace

-- | Validate configuration after parsing input.
validateConfig :: Config -> [String] -> IO ()
validateConfig Config {..} names
  | cfgCount <= 0 = do
      hPutStrLn stderr "Count must be positive"
      exitFailure
  | not cfgWithReplacement && cfgCount > length names = do
      hPutStrLn stderr $ "Cannot pick " ++ show cfgCount ++ " unique names from " ++ show (length names)
      exitFailure
  | otherwise = pure ()

-- | Select names according to the requested replacement mode and weights.
pickNames :: StdGen -> [String] -> Maybe [Double] -> Int -> Bool -> ([String], StdGen)
pickNames gen names weights count withReplacement
  | withReplacement = pickWithReplacement gen count dist []
  | otherwise = pickWithoutReplacement gen count (zip names weightVec) []
 where
  weightVec = fromMaybe (replicate (length names) 1.0) weights
  dist = zip names weightVec

pickWithReplacement :: StdGen -> Int -> [(String, Double)] -> [String] -> ([String], StdGen)
pickWithReplacement gen count dist acc
  | count <= 0 = (reverse acc, gen)
  | null dist = (reverse acc, gen)
  | otherwise =
      let total = sum (map snd dist)
          (r, gen') = randomR (0.0, total) gen
          name = fst (selectWeighted dist r)
       in pickWithReplacement gen' (count - 1) dist (name : acc)

pickWithoutReplacement :: StdGen -> Int -> [(String, Double)] -> [String] -> ([String], StdGen)
pickWithoutReplacement gen count dist acc
  | count <= 0 = (reverse acc, gen)
  | null dist = (reverse acc, gen)
  | otherwise =
      let total = sum (map snd dist)
          (r, gen') = randomR (0.0, total) gen
          (name, remaining) = selectAndRemove dist r
       in pickWithoutReplacement gen' (count - 1) remaining (name : acc)

-- | Select a name by cumulative weight.
selectWeighted :: [(String, Double)] -> Double -> (String, Double)
selectWeighted [] _ = error "selectWeighted called with empty distribution"
selectWeighted ((name, w) : rest) r
  | r <= w = (name, w)
  | null rest = (name, w)
  | otherwise = selectWeighted rest (r - w)

selectAndRemove :: [(String, Double)] -> Double -> (String, [(String, Double)])
selectAndRemove [] _ = error "selectAndRemove called with empty distribution"
selectAndRemove ((name, w) : rest) r
  | r <= w = (name, rest)
  | null rest = (name, rest)
  | otherwise =
      let (picked, remaining) = selectAndRemove rest (r - w)
       in (picked, (name, w) : remaining)

-- | Build a JSON formatted string, optionally embedding statistics.
renderJSON :: Config -> [String] -> [String] -> Maybe [Double] -> Maybe String -> String
renderJSON Config {..} picks names weights statsBlock =
  let baseFields =
        [ jsonField "picked" (jsonArray (map jsonString picks))
        , jsonField "count" (show cfgCount)
        , jsonField "with_replacement" (jsonBool cfgWithReplacement)
        , jsonField "seed" (maybe "null" show cfgSeed)
        , jsonField "total_names" (show (length names))
        , jsonField "weights_used" (jsonBool (maybe False (const True) weights))
        ]
      statsField = maybe [] (\s -> [jsonField "statistics" s]) statsBlock
   in "{" ++ intercalate "," (baseFields ++ statsField) ++ "}"

-- | Construct the statistics block used by the visualiser.
buildStats :: [String] -> Maybe [Double] -> String
buildStats names weights =
  let concreteWeights = fromMaybe (replicate (length names) 1.0) weights
      normalised = normalise concreteWeights
   in
      "{"
        ++ intercalate
          ","
          [ jsonField "names" (jsonArray (map jsonString names))
          , jsonField "weights" (maybe "null" (jsonArray . map jsonNumber) weights)
          , jsonField "normalized_weights" (jsonArray (map jsonNumber normalised))
          ]
        ++ "}"

normalise :: [Double] -> [Double]
normalise ws =
  let total = sum ws
   in if total <= 0 then replicate (length ws) 0 else map (/ total) ws

-- | Render a key-value JSON pair without additional whitespace.
jsonField :: String -> String -> String
jsonField key value = jsonString key ++ ":" ++ value

jsonArray :: [String] -> String
jsonArray xs = "[" ++ intercalate "," xs ++ "]"

jsonBool :: Bool -> String
jsonBool True = "true"
jsonBool False = "false"

jsonString :: String -> String
jsonString s = '"' : concatMap escapeChar s ++ ""
 where
  escapeChar '"' = "\\\""
  escapeChar '\\' = "\\\\"
  escapeChar '\n' = "\\n"
  escapeChar '\r' = "\\r"
  escapeChar '\t' = "\\t"
  escapeChar c
    | c < ' ' = "\\u" ++ hex4 (fromEnum c)
    | otherwise = [c]
  hex4 n = let hex = "0123456789abcdef" in [hex !! (n `div` 4096), hex !! ((n `div` 256) `mod` 16), hex !! ((n `div` 16) `mod` 16), hex !! (n `mod` 16)]

jsonNumber :: Double -> String
jsonNumber = trimZeroes . show
 where
  trimZeroes str =
    case break (== '.') str of
      (intPart, '.' : frac) ->
        let frac' = reverse (dropWhile (== '0') (reverse frac))
         in if null frac' then intPart else intPart ++ "." ++ frac'
      _ -> str
