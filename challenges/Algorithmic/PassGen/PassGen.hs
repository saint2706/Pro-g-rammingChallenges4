{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : PassGen
-- Description : Cryptographically secure password generator CLI implemented in Haskell.
--
-- The goal of this executable is to mirror the ergonomics of the Python
-- implementation living in the same folder (``passgen.py``) while showcasing
-- how to wire the workflow in a strongly typed language.  The generator relies
-- on the ``crypto-random`` package for entropy so the produced passwords are
-- suitable for security-sensitive contexts.  It supports the same high-level
-- feature set: configurable character pools, ambiguity filtering, batch
-- generation, entropy reporting, JSON output, and an analytics summary that can
-- feed the visualiser utility.

module Main (main) where

import Control.Monad (foldM, when)
import Crypto.Random (DRG, getSystemDRG, randomBytesGenerate)
import Data.Aeson (Value, object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.List (foldl')
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Set as Set
import qualified Data.Text as T
import System.Console.GetOpt
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Text.Printf (printf)

lettersCharset :: String
lettersCharset = ['a' .. 'z'] ++ ['A' .. 'Z']

digitsCharset :: String
digitsCharset = ['0' .. '9']

symbolsCharset :: String
symbolsCharset = "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"

fullCharset :: String
fullCharset = lettersCharset ++ digitsCharset ++ symbolsCharset

ambiguousCharacters :: Set.Set Char
ambiguousCharacters =
  Set.fromList "O0l1I|`'\"{}[]()/\\"

data Flag
  = FlagLength String
  | FlagCount String
  | FlagLetters
  | FlagDigits
  | FlagSymbols
  | FlagNoAmbiguous
  | FlagJson
  | FlagSummary
  | FlagBatches String
  | FlagMinCategories String
  deriving (Eq, Show)

data RawOptions = RawOptions
  { rawLength :: Maybe Int
  , rawCount :: Maybe Int
  , rawLetters :: Maybe Bool
  , rawDigits :: Maybe Bool
  , rawSymbols :: Maybe Bool
  , rawExcludeAmbiguous :: Bool
  , rawJson :: Bool
  , rawSummary :: Bool
  , rawBatches :: Maybe Int
  , rawMinCategories :: Maybe Int
  }

defaultRaw :: RawOptions
defaultRaw =
  RawOptions
    { rawLength = Nothing
    , rawCount = Nothing
    , rawLetters = Nothing
    , rawDigits = Nothing
    , rawSymbols = Nothing
    , rawExcludeAmbiguous = False
    , rawJson = False
    , rawSummary = False
    , rawBatches = Nothing
    , rawMinCategories = Nothing
    }

data Spec = Spec
  { specLength :: Int
  , specCount :: Int
  , specLetters :: Bool
  , specDigits :: Bool
  , specSymbols :: Bool
  , specExcludeAmbiguous :: Bool
  , specJson :: Bool
  , specSummary :: Bool
  , specBatches :: Int
  , specMinCategories :: Int
  }
  deriving (Show)

data Pools = Pools
  { poolAll :: String
  , poolLetters :: String
  , poolDigits :: String
  , poolSymbols :: String
  }

data Summary = Summary
  { summarySpec :: Spec
  , summaryBatches :: Int
  , summaryTotalPasswords :: Int
  , summaryTotalCharacters :: Int
  , summaryPoolSize :: Int
  , summaryEntropy :: Double
  , summaryBaselineEntropy :: Double
  , summaryFrequencies :: [(Char, Int)]
  , summaryProbabilities :: [(Char, Double)]
  , summaryCategoryFrequencies :: (Int, Int, Int)
  }

options :: [OptDescr Flag]
options =
  [ Option ['l'] ["length"] (ReqArg FlagLength "INT") "Password length (minimum 8)"
  , Option ['n'] ["count"] (ReqArg FlagCount "INT") "Number of passwords to generate"
  , Option [] ["letters"] (NoArg FlagLetters) "Include ASCII letters"
  , Option [] ["digits"] (NoArg FlagDigits) "Include digits"
  , Option [] ["symbols"] (NoArg FlagSymbols) "Include punctuation symbols"
  , Option [] ["no-ambiguous"] (NoArg FlagNoAmbiguous) "Exclude ambiguous characters"
  , Option [] ["json"] (NoArg FlagJson) "Emit JSON instead of human-readable text"
  , Option [] ["summary"] (NoArg FlagSummary) "Generate analytics summary data"
  , Option [] ["batches"] (ReqArg FlagBatches "INT") "Number of batches when collecting summary data"
  , Option [] ["min-categories"] (ReqArg FlagMinCategories "INT") "Minimum distinct character categories"
  ]

usage :: String
usage = usageInfo "Usage: runghc PassGen.hs [OPTIONS]" options

main :: IO ()
main = do
  argv <- getArgs
  case parseArgs argv of
    Left err -> do
      hPutStrLn stderr err
      hPutStrLn stderr usage
      exitFailure
    Right spec -> do
      case buildPools spec of
        Left err -> do
          hPutStrLn stderr err
          exitFailure
        Right pools ->
          case validateSpec spec pools of
            Left err -> do
              hPutStrLn stderr err
              exitFailure
            Right () ->
              if specSummary spec
                then runSummaryMode spec pools
                else runGenerationMode spec pools

parseArgs :: [String] -> Either String Spec
parseArgs args = do
  let (flags, rest, errs) = getOpt Permute options args
  if not (null errs)
    then Left (concat errs)
    else
      if not (null rest)
        then Left "Unexpected positional arguments provided"
        else do
          raw <- foldM applyFlag defaultRaw flags
          finalize raw

applyFlag :: RawOptions -> Flag -> Either String RawOptions
applyFlag acc flag =
  case flag of
    FlagLength txt -> setIntField txt acc rawLength (\val opts -> opts {rawLength = Just val})
    FlagCount txt -> setIntField txt acc rawCount (\val opts -> opts {rawCount = Just val})
    FlagLetters -> pure acc {rawLetters = Just True}
    FlagDigits -> pure acc {rawDigits = Just True}
    FlagSymbols -> pure acc {rawSymbols = Just True}
    FlagNoAmbiguous -> pure acc {rawExcludeAmbiguous = True}
    FlagJson -> pure acc {rawJson = True}
    FlagSummary -> pure acc {rawSummary = True}
    FlagBatches txt -> setIntField txt acc rawBatches (\val opts -> opts {rawBatches = Just val})
    FlagMinCategories txt -> setIntField txt acc rawMinCategories (\val opts -> opts {rawMinCategories = Just val})

setIntField :: String -> RawOptions -> (RawOptions -> Maybe Int) -> (Int -> RawOptions -> RawOptions) -> Either String RawOptions
setIntField rawTxt opts _ setter =
  case reads rawTxt of
    [(val, "")] -> pure (setter val opts)
    _ -> Left ("Unable to parse integer from: " ++ rawTxt)

finalize :: RawOptions -> Either String Spec
finalize raw = do
  let lenVal = fromMaybe 16 (rawLength raw)
      countVal = fromMaybe 1 (rawCount raw)
      batchesVal = fromMaybe 1 (rawBatches raw)
      minCats = fromMaybe 2 (rawMinCategories raw)
      anyCategoryFlag = isJust (rawLetters raw) || isJust (rawDigits raw) || isJust (rawSymbols raw)
      lettersVal = fromMaybe (if anyCategoryFlag then False else True) (rawLetters raw)
      digitsVal = fromMaybe (if anyCategoryFlag then False else True) (rawDigits raw)
      symbolsVal = fromMaybe False (rawSymbols raw)
  pure
    Spec
      { specLength = lenVal
      , specCount = countVal
      , specLetters = lettersVal
      , specDigits = digitsVal
      , specSymbols = symbolsVal
      , specExcludeAmbiguous = rawExcludeAmbiguous raw
      , specJson = rawJson raw
      , specSummary = rawSummary raw
      , specBatches = batchesVal
      , specMinCategories = minCats
      }

validateSpec :: Spec -> Pools -> Either String ()
validateSpec spec pools = do
  when (specLength spec < 8) $ Left "Password length must be at least 8"
  when (specCount spec <= 0) $ Left "Count must be >= 1"
  when (specBatches spec <= 0) $ Left "Number of batches must be >= 1"
  when (specMinCategories spec <= 0) $ Left "Minimum categories must be >= 1"
  let selectedCategories = length (filter id [specLetters spec, specDigits spec, specSymbols spec])
  when (selectedCategories == 0) $ Left "At least one character category must be enabled"
  when (selectedCategories < specMinCategories spec) $ Left "Selected categories do not meet the minimum diversity requirement"
  when (specLength spec < selectedCategories) $ Left "Password length too small for required categories"
  when (null (poolAll pools)) $ Left "Character pool is empty"
  when (specLetters spec && null (poolLetters pools)) $ Left "Letter pool is empty after filtering"
  when (specDigits spec && null (poolDigits pools)) $ Left "Digit pool is empty after filtering"
  when (specSymbols spec && null (poolSymbols pools)) $ Left "Symbol pool is empty after filtering"
  pure ()

buildPools :: Spec -> Either String Pools
buildPools spec = do
  let basePool = concat $ filter (not . null)
        [ if specLetters spec then lettersCharset else ""
        , if specDigits spec then digitsCharset else ""
        , if specSymbols spec then symbolsCharset else ""
        ]
      filteredPool =
        if specExcludeAmbiguous spec
          then filter (`Set.notMember` ambiguousCharacters) basePool
          else basePool
      lettersPool = filter (`elem` filteredPool) lettersCharset
      digitsPool = filter (`elem` filteredPool) digitsCharset
      symbolsPool = filter (`elem` filteredPool) symbolsCharset
  if null filteredPool
    then Left "Character pool is empty; enable more categories or disable ambiguity filtering"
    else pure $ Pools filteredPool lettersPool digitsPool symbolsPool

runGenerationMode :: Spec -> Pools -> IO ()
runGenerationMode spec pools = do
  drg <- getSystemDRG
  let (passwords, _) = generatePasswordsWithDRG spec pools (specCount spec) drg
      entropyBits = estimateEntropy (length (poolAll pools)) (specLength spec)
  if specJson spec
    then BL8.putStrLn (encodePasswordsJson spec pools passwords entropyBits)
    else do
      putStrLn "Generated Password(s):"
      mapM_ (uncurry (printf " %2d: %s\n")) (zip [1 ..] passwords)
      printf "\nPool size: %d | Estimated entropy: %.2f bits\n" (length (poolAll pools)) entropyBits

runSummaryMode :: Spec -> Pools -> IO ()
runSummaryMode spec pools = do
  drg <- getSystemDRG
  let (frequencyMap, _) = collectFrequencies spec pools drg
      summary = buildSummary spec pools frequencyMap
  if specJson spec
    then BL8.putStrLn (Aeson.encode (summaryToJson summary))
    else printSummary summary

encodePasswordsJson :: Spec -> Pools -> [String] -> Double -> BL8.ByteString
encodePasswordsJson spec pools passwords entropyBits =
  Aeson.encode . object $
    [ "passwords" .= passwords
    , "length" .= specLength spec
    , "count" .= specCount spec
    , "letters" .= specLetters spec
    , "digits" .= specDigits spec
    , "symbols" .= specSymbols spec
    , "excluded_ambiguous" .= specExcludeAmbiguous spec
    , "pool_size" .= length (poolAll pools)
    , "estimated_entropy_bits" .= roundTo 2 entropyBits
    ]

collectFrequencies :: DRG g => Spec -> Pools -> g -> (Map.Map Char Int, g)
collectFrequencies spec pools drg =
  go (specBatches spec) drg Map.empty
  where
    go 0 g acc = (acc, g)
    go n g acc =
      let (batch, g1) = generatePasswordsWithDRG spec pools (specCount spec) g
          acc' = foldl' updateFreq acc batch
       in go (n - 1) g1 acc'
    updateFreq acc password = foldl' (\m c -> Map.insertWith (+) c 1 m) acc password

buildSummary :: Spec -> Pools -> Map.Map Char Int -> Summary
buildSummary spec pools freqMap =
  let ordered = Map.toAscList freqMap
      totalChars = sum (map snd ordered)
      poolSize = length (poolAll pools)
      entropyBits = estimateEntropy poolSize (specLength spec)
      baselineBits = estimateEntropy (Set.size (Set.fromList fullCharset)) (specLength spec)
      probabilities =
        if totalChars == 0
          then []
          else map (\(ch, count) -> (ch, roundTo 6 (fromIntegral count / fromIntegral totalChars))) ordered
      categoryCounts = foldl' tally (0, 0, 0) ordered
   in Summary
        { summarySpec = spec
        , summaryBatches = specBatches spec
        , summaryTotalPasswords = specCount spec * specBatches spec
        , summaryTotalCharacters = totalChars
        , summaryPoolSize = poolSize
        , summaryEntropy = roundTo 4 entropyBits
        , summaryBaselineEntropy = roundTo 4 baselineBits
        , summaryFrequencies = ordered
        , summaryProbabilities = probabilities
        , summaryCategoryFrequencies = categoryCounts
        }
  where
    letterSet = Set.fromList lettersCharset
    digitSet = Set.fromList digitsCharset
    tally (l, d, s) (ch, count)
      | Set.member ch letterSet = (l + count, d, s)
      | Set.member ch digitSet = (l, d + count, s)
      | otherwise = (l, d, s + count)

summaryToJson :: Summary -> Value
summaryToJson summary =
  object
    [ "spec" .= specValue (summarySpec summary)
    , "batches" .= summaryBatches summary
    , "total_passwords" .= summaryTotalPasswords summary
    , "total_characters" .= summaryTotalCharacters summary
    , "pool_size" .= summaryPoolSize summary
    , "estimated_entropy_bits" .= summaryEntropy summary
    , "baseline_entropy_bits" .= summaryBaselineEntropy summary
    , "character_frequencies" .= Map.fromList [ (T.singleton ch, count) | (ch, count) <- summaryFrequencies summary ]
    , "character_probabilities" .= Map.fromList [ (T.singleton ch, prob) | (ch, prob) <- summaryProbabilities summary ]
    , "category_frequencies" .= categoryValue (summaryCategoryFrequencies summary)
    ]

specValue :: Spec -> Value
specValue spec =
  object
    [ "length" .= specLength spec
    , "count" .= specCount spec
    , "letters" .= specLetters spec
    , "digits" .= specDigits spec
    , "symbols" .= specSymbols spec
    , "exclude_ambiguous" .= specExcludeAmbiguous spec
    , "min_categories" .= specMinCategories spec
    ]

categoryValue :: (Int, Int, Int) -> Value
categoryValue (l, d, s) =
  object
    [ "letters" .= l
    , "digits" .= d
    , "symbols" .= s
    ]

printSummary :: Summary -> IO ()
printSummary summary = do
  putStrLn "Summary overview"
  printf " Spec: length=%d count=%d letters=%s digits=%s symbols=%s\n"
    (specLength spec)
    (specCount spec)
    (boolText (specLetters spec))
    (boolText (specDigits spec))
    (boolText (specSymbols spec))
  printf " Batches: %d | Total passwords: %d | Total characters: %d\n"
    (summaryBatches summary)
    (summaryTotalPasswords summary)
    (summaryTotalCharacters summary)
  printf " Pool size: %d | Estimated entropy: %.4f bits | Baseline entropy: %.4f bits\n"
    (summaryPoolSize summary)
    (summaryEntropy summary)
    (summaryBaselineEntropy summary)
  putStrLn " Character frequencies:"
  mapM_ (\(ch, count) -> putStrLn $ "  " ++ [ch] ++ ": " ++ show count) (summaryFrequencies summary)
  when (not (null (summaryProbabilities summary))) $ do
    putStrLn " Character probabilities:"
    mapM_ (\(ch, prob) -> printf "  %s: %.6f\n" [ch] prob) (summaryProbabilities summary)
  let (lettersTotal, digitsTotal, symbolsTotal) = summaryCategoryFrequencies summary
  printf " Category totals -> Letters: %d, Digits: %d, Symbols: %d\n"
    lettersTotal digitsTotal symbolsTotal
  where
    spec = summarySpec summary
    boolText True = "yes"
    boolText False = "no"

generatePasswordsWithDRG :: DRG g => Spec -> Pools -> Int -> g -> ([String], g)
generatePasswordsWithDRG spec pools countVal drg =
  go countVal drg []
  where
    go 0 g acc = (reverse acc, g)
    go n g acc =
      let (password, g1) = generateOne spec pools g
       in go (n - 1) g1 (password : acc)

generateOne :: DRG g => Spec -> Pools -> g -> (String, g)
generateOne spec pools drg =
  let requiredPools =
        filter (not . null)
          [ if specLetters spec then poolLetters pools else ""
          , if specDigits spec then poolDigits pools else ""
          , if specSymbols spec then poolSymbols pools else ""
          ]
      (requiredChars, g1) = drawRequired drg requiredPools []
      remainingCount = specLength spec - length requiredChars
      (bodyChars, g2) = drawCharacters g1 (poolAll pools) remainingCount []
      combined = requiredChars ++ bodyChars
   in shuffleWithDRG g2 combined

drawRequired :: DRG g => g -> [String] -> String -> (String, g)
drawRequired drg [] acc = (acc, drg)
drawRequired drg (pool : rest) acc =
  let (ch, g1) = drawOne drg pool
   in drawRequired g1 rest (ch : acc)

drawCharacters :: DRG g => g -> String -> Int -> String -> (String, g)
drawCharacters drg _ 0 acc = (acc, drg)
drawCharacters drg pool n acc =
  let (ch, g1) = drawOne drg pool
   in drawCharacters g1 pool (n - 1) (ch : acc)

drawOne :: DRG g => g -> String -> (Char, g)
drawOne drg pool =
  let (idx, g1) = uniformIndex drg (length pool)
   in (pool !! idx, g1)

shuffleWithDRG :: DRG g => g -> String -> (String, g)
shuffleWithDRG drg xs =
  let (shuffled, gFinal) = go drg xs []
   in (shuffled, gFinal)
  where
    go g [] acc = (acc, g)
    go g lst acc =
      let (idx, g1) = uniformIndex g (length lst)
          (selected, remainder) = removeAt idx lst
       in go g1 remainder (selected : acc)

removeAt :: Int -> [a] -> (a, [a])
removeAt idx xs =
  case splitAt idx xs of
    (before, element : after) -> (element, before ++ after)
    _ -> error "removeAt: index out of bounds"

uniformIndex :: DRG g => g -> Int -> (Int, g)
uniformIndex drg lenVal
  | lenVal <= 0 = error "uniformIndex called with non-positive bound"
  | otherwise =
      let bytesNeeded = requiredBytes lenVal
          modulus = fromIntegral lenVal
          space = 256 ^ bytesNeeded
          threshold = space - (space `mod` modulus)
       in drawIndex drg bytesNeeded modulus threshold
  where
    drawIndex g bytes modulus threshold =
      let (chunk, g1) = randomBytesGenerate bytes g
          value = bytesToInteger chunk
       in if value < threshold
            then (fromIntegral (value `mod` modulus), g1)
            else drawIndex g1 bytes modulus threshold

requiredBytes :: Int -> Int
requiredBytes n
  | n <= 1 = 1
  | otherwise = go 1 256
  where
    target = fromIntegral n
    go bytes cap
      | target <= cap = bytes
      | otherwise = go (bytes + 1) (cap * 256)

bytesToInteger :: BS.ByteString -> Integer
bytesToInteger = BS.foldl' step 0
  where
    step acc byte = acc * 256 + fromIntegral byte

estimateEntropy :: Int -> Int -> Double
estimateEntropy poolSize lenVal =
  fromIntegral lenVal * logBase 2 (fromIntegral poolSize)

roundTo :: Int -> Double -> Double
roundTo precision value =
  let factor = 10 ^^ precision
   in fromIntegral (round (value * factor)) / factor
