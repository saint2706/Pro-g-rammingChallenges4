{-# LANGUAGE RecordWildCards #-}

module Caesar
  ( Alphabet(..)
  , CrackResult(..)
  , encrypt
  , decrypt
  , crack
  , main
  ) where

import Data.Char (chr, isAlpha, isAsciiLower, isAsciiUpper, isDigit, ord, toUpper)
import Data.List (elemIndex, foldl', sortBy)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)
import Text.Printf (printf)

-- | Supported alphabets for the Caesar cipher.
data Alphabet = Letters | Alphanumeric | Printable
  deriving (Eq, Show, Read)

-- | Result produced by 'crack' containing the attempted shift, plaintext, and score.
data CrackResult = CrackResult
  { resultShift :: !Int
  , resultPlaintext :: !String
  , resultScore :: !Double
  }
  deriving (Eq, Show)

-- | Encrypt text with the supplied shift and alphabet.
encrypt :: Alphabet -> Int -> String -> String
encrypt Letters rawShift = map (shiftLetter normalized)
  where
    normalized = normalizeShift 26 rawShift
    shiftLetter s c
      | isAsciiUpper c = chr $ baseUpper + ((ord c - baseUpper + s) `mod` 26)
      | isAsciiLower c = chr $ baseLower + ((ord c - baseLower + s) `mod` 26)
      | otherwise = c
    baseUpper = ord 'A'
    baseLower = ord 'a'

encrypt Alphanumeric rawShift = map (shiftChar letterShift digitShift)
  where
    letterShift = normalizeShift 26 rawShift
    digitShift = normalizeShift 10 rawShift
    shiftChar ls ds c
      | isAsciiUpper c = chr $ baseUpper + ((ord c - baseUpper + ls) `mod` 26)
      | isAsciiLower c = chr $ baseLower + ((ord c - baseLower + ls) `mod` 26)
      | isDigit c = chr $ baseDigit + ((ord c - baseDigit + ds) `mod` 10)
      | otherwise = c
    baseUpper = ord 'A'
    baseLower = ord 'a'
    baseDigit = ord '0'

encrypt Printable rawShift = map shiftPrintable
  where
    printableChars = [' ' .. '~']
    alphabetLength = length printableChars
    normalized = normalizeShift alphabetLength rawShift
    shiftPrintable c =
      case elemIndex c printableChars of
        Just idx -> printableChars !! ((idx + normalized) `mod` alphabetLength)
        Nothing -> c

-- | Decrypt text with the supplied shift and alphabet.
decrypt :: Alphabet -> Int -> String -> String
decrypt alphabet shiftValue = encrypt alphabet (negate shiftValue)

-- | Attempt to crack a Caesar cipher via brute force.
crack :: Alphabet -> String -> [CrackResult]
crack alphabet ciphertext =
  let maxShift = case alphabet of
        Letters -> 26
        Alphanumeric -> 26
        Printable -> 95
      candidates =
        [ let plaintext = decrypt alphabet shift ciphertext
              score = frequencyScore plaintext
           in CrackResult shift plaintext score
        | shift <- [0 .. maxShift - 1]
        ]
   in sortBy (comparing resultScore) candidates

-- | Normalise a shift value within the given alphabet size.
normalizeShift :: Int -> Int -> Int
normalizeShift base n = ((n `mod` base) + base) `mod` base

-- | Frequency table for English letters (percentage).
englishFrequency :: Map.Map Char Double
englishFrequency = Map.fromList
  [ ('E', 12.7), ('T', 9.1), ('A', 8.2), ('O', 7.5), ('I', 7.0)
  , ('N', 6.7), ('S', 6.3), ('H', 6.1), ('R', 6.0), ('D', 4.3)
  , ('L', 4.0), ('C', 2.8), ('U', 2.8), ('M', 2.4), ('W', 2.4)
  , ('F', 2.2), ('G', 2.0), ('Y', 2.0), ('P', 1.9), ('B', 1.3)
  , ('V', 1.0), ('K', 0.8), ('J', 0.15), ('X', 0.15), ('Q', 0.10)
  , ('Z', 0.07)
  ]

-- | Compute a chi-squared frequency score similar to the Python reference implementation.
frequencyScore :: String -> Double
frequencyScore text
  | totalLetters == 0 = 1 / 0 -- Infinity: no letters to analyse
  | otherwise = total / 100.0
  where
    letters = [toUpper c | c <- text, isAlpha c]
    counts = foldl' (\acc ch -> Map.insertWith (+) ch (1 :: Int) acc) Map.empty letters
    totalLetters = sum (Map.elems counts)
    total = Map.foldlWithKey' accumulate 0 englishFrequency
    accumulate acc letter expected =
      let observed = (fromIntegral (Map.findWithDefault 0 letter counts) / totalLettersD) * 100.0
          delta = observed - expected
       in if expected > 0
            then acc + (delta * delta) / expected
            else acc
    totalLettersD = fromIntegral totalLetters

-- | Command-line interface --------------------------------------------------

data Mode = ModeEncrypt | ModeDecrypt | ModeCrack
  deriving (Eq, Show)

data Options = Options
  { optMode :: Mode
  , optShift :: Maybe Int
  , optAlphabet :: Alphabet
  , optTop :: Int
  , optVerbose :: Bool
  , optText :: Maybe String
  }
  deriving (Eq, Show)

defaultOptions :: Options
defaultOptions = Options
  { optMode = ModeEncrypt
  , optShift = Nothing
  , optAlphabet = Letters
  , optTop = 5
  , optVerbose = False
  , optText = Nothing
  }

data ParseError = ParseError
  { errorMessage :: String
  , isHelp :: Bool
  }

usage :: String
usage = unlines
  [ "Usage: caesar-hs [--mode encrypt|decrypt|crack] [--shift N] [--alphabet letters|alphanumeric|printable]"
  , "                  [--top K] [--verbose] [--brute-force] --text TEXT"
  , ""
  , "Examples:"
  , "  caesar-hs --mode encrypt --shift 3 --text 'Hello World'"
  , "  caesar-hs --mode decrypt --shift 3 --text 'Khoor Zruog'"
  , "  caesar-hs --brute-force --text 'Wklv lv d vhfuhw' --top 10"
  ]

parseArgs :: [String] -> Either ParseError Options
parseArgs = go defaultOptions
  where
    go opts [] =
      case optText opts of
        Nothing -> Left $ ParseError "Missing --text argument" False
        Just _ -> Right opts
    go opts (arg : rest)
      | arg == "--help" || arg == "-h" = Left $ ParseError usage True
      | arg == "--mode" || arg == "-m" =
          case rest of
            modeValue : xs ->
              case parseMode modeValue of
                Just mode' -> go (opts {optMode = mode'}) xs
                Nothing -> Left $ ParseError ("Unknown mode: " ++ modeValue) False
            [] -> Left $ ParseError "--mode requires an argument" False
      | arg == "--shift" || arg == "-s" =
          case rest of
            value : xs ->
              case parseInt value of
                Just n -> go (opts {optShift = Just n}) xs
                Nothing -> Left $ ParseError ("Invalid shift value: " ++ value) False
            [] -> Left $ ParseError "--shift requires an argument" False
      | arg == "--alphabet" || arg == "-a" =
          case rest of
            value : xs ->
              case parseAlphabet value of
                Just alphabet -> go (opts {optAlphabet = alphabet}) xs
                Nothing -> Left $ ParseError ("Unknown alphabet: " ++ value) False
            [] -> Left $ ParseError "--alphabet requires an argument" False
      | arg == "--top" =
          case rest of
            value : xs ->
              case parseInt value of
                Just n | n > 0 -> go (opts {optTop = n}) xs
                _ -> Left $ ParseError ("Invalid top value: " ++ value) False
            [] -> Left $ ParseError "--top requires an argument" False
      | arg == "--verbose" || arg == "-v" = go (opts {optVerbose = True}) rest
      | arg == "--brute-force" = go (opts {optMode = ModeCrack}) rest
      | arg == "--text" || arg == "-t" =
          case rest of
            value : xs -> go (opts {optText = Just value}) xs
            [] -> Left $ ParseError "--text requires an argument" False
      | "-" `elem` [take 1 arg] = Left $ ParseError ("Unknown option: " ++ arg) False
      | otherwise =
          case optText opts of
            Nothing -> go (opts {optText = Just arg}) rest
            Just existing -> go (opts {optText = Just (existing ++ " " ++ arg)}) rest

    parseMode "encrypt" = Just ModeEncrypt
    parseMode "decrypt" = Just ModeDecrypt
    parseMode "crack" = Just ModeCrack
    parseMode _ = Nothing

    parseAlphabet value =
      case fmap toUpper value of
        "LETTERS" -> Just Letters
        "ALPHANUMERIC" -> Just Alphanumeric
        "PRINTABLE" -> Just Printable
        _ -> Nothing

    parseInt value = case reads value of
      [(n, "")] -> Just n
      _ -> Nothing

run :: Options -> IO ()
run Options {..} = do
  let alphabet = optAlphabet
      textValue = fromMaybe "" optText
  case optMode of
    ModeEncrypt ->
      case optShift of
        Nothing -> die "Shift value is required for encryption"
        Just shiftValue -> do
          let result = encrypt alphabet shiftValue textValue
          putStrLn $ "Encrypted text: " ++ result
          whenVerbose $ do
            putStrLn $ "Plaintext: " ++ textValue
            putStrLn $ "Shift: " ++ show shiftValue
    ModeDecrypt ->
      case optShift of
        Nothing -> die "Shift value is required for decryption"
        Just shiftValue -> do
          let result = decrypt alphabet shiftValue textValue
          putStrLn $ "Decrypted text: " ++ result
          whenVerbose $ do
            putStrLn $ "Ciphertext: " ++ textValue
            putStrLn $ "Shift: " ++ show shiftValue
    ModeCrack -> do
      putStrLn "Performing brute force attack..."
      let results = crack alphabet textValue
      if null results
        then putStrLn "No valid decryptions found."
        else do
          let displayCount = min optTop (length results)
              header = printf "%-5s %-6s %-10s %s" ("Rank" :: String) ("Shift" :: String) ("Fitness" :: String) ("Decrypted Text" :: String)
          putStrLn ""
          putStrLn $ "Top " ++ show displayCount ++ " most likely decryptions:"
          putStrLn header
          putStrLn (replicate 60 '-')
          mapM_ (uncurry printResult) (zip [1 .. displayCount] results)
          let CrackResult bestShift bestPlain bestScore = head results
          putStrLn ""
          putStrLn $ printf "Best guess: '%s' (shift: %d, fitness: %.3f)" bestPlain bestShift bestScore
  where
    whenVerbose action = if optVerbose then action else pure ()
    printResult rank CrackResult {..} =
      putStrLn $ printf "%-5d %-6d %-10.3f %s" (rank :: Int) resultShift resultScore resultPlaintext
    die message = do
      hPutStrLn stderr message
      exitFailure

main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    Left ParseError {..}
      | isHelp -> do
          putStrLn errorMessage
          exitSuccess
      | otherwise -> do
          hPutStrLn stderr errorMessage
          hPutStrLn stderr usage
          exitFailure
    Right opts -> run opts
