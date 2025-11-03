{-# LANGUAGE RecordWildCards #-}
module Main (main, affineCipher, bruteForceAffine, validAValues, modularInverse) where

import Control.Monad (when)
import Data.Char (chr, isAlpha, isLower, ord)
import Data.List (intercalate)
import Data.Maybe (isJust)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Text.Printf (printf)
import Text.Read (readMaybe)

-- | Size of the English alphabet.
alphabetSize :: Int
alphabetSize = 26

-- | All valid values for the multiplier component of the affine key.
validAValues :: [Int]
validAValues = [a | a <- [1 .. alphabetSize - 1], gcd a alphabetSize == 1]

-- | Extended Euclidean algorithm used to compute modular inverses.
extendedGcd :: Int -> Int -> (Int, Int, Int)
extendedGcd 0 b = (b, 0, 1)
extendedGcd a b =
  let (g, s, t) = extendedGcd (b `mod` a) a
   in (g, t - (b `div` a) * s, s)

-- | Compute the modular inverse of @a@ under modulus @m@, if it exists.
modularInverse :: Int -> Int -> Maybe Int
modularInverse a m
  | g /= 1 = Nothing
  | otherwise = Just ((x `mod` m + m) `mod` m)
  where
    (g, x, _) = extendedGcd (a `mod` m) m

-- | Supported modes for the CLI.
data Mode = Encrypt | Decrypt | BruteForce deriving (Eq, Show)

-- | CLI configuration after parsing arguments.
data Config = Config
  { cfgMode :: Maybe Mode
  , cfgA :: Maybe Int
  , cfgB :: Maybe Int
  , cfgText :: Maybe String
  , cfgStdin :: Bool
  , cfgJson :: Bool
  }
  deriving (Show)

defaultConfig :: Config
defaultConfig =
  Config
    { cfgMode = Nothing
    , cfgA = Nothing
    , cfgB = Nothing
    , cfgText = Nothing
    , cfgStdin = False
    , cfgJson = False
    }

-- | Attempt to parse CLI arguments into a configuration structure.
parseArgs :: [String] -> Config -> Either String Config
parseArgs [] cfg = Right cfg
parseArgs ("--mode" : m : rest) cfg =
  case m of
    "encrypt" -> parseArgs rest cfg {cfgMode = Just Encrypt}
    "decrypt" -> parseArgs rest cfg {cfgMode = Just Decrypt}
    "brute-force" -> parseArgs rest cfg {cfgMode = Just BruteForce}
    other -> Left ("Unknown mode: " ++ other)
parseArgs ("--a" : v : rest) cfg =
  case readMaybe v of
    Just n -> parseArgs rest cfg {cfgA = Just n}
    Nothing -> Left "Invalid integer for --a"
parseArgs ("--b" : v : rest) cfg =
  case readMaybe v of
    Just n -> parseArgs rest cfg {cfgB = Just n}
    Nothing -> Left "Invalid integer for --b"
parseArgs ("--text" : t : rest) cfg = parseArgs rest cfg {cfgText = Just t}
parseArgs ("--stdin" : rest) cfg = parseArgs rest cfg {cfgStdin = True}
parseArgs ("--json" : rest) cfg = parseArgs rest cfg {cfgJson = True}
parseArgs ("--help" : _) _ = Left usage
parseArgs (opt : _) _ = Left ("Unknown option: " ++ opt)

-- | Usage information shown on invalid arguments.
usage :: String
usage = intercalate "\n"
  [ "Affine cipher tool (Haskell)"
  , ""
  , "Required flags:"
  , "  --mode <encrypt|decrypt|brute-force>"
  , "  --text <value> (or use --stdin)"
  , "  --a <int> and --b <int> for encrypt/decrypt"
  , "Optional flags:"
  , "  --json        Emit JSON payload"
  , "  --stdin       Read input from STDIN"
  , "Examples:"
  , "  ./Affine --mode encrypt --text hello --a 5 --b 8"
  , "  ./Affine --mode brute-force --text ZOLSS --json"
  ]

-- | Validate the parsed configuration and return a concrete mode.
validateConfig :: Config -> Either String (Mode, Int, Int)
validateConfig Config {..} =
  case cfgMode of
    Nothing -> Left "Missing --mode"
    Just BruteForce ->
      if any isJust [cfgA, cfgB]
        then Left "Do not supply --a/--b with --mode brute-force"
        else Right (BruteForce, 0, 0)
    Just mode -> do
      a <- maybe (Left "Missing required --a value") Right cfgA
      b <- maybe (Left "Missing required --b value") Right cfgB
      if a `elem` validAValues
        then Right (mode, a, b `mod` alphabetSize)
        else Left (printf "Key a must be one of %s" (show validAValues))

-- | Ensure a text source is available and return the string to transform.
resolveInput :: Config -> IO (Either String String)
resolveInput Config {..}
  | cfgStdin && isJust cfgText = pure (Left "Use either --text or --stdin, not both")
  | cfgStdin = Right <$> getContents
  | Just txt <- cfgText = pure (Right txt)
  | otherwise = pure (Left "Missing input: provide --text or --stdin")

-- | Transform a single character according to the affine cipher.
transformChar :: Mode -> Int -> Int -> Int -> Char -> Char
transformChar mode a b invA ch
  | not (isAlpha ch) = ch
  | otherwise =
      let base = if isLower ch then ord 'a' else ord 'A'
          offset = ord ch - base
          newVal = case mode of
            Encrypt -> (a * offset + b) `mod` alphabetSize
            Decrypt -> (invA * (offset - b)) `mod` alphabetSize
            BruteForce -> offset -- not used directly
       in chr (base + newVal)

-- | Apply the affine cipher to an entire text value.
affineCipher :: Mode -> String -> Int -> Int -> Either String String
affineCipher mode text a b
  | mode == BruteForce = Left "Internal error: brute-force mode uses dedicated handler"
  | otherwise = do
      inv <- case mode of
        Decrypt -> maybe (Left err) Right (modularInverse a alphabetSize)
        Encrypt -> Right 0
        BruteForce -> Left err
      let invA = case mode of
            Encrypt -> 0
            Decrypt -> inv
            BruteForce -> inv
          apply = map (transformChar mode a b invA)
      pure (apply text)
  where
    err = printf "Invalid key 'a'=%d; must be coprime with %d" a alphabetSize

-- | Enumerate all key combinations and decrypt the ciphertext.
bruteForceAffine :: String -> [(Int, Int, String)]
bruteForceAffine ciphertext =
  [ (a, b, decryptText a b)
  | a <- validAValues
  , let Just invA = modularInverse a alphabetSize
  , b <- [0 .. alphabetSize - 1]
  , let decryptText a' b' = map (transformChar Decrypt a' b' invA) ciphertext
  ]

-- | Escape a string for safe embedding in JSON output.
jsonEscape :: String -> String
jsonEscape = concatMap escape
  where
    escape '"' = "\\\""
    escape '\\' = "\\\\"
    escape '\n' = "\\n"
    escape '\r' = "\\r"
    escape '\t' = "\\t"
    escape c
      | ord c < 0x20 = printf "\\u%04x" (ord c)
      | otherwise = [c]

-- | Emit JSON describing a single encryption/decryption run.
printJsonResult :: Mode -> Int -> Int -> String -> String -> IO ()
printJsonResult mode a b input output = do
  let invA = modularInverse a alphabetSize
      (inverseField, maybeInv) =
        case invA of
          Just inv -> (printf "\n  \"inverse_a\": %d," inv, True)
          Nothing -> ("", False)
      payload = concat
        [ "{\n"
        , printf "  \"mode\": \"%s\",\n" (modeLabel mode)
        , printf "  \"a\": %d,\n" a
        , printf "  \"b\": %d,\n" b
        , inverseField
        , printf "  \"input_length\": %d,\n" (length input)
        , printf "  \"output_length\": %d,\n" (length output)
        , printf "  \"sample_in\": \"%s\",\n" (jsonEscape (take 80 input))
        , printf "  \"sample_out\": \"%s\"\n" (jsonEscape (take 80 output))
        , "}"
        ]
  putStrLn payload
  when (mode == Decrypt && not maybeInv) $
    hPutStrLn stderr "Warning: failed to compute modular inverse for provided key"

-- | Emit JSON array for brute-force enumeration.
printJsonBruteforce :: [(Int, Int, String)] -> IO ()
printJsonBruteforce combos = do
  putStrLn "{"
  printf "  \"count\": %d,\n" (length combos)
  putStrLn "  \"results\": ["
  let render (idx, (a, b, plain)) =
        printf
          "    {\"a\": %d, \"b\": %d, \"plaintext\": \"%s\"}%s"
          a
          b
          (jsonEscape (take 120 plain))
          (if idx == length combos - 1 then "" else ",")
  mapM_ (putStrLn . render) (zip [0 ..] combos)
  putStrLn "  ]"
  putStrLn "}"

modeLabel :: Mode -> String
modeLabel Encrypt = "encrypt"
modeLabel Decrypt = "decrypt"
modeLabel BruteForce = "brute-force"

main :: IO ()
main = do
  args <- getArgs
  case parseArgs args defaultConfig of
    Left err -> do
      hPutStrLn stderr err
      when (err /= usage) $ hPutStrLn stderr usage
      exitFailure
    Right cfg -> do
      inputResult <- resolveInput cfg
      case inputResult of
        Left err -> hPutStrLn stderr err >> exitFailure
        Right inputText -> do
          case validateConfig cfg of
            Left err -> hPutStrLn stderr err >> exitFailure
            Right (mode, a, b) ->
              case mode of
                BruteForce ->
                  let combos = bruteForceAffine inputText
                   in if cfgJson cfg
                        then printJsonBruteforce combos
                        else mapM_ (\(ka, kb, plain) ->
                                      putStrLn (printf "a=%2d b=%2d: %s" ka kb plain)
                                   ) combos
                Encrypt -> runTransform Encrypt a b inputText cfg
                Decrypt -> runTransform Decrypt a b inputText cfg

runTransform :: Mode -> Int -> Int -> String -> Config -> IO ()
runTransform mode a b inputText cfg =
  case affineCipher mode inputText a b of
    Left err -> hPutStrLn stderr err >> exitFailure
    Right output ->
      if cfgJson cfg
        then printJsonResult mode a b inputText output
        else putStrLn output
