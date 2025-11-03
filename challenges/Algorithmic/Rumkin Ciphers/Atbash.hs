module Main (main, atbashCipher) where

import Data.Char (chr, isAlpha, isLower, ord)
import Data.List (intercalate)
import Data.Maybe (isJust)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Text.Printf (printf)

alphabetSize :: Int
alphabetSize = 26

data Mode = Encrypt | Decrypt deriving (Eq, Show)

data Config = Config
  { cfgMode :: Maybe Mode
  , cfgText :: Maybe String
  , cfgStdin :: Bool
  , cfgJson :: Bool
  }
  deriving (Show)

defaultConfig :: Config
defaultConfig =
  Config
    { cfgMode = Nothing
    , cfgText = Nothing
    , cfgStdin = False
    , cfgJson = False
    }

parseArgs :: [String] -> Config -> Either String Config
parseArgs [] cfg = Right cfg
parseArgs ("--mode" : m : rest) cfg =
  case m of
    "encrypt" -> parseArgs rest cfg {cfgMode = Just Encrypt}
    "decrypt" -> parseArgs rest cfg {cfgMode = Just Decrypt}
    other -> Left ("Unknown mode: " ++ other)
parseArgs ("--text" : t : rest) cfg = parseArgs rest cfg {cfgText = Just t}
parseArgs ("--stdin" : rest) cfg = parseArgs rest cfg {cfgStdin = True}
parseArgs ("--json" : rest) cfg = parseArgs rest cfg {cfgJson = True}
parseArgs ("--help" : _) _ = Left usage
parseArgs (opt : _) _ = Left ("Unknown option: " ++ opt)

usage :: String
usage = intercalate "\n"
  [ "Atbash cipher tool (Haskell)"
  , ""
  , "Required flags:"
  , "  --mode <encrypt|decrypt>"
  , "  --text <value> (or use --stdin)"
  , "Optional flags:"
  , "  --json        Emit JSON payload"
  , "  --stdin       Read input from STDIN"
  , "Examples:"
  , "  ./Atbash --mode encrypt --text hello"
  , "  ./Atbash --mode decrypt --text svool --json"
  ]

resolveInput :: Config -> IO (Either String String)
resolveInput Config {..}
  | cfgStdin && isJust cfgText = pure (Left "Use either --text or --stdin, not both")
  | cfgStdin = Right <$> getContents
  | Just txt <- cfgText = pure (Right txt)
  | otherwise = pure (Left "Missing input: provide --text or --stdin")

validateConfig :: Config -> Either String Mode
validateConfig Config {..} =
  case cfgMode of
    Nothing -> Left "Missing --mode"
    Just mode -> Right mode

atbashChar :: Char -> Char
atbashChar ch
  | not (isAlpha ch) = ch
  | otherwise =
      let base = if isLower ch then ord 'a' else ord 'A'
          offset = ord ch - base
          mirrored = alphabetSize - 1 - offset
       in chr (base + mirrored)

atbashCipher :: String -> String
atbashCipher = map atbashChar

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

modeLabel :: Mode -> String
modeLabel Encrypt = "encrypt"
modeLabel Decrypt = "decrypt"

printJson :: Mode -> String -> String -> IO ()
printJson mode input output = do
  let payload = concat
        [ "{\n"
        , printf "  \"mode\": \"%s\",\n" (modeLabel mode)
        , printf "  \"input_length\": %d,\n" (length input)
        , printf "  \"output_length\": %d,\n" (length output)
        , printf "  \"sample_in\": \"%s\",\n" (jsonEscape (take 80 input))
        , printf "  \"sample_out\": \"%s\",\n" (jsonEscape (take 80 output))
        , printf "  \"mapping_upper\": \"%s\",\n" (reverse ['A' .. 'Z'])
        , printf "  \"mapping_lower\": \"%s\"\n" (reverse ['a' .. 'z'])
        , "}"
        ]
  putStrLn payload

main :: IO ()
main = do
  args <- getArgs
  case parseArgs args defaultConfig of
    Left err -> do
      hPutStrLn stderr err
      whenUsage err
      exitFailure
    Right cfg -> do
      inputResult <- resolveInput cfg
      case inputResult of
        Left err -> hPutStrLn stderr err >> exitFailure
        Right inputText ->
          case validateConfig cfg of
            Left err -> hPutStrLn stderr err >> exitFailure
            Right mode -> do
              let output = atbashCipher inputText
              if cfgJson cfg
                then printJson mode inputText output
                else putStrLn output

whenUsage :: String -> IO ()
whenUsage err
  | err == usage = pure ()
  | otherwise = hPutStrLn stderr usage
