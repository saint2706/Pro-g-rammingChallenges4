module Main (main) where

import Data.Char (chr, isAlpha, isLetter, isUpper, ord, toLower, toUpper)
import Data.List (intercalate, isPrefixOf)
import Data.Maybe (isJust)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, hSetEncoding, stderr, stdin, stdout, utf8)

-- | Cipher mode (encrypt or decrypt).
data Mode = ModeEncrypt | ModeDecrypt deriving (Eq, Show)

data Config = Config
  { cfgMode :: Mode
  , cfgKey :: String
  , cfgText :: Maybe String
  , cfgInFile :: Maybe FilePath
  , cfgOutFile :: Maybe FilePath
  , cfgJson :: Bool
  , cfgUpper :: Bool
  }

-- | Result of parsing CLI arguments.
data ParseResult
  = Parsed Config
  | ShowHelp
  | ParseError String

main :: IO ()
main = do
  setUtf8
  args <- getArgs
  case parseArgs args of
    ShowHelp -> do
      prog <- getProgName
      putStrLn (usage prog)
      exitSuccess
    ParseError err -> do
      hPutStrLn stderr err
      prog <- getProgName
      hPutStrLn stderr (usage prog)
      exitFailure
    Parsed cfg -> runWith cfg

setUtf8 :: IO ()
setUtf8 = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  hSetEncoding stdin utf8

runWith :: Config -> IO ()
runWith cfg = do
  case sanitizeKey (cfgKey cfg) of
    "" -> die "key must contain at least one alphabetic character after sanitization"
    _ -> pure ()
  input <- loadInput cfg
  let result = vigenereCipher (cfgMode cfg) (cfgKey cfg) (cfgUpper cfg) input
  if cfgJson cfg
    then emitJson cfg input result
    else emitOutput cfg result

emitOutput :: Config -> String -> IO ()
emitOutput cfg result =
  case cfgOutFile cfg of
    Just path -> writeFile path result
    Nothing -> putStrLn result

emitJson :: Config -> String -> String -> IO ()
emitJson cfg input result = do
  let sanitized = sanitizeKey (cfgKey cfg)
      payload =
        [ jsonPair "mode" (jsonString (modeLabel (cfgMode cfg)))
        , jsonPair "key_length" (jsonNumber (length sanitized))
        , jsonPair "input_length" (jsonNumber (length input))
        , jsonPair "output_length" (jsonNumber (length result))
        , jsonPair "result" (jsonString result)
        , jsonPair "upper" (jsonBool (cfgUpper cfg))
        , jsonPair "source" (jsonString (inputSource cfg))
        , jsonPair "outfile" (jsonBool (isJust (cfgOutFile cfg)))
        ]
      jsonText = buildJson payload
  case cfgOutFile cfg of
    Just path -> writeFile path jsonText
    Nothing -> putStrLn jsonText

inputSource :: Config -> String
inputSource cfg =
  case (cfgText cfg, cfgInFile cfg) of
    (Just _, _) -> "text"
    (Nothing, Just _) -> "file"
    _ -> "stdin"

loadInput :: Config -> IO String
loadInput cfg =
  case (cfgText cfg, cfgInFile cfg) of
    (Just txt, Nothing) -> pure txt
    (Nothing, Just path) -> readFile path
    (Just _, Just _) -> die "Provide either --text or --in, not both"
    (Nothing, Nothing) -> getContents

die :: String -> IO a
die msg = do
  hPutStrLn stderr msg
  exitFailure

modeLabel :: Mode -> String
modeLabel mode =
  case mode of
    ModeEncrypt -> "encrypt"
    ModeDecrypt -> "decrypt"

vigenereCipher :: Mode -> String -> Bool -> String -> String
vigenereCipher mode key makeUpper text =
  case sanitizeKey key of
    "" -> error "sanitizeKey should ensure non-empty keys before cipher"
    sanitized ->
      let len = length sanitized
       in go sanitized len 0 text
  where
    decryptMode = mode == ModeDecrypt
    go _ _ _ [] = []
    go sanitized len idx (c : cs)
      | isLetter c =
          let keyIdx = idx `mod` len
              shift = ord (sanitized !! keyIdx) - ord 'a'
              base = if isUpper c then ord 'A' else ord 'a'
              raw = ord c - base
              adj = if decryptMode then (raw - shift) else (raw + shift)
              rotated = chrMod base adj
              finalChar = if makeUpper then toUpper rotated else rotated
           in finalChar : go sanitized len (idx + 1) cs
      | otherwise = c : go sanitized len idx cs

chrMod :: Int -> Int -> Char
chrMod base value =
  let m = value `mod` 26
   in chr (base + m)

sanitizeKey :: String -> String
sanitizeKey = map toLower . filter isAlpha

buildJson :: [String] -> String
buildJson pairs =
  let indent = "  "
      inner = intercalate (",\n" ++ indent) pairs
   in "{\n" ++ indent ++ inner ++ "\n}"

jsonPair :: String -> String -> String
jsonPair key value = jsonString key ++ ": " ++ value

jsonString :: String -> String
jsonString s = "\"" ++ concatMap escapeChar s ++ "\""
  where
    escapeChar '"' = "\\\""
    escapeChar '\\' = "\\\\"
    escapeChar '\b' = "\\b"
    escapeChar '\f' = "\\f"
    escapeChar '\n' = "\\n"
    escapeChar '\r' = "\\r"
    escapeChar '\t' = "\\t"
    escapeChar c
      | c < ' ' = unicodeEscape c
      | otherwise = [c]
    unicodeEscape c =
      let code = fromEnum c
          hex = padLeft 4 '0' (showHex code)
       in "\\u" ++ hex

jsonNumber :: Int -> String
jsonNumber = show

jsonBool :: Bool -> String
jsonBool True = "true"
jsonBool False = "false"

padLeft :: Int -> Char -> String -> String
padLeft len ch str = replicate (max 0 (len - length str)) ch ++ str

showHex :: Int -> String
showHex n
  | n < 16 = [digits !! n]
  | otherwise = showHex (n `div` 16) ++ [digits !! (n `mod` 16)]
  where
    digits = "0123456789abcdef"

parseArgs :: [String] -> ParseResult
parseArgs args
  | "--help" `elem` args || "-h" `elem` args = ShowHelp
  | otherwise =
      case args of
        (modeStr : keyStr : rest) ->
          case toMode modeStr of
            Nothing -> ParseError "mode must be 'encrypt' or 'decrypt'"
            Just mode ->
              case parseOptions rest defaultConfig {cfgMode = mode, cfgKey = keyStr} of
                Left err -> ParseError err
                Right cfg -> Parsed cfg
        _ -> ParseError "Usage: vigenere MODE KEY [options]"

parseOptions :: [String] -> Config -> Either String Config
parseOptions [] cfg = validateConfig cfg
parseOptions (flag : rest) cfg =
  case flag of
    "-t" -> grabValue rest $ \val xs -> parseOptions xs cfg {cfgText = Just val}
    "--text" -> grabValue rest $ \val xs -> parseOptions xs cfg {cfgText = Just val}
    "--in" -> grabValue rest $ \val xs -> parseOptions xs cfg {cfgInFile = Just val}
    "--out" -> grabValue rest $ \val xs -> parseOptions xs cfg {cfgOutFile = Just val}
    "--json" -> parseOptions rest cfg {cfgJson = True}
    "--upper" -> parseOptions rest cfg {cfgUpper = True}
    unknown | "-" `isPrefixOf` unknown -> Left ("Unrecognized option: " ++ unknown)
    value -> Left ("Unexpected positional argument: " ++ value)
  where
    grabValue xs cont =
      case xs of
        (v : rest') -> cont v rest'
        [] -> Left ("Missing value for option " ++ flag)

validateConfig :: Config -> Either String Config
validateConfig cfg
  | cfgText cfg /= Nothing && cfgInFile cfg /= Nothing =
      Left "Provide either --text or --in, not both"
  | otherwise = Right cfg

usage :: String -> String
usage progName =
  unlines
    [ "Usage: " ++ progName ++ " MODE KEY [options]"
    , "\nOptions:"
    , "  -t, --text TEXT        Inline plaintext/ciphertext input"
    , "      --in PATH          Read input from file"
    , "      --out PATH         Write output to file"
    , "      --json             Emit JSON metadata"
    , "      --upper            Force alphabetic output to uppercase"
    , "  -h, --help            Show this help message"
    ]

defaultConfig :: Config
defaultConfig =
  Config
    { cfgMode = ModeEncrypt
    , cfgKey = ""
    , cfgText = Nothing
    , cfgInFile = Nothing
    , cfgOutFile = Nothing
    , cfgJson = False
    , cfgUpper = False
    }

toMode :: String -> Maybe Mode
toMode str
  | map toLower str == "encrypt" = Just ModeEncrypt
  | map toLower str == "decrypt" = Just ModeDecrypt
  | otherwise = Nothing
