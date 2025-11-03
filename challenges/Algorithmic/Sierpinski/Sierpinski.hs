{-# LANGUAGE RecordWildCards #-}

module Main
  ( main
  , generateSierpinskiLines
  , generateSierpinski
  , Config(..)
  , powerOfTwo
  ) where

import Control.Exception (IOException, displayException, try)
import Control.Monad (unless)
import Data.Bits ((.&.))
import Data.Char (isSpace)
import Data.List (intercalate)
import Data.Maybe (listToMaybe)
import Numeric (showHex)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)
import Text.Read (readMaybe)

-- | Runtime configuration for the executable.
data Config = Config
  { cfgSize :: !Int
  , cfgChar :: !String
  , cfgJson :: !Bool
  , cfgSave :: !(Maybe FilePath)
  } deriving (Eq, Show)

data ParseResult
  = Parsed Config
  | HelpRequested

-- | Entry point for the executable.
main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    Left err -> do
      hPutStrLn stderr $ "Error: " ++ err
      exitFailure
    Right HelpRequested -> do
      putStrLn usage
      exitSuccess
    Right (Parsed cfg0) ->
      case validateConfig cfg0 of
        Left err -> do
          hPutStrLn stderr $ "Error: " ++ err
          exitFailure
        Right cfg -> runWith cfg

-- | Validate the parsed configuration.
validateConfig :: Config -> Either String Config
validateConfig cfg@Config {..}
  | cfgSize <= 0 = Left "size must be positive"
  | length cfgChar /= 1 = Left "char must be a single character"
  | otherwise = Right cfg

runWith :: Config -> IO ()
runWith cfg@Config {..} = do
  let lines' = generateSierpinskiLines cfgSize cfgChar
      outputText = intercalate "\n" lines'
  if cfgJson
    then do
      let payload = encodeMetadata cfg lines'
      case cfgSave of
        Just path -> writeOrFail path payload
        Nothing -> pure ()
      putStrLn payload
    else do
      unless (powerOfTwo cfgSize) $
        hPutStrLn stderr "Warning: size is not a power of two; pattern may look irregular."
      case cfgSave of
        Just path -> writeOrFail path (outputText ++ "\n")
        Nothing -> pure ()
      putStrLn outputText

writeOrFail :: FilePath -> String -> IO ()
writeOrFail path content = do
  result <- try (writeFile path content) :: IO (Either IOException ())
  case result of
    Left e -> do
      hPutStrLn stderr $ "Error writing file: " ++ displayException e
      exitFailure
    Right _ -> pure ()

usage :: String
usage = unlines
  [ "Generate an ASCII Sierpinski triangle"
  , ""
  , "Usage: Sierpinski [--size N] [--char C] [--json] [--save FILE]"
  , ""
  , "  --size N     Triangle height (power of two recommended)"
  , "  --char C     Fill character (default: *)"
  , "  --json       Emit JSON metadata instead of raw triangle text"
  , "  --save FILE  Write output (text or JSON) to file"
  , "  --help       Show this help text"
  ]

defaultConfig :: Config
defaultConfig = Config
  { cfgSize = 8
  , cfgChar = "*"
  , cfgJson = False
  , cfgSave = Nothing
  }

parseArgs :: [String] -> Either String ParseResult
parseArgs = go defaultConfig
  where
    go cfg [] = Right (Parsed cfg)
    go _ ("--help":_) = Right HelpRequested
    go _ ("-h":_) = Right HelpRequested
    go _ ("-?":_) = Right HelpRequested
    go cfg ("--size":val:rest) =
      case readMaybe val of
        Nothing -> Left "--size requires an integer argument"
        Just n -> go (cfg { cfgSize = n }) rest
    go _ ("--size":[]) = Left "--size requires an integer argument"
    go cfg ("--char":val:rest)
      | null val = Left "--char requires a non-empty character argument"
      | otherwise = go (cfg { cfgChar = val }) rest
    go _ ("--char":[]) = Left "--char requires a non-empty character argument"
    go cfg ("--json":rest) = go (cfg { cfgJson = True }) rest
    go cfg ("--save":path:rest) = go (cfg { cfgSave = Just path }) rest
    go _ ("--save":[]) = Left "--save requires a file path argument"
    go _ (flag:_) = Left $ "Unrecognized option: " ++ flag

-- | Determine if the provided integer is a power of two.
powerOfTwo :: Int -> Bool
powerOfTwo n = n > 0 && (n .&. (n - 1)) == 0

-- | Generate Sierpinski triangle lines using bitwise rules.
generateSierpinskiLines :: Int -> String -> [String]
generateSierpinskiLines size charToken =
  [ rstrip $ replicate y ' ' ++ concatMap (cell calcY) [0 .. size - y - 1]
  | y <- [0 .. size - 1]
  , let calcY = size - 1 - y
  ]
  where
    cell calcY x
      | (x .&. calcY) == 0 = charToken ++ " "
      | otherwise = "  "
    rstrip = reverse . dropWhile isSpace . reverse

generateSierpinski :: Int -> String -> String
generateSierpinski size charToken = intercalate "\n" (generateSierpinskiLines size charToken)

encodeMetadata :: Config -> [String] -> String
encodeMetadata Config {..} lines' =
  let totalCells = sum (map countNonSpaces lines')
      lineCount = length lines'
      firstLineWidth = maybe 0 length (listToMaybe lines')
      densityValue =
        if cfgSize <= 0
          then 0.0
          else fromIntegral totalCells / fromIntegral (cfgSize * cfgSize)
      densityText = show densityValue
  in intercalate "\n"
      [ "{"
      , "  \"size\": " ++ show cfgSize ++ ","
      , "  \"char\": \"" ++ jsonEscape cfgChar ++ ['"', ',']
      , "  \"power_of_two\": " ++ mapBool (powerOfTwo cfgSize) ++ ","
      , "  \"lines\": " ++ show lineCount ++ ","
      , "  \"width_first_line\": " ++ show firstLineWidth ++ ","
      , "  \"non_space_chars\": " ++ show totalCells ++ ","
      , "  \"density\": " ++ densityText
      , "}"
      ]

countNonSpaces :: String -> Int
countNonSpaces = length . filter (/= ' ')

mapBool :: Bool -> String
mapBool True = "true"
mapBool False = "false"

jsonEscape :: String -> String
jsonEscape = concatMap escapeChar
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
          hex = showHex code ""
          padded = replicate (4 - length hex) '0' ++ hex
      in "\\u" ++ padded
