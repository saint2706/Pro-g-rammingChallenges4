{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Rot13
-- Description : Command line ROT13 encoder/decoder mirroring the Python tool.
--
-- This executable provides the same behaviours as @rot13.py@: read input from
-- inline arguments, files, STDIN, or an interactive prompt; optionally save the
-- result to disk; and emit a JSON report for scripting pipelines.
--
-- The script is intentionally self contained and depends only on "base" so that
-- it can be executed with @runghc Rot13.hs@ without additional build tooling.
module Main (main, rot13, resolveInput, CLIOptions (..)) where

import Control.Exception (IOException, try)
import Data.Char (chr, isAsciiLower, isAsciiUpper, ord)
import Data.List (intercalate)
import Data.Maybe (isJust)
import System.Console.GetOpt
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO
import System.IO.Error (ioeGetErrorString)

-- | Apply the ROT13 transform to a single character.
rot13Char :: Char -> Char
rot13Char c
  | isAsciiLower c = rotate 'a'
  | isAsciiUpper c = rotate 'A'
  | otherwise = c
  where
    rotate base =
      let offset = ord c - ord base
       in chr (ord base + ((offset + 13) `mod` 26))

-- | Transform an entire string using ROT13.
rot13 :: String -> String
rot13 = map rot13Char

-- | Command line configuration for the executable.
data CLIOptions = CLIOptions
  { optText :: Maybe String
  , optFile :: Maybe FilePath
  , optStdin :: Bool
  , optSave :: Maybe FilePath
  , optJson :: Bool
  , optHelp :: Bool
  }
  deriving (Show)

-- | Defaults used prior to parsing CLI flags.
defaultOptions :: CLIOptions
defaultOptions =
  CLIOptions
    { optText = Nothing
    , optFile = Nothing
    , optStdin = False
    , optSave = Nothing
    , optJson = False
    , optHelp = False
    }

-- | Internal representation of command line flags while parsing.
data Flag
  = FlagText String
  | FlagFile FilePath
  | FlagStdin
  | FlagSave FilePath
  | FlagJson
  | FlagHelp
  deriving (Eq, Show)

-- | CLI specification mirroring the Python script's options.
options :: [OptDescr Flag]
options =
  [ Option [] ["text"] (ReqArg FlagText "TEXT") "Text to transform (bypasses interactive prompt)"
  , Option [] ["file"] (ReqArg FlagFile "FILE") "Read input text from file (UTF-8)"
  , Option [] ["stdin"] (NoArg FlagStdin) "Read entire STDIN as input"
  , Option [] ["save"] (ReqArg FlagSave "FILE") "Write transformed output to file"
  , Option [] ["json"] (NoArg FlagJson) "Emit JSON with input & output"
  , Option ['h'] ["help"] (NoArg FlagHelp) "Show this help text"
  ]

usageHeader :: String
usageHeader = "Usage: runghc Rot13.hs [OPTIONS]"

-- | Apply a parsed flag to the configuration.
applyFlag :: CLIOptions -> Flag -> CLIOptions
applyFlag opts flag =
  case flag of
    FlagText t -> opts {optText = Just t}
    FlagFile f -> opts {optFile = Just f}
    FlagStdin -> opts {optStdin = True}
    FlagSave p -> opts {optSave = Just p}
    FlagJson -> opts {optJson = True}
    FlagHelp -> opts {optHelp = True}

-- | Resolve which input source should be used, enforcing exclusivity.
resolveInput :: CLIOptions -> IO (Either String String)
resolveInput cfg = do
  let sources = [isJust (optText cfg), isJust (optFile cfg), optStdin cfg]
  if length (filter id sources) > 1
    then pure (Left "Specify only one of --text / --file / --stdin")
    else case (optText cfg, optFile cfg, optStdin cfg) of
      (Just t, _, _) -> pure (Right t)
      (Nothing, Just path, _) -> readFromFile path
      (Nothing, Nothing, True) -> Right <$> hGetContents stdin
      (Nothing, Nothing, False) -> interactivePrompt
  where
    readFromFile :: FilePath -> IO (Either String String)
    readFromFile path = do
      result <- try (readFile path)
      case result of
        Left (err :: IOException) -> pure (Left ("Failed to read file: " ++ displayIOError err))
        Right contents -> pure (Right contents)

    interactivePrompt :: IO (Either String String)
    interactivePrompt = do
      putStr "Enter text to apply ROT13 to: "
      hFlush stdout
      result <- try getLine
      case result of
        Left (_ :: IOException) -> pure (Left "No input provided")
        Right line -> pure (Right line)

-- | Serialize a string for JSON output (minimal escaping required here).
escapeJsonString :: String -> String
escapeJsonString = concatMap escapeChar
  where
    escapeChar '"' = "\\\""
    escapeChar '\\' = "\\\\"
    escapeChar '\b' = "\\b"
    escapeChar '\f' = "\\f"
    escapeChar '\n' = "\\n"
    escapeChar '\r' = "\\r"
    escapeChar '\t' = "\\t"
    escapeChar c
      | c < ' ' = "\\u" ++ hex4 (fromEnum c)
      | otherwise = [c]
    hex4 n =
      let digits = "0123456789abcdef"
          a = n `div` 4096
          b = (n `div` 256) `mod` 16
          c = (n `div` 16) `mod` 16
          d = n `mod` 16
       in [digits !! a, digits !! b, digits !! c, digits !! d]

-- | Render a JSON payload equivalent to the Python implementation.
renderJsonReport :: String -> String -> String
renderJsonReport original transformed =
  let inLen = length original
      outLen = length transformed
      sameLen = inLen == outLen
      sample n = take 80 n
      boolText True = "true"
      boolText False = "false"
      lines' =
        [ "{"
        , "  \"input_length\": " ++ show inLen ++ ","
        , "  \"output_length\": " ++ show outLen ++ ","
        , "  \"same_length\": " ++ boolText sameLen ++ ","
        , "  \"input_sample\": \"" ++ escapeJsonString (sample original) ++ "\","
        , "  \"output_sample\": \"" ++ escapeJsonString (sample transformed) ++ "\","
        , "  \"rot13_twice_equals_original\": " ++ boolText (rot13 transformed == original)
        , "}"
        ]
   in intercalate "\n" lines'

-- | Produce a human-readable IO error string.
displayIOError :: IOException -> String
displayIOError = ioeGetErrorString

main :: IO ()
main = do
  args <- getArgs
  let (flags, nonOpts, errs) = getOpt Permute options args
  unless (null errs) $ do
    hPutStr stderr (concat errs ++ usageInfo usageHeader options)
    exitFailure
  unless (null nonOpts) $ do
    hPutStr stderr ("Unexpected arguments: " ++ unwords nonOpts ++ "\n")
    hPutStr stderr (usageInfo usageHeader options)
    exitFailure
  let cfg = foldl applyFlag defaultOptions flags
  when (optHelp cfg) $ do
    putStr (usageInfo usageHeader options)
    exitSuccess
  inputResult <- resolveInput cfg
  case inputResult of
    Left err -> do
      hPutStrLn stderr ("Error: " ++ err)
      exitFailure
    Right original -> do
      let transformed = rot13 original
      case optSave cfg of
        Just path -> do
          writeResult <- try (writeFile path transformed)
          case writeResult of
            Left (e :: IOException) -> do
              hPutStrLn stderr ("Error writing output file: " ++ displayIOError e)
              exitFailure
            Right () -> pure ()
        Nothing -> pure ()
      if optJson cfg
        then putStrLn (renderJsonReport original transformed)
        else putStrLn transformed

-- | Simple re-implementations of Prelude helpers to avoid redundant imports.
unless :: Bool -> IO () -> IO ()
unless condition action = if condition then pure () else action

when :: Bool -> IO () -> IO ()
when condition action = if condition then action else pure ()
