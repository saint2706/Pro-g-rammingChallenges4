module Main (main) where

import Control.Monad (foldM)
import Data.Char (ord)
import Data.List (intercalate)
import Data.Maybe (catMaybes, fromMaybe)
import Numeric (showHex)
import System.Console.GetOpt
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.IO (IOMode (WriteMode), hPutStr, hPutStrLn, stderr, withFile)

-- | Represents a single FizzBuzz rule mapping a divisor to a label.
data Rule = Rule
  { ruleDivisor :: !Int
  , ruleLabel :: !String
  }
  deriving (Eq, Show)

-- | Supported output formats.
data Format = Plain | Json | Csv deriving (Eq, Show)

-- | Command line options mirroring the Python script defaults.
data Options = Options
  { optLimit :: !Int
  , optRules :: !(Maybe [Rule])
  , optFormat :: !Format
  , optIncludeNumbers :: !Bool
  , optOutput :: !(Maybe FilePath)
  }
  deriving (Show)

defaultOptions :: Options
defaultOptions =
  Options
    { optLimit = 100
    , optRules = Nothing
    , optFormat = Plain
    , optIncludeNumbers = True
    , optOutput = Nothing
    }

optionDescriptors :: [OptDescr (Options -> Either String Options)]
optionDescriptors =
  [ Option ['n'] ["limit"] (ReqArg setLimit "N") "Upper limit (inclusive). Default: 100"
  , Option [] ["rule"] (ReqArg addRule "DIVISOR:LABEL") "Add a custom rule. Overrides defaults if provided."
  , Option [] ["format"] (ReqArg setFormat "FORMAT") "Output format: plain, json, csv"
  , Option [] ["no-numbers"] (NoArg disableNumbers) "Suppress raw numbers when no rule matches"
  , Option ['o'] ["output"] (ReqArg setOutput "FILE") "Write output to FILE instead of stdout"
  ]
  where
    setLimit raw opts =
      case reads raw of
        [(n, "")]
          | n >= 1 -> Right opts {optLimit = n}
          | otherwise -> Left "Error: limit must be >= 1"
        _ -> Left "Error: limit must be an integer"
    addRule raw opts = do
      rule <- parseRule raw
      let nextRules = maybe [rule] (++ [rule]) (optRules opts)
      pure opts {optRules = Just nextRules}
    setFormat raw opts =
      case raw of
        "plain" -> Right opts {optFormat = Plain}
        "json" -> Right opts {optFormat = Json}
        "csv" -> Right opts {optFormat = Csv}
        _ -> Left "Error: unsupported format (choose plain, json, or csv)"
    disableNumbers opts = Right opts {optIncludeNumbers = False}
    setOutput path opts = Right opts {optOutput = Just path}

parseRule :: String -> Either String Rule
parseRule spec =
  case break (== ':') spec of
    (divPart, ':' : label)
      | null label -> Left "Error: label must be non-empty"
      | otherwise ->
          case reads divPart of
            [(n, "")]
              | n > 0 -> Right (Rule n label)
              | otherwise -> Left "Error: divisor must be a positive integer"
            _ -> Left "Error: divisor must be an integer"
    _ -> Left "Error: rule must be in form DIVISOR:LABEL"

-- | Default classic FizzBuzz rules.
defaultRules :: [Rule]
defaultRules = [Rule 3 "Fizz", Rule 5 "Buzz"]

main :: IO ()
main = do
  args <- getArgs
  case getOpt Permute optionDescriptors args of
    (actions, nonOpts, [])
      | null nonOpts ->
          case applyActions actions defaultOptions of
            Left err -> die err
            Right opts -> runWithOptions opts
      | otherwise -> die "Error: unexpected positional arguments"
    (_, _, errs) -> die (concat errs)

applyActions :: [Options -> Either String Options] -> Options -> Either String Options
applyActions actions start = foldM (flip ($)) start actions

runWithOptions :: Options -> IO ()
runWithOptions opts = do
  let limit = optLimit opts
      rules = fromMaybe defaultRules (optRules opts)
      includeNumbers = optIncludeNumbers opts
      formatChoice = optFormat opts
      outputPath = optOutput opts
      sequenceValues = fizzBuzz limit rules includeNumbers
      rendered = formatOutput formatChoice sequenceValues
      filePayload =
        case formatChoice of
          Plain -> rendered ++ "\n"
          _ -> rendered
  case outputPath of
    Just path -> withFile path WriteMode $ \handle -> hPutStr handle filePayload
    Nothing -> putStrLn rendered

fizzBuzz :: Int -> [Rule] -> Bool -> [String]
fizzBuzz limit rules includeNumbers =
  catMaybes (map renderValue [1 .. limit])
  where
    renderValue n =
      let labels = [ruleLabel rule | rule <- rules, n `mod` ruleDivisor rule == 0]
       in case labels of
            []
              | includeNumbers -> Just (show n)
              | otherwise -> Nothing
            xs -> Just (concat xs)

formatOutput :: Format -> [String] -> String
formatOutput choice values =
  case choice of
    Plain -> intercalate "\n" values
    Json -> "[" ++ intercalate "," (map jsonString values) ++ "]"
    Csv -> intercalate "," (map csvEscape values)

jsonString :: String -> String
jsonString str = '"' : go str
  where
    go [] = "\""
    go (c : cs) = escapeChar c ++ go cs
    escapeChar '"' = "\\\""
    escapeChar '\\' = "\\\\"
    escapeChar '\b' = "\\b"
    escapeChar '\f' = "\\f"
    escapeChar '\n' = "\\n"
    escapeChar '\r' = "\\r"
    escapeChar '\t' = "\\t"
    escapeChar ch
      | ord ch < 0x20 = "\\u" ++ pad4 (showHex (ord ch) "")
      | otherwise = [ch]
    pad4 s = replicate (4 - length s) '0' ++ s

csvEscape :: String -> String
csvEscape value
  | needsQuoting value = '"' : foldr quoteChar "\"" value
  | otherwise = value
  where
    needsQuoting v = any (`elem` v) [',', '\n', '\r', '"']
    quoteChar '"' acc = '"' : '"' : acc
    quoteChar c acc = c : acc

-- | Emit an error and exit with code 2, matching the Python CLI.
die :: String -> IO a
die msg = do
  hPutStrLn stderr msg
  exitWith (ExitFailure 2)
