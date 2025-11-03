-- |
-- Module      : RPN
-- Description : Reverse Polish Notation (RPN) calculator mirroring the Python implementation.
-- Provides CLI, REPL, tracing, and JSON-formatted output compatible with the
-- visualisation tooling used by the Python version.

module Main (main) where

import Control.Exception (IOException, catch, displayException)
import Control.Monad (foldM, when)
import Data.Char (isSpace, toLower)
import Data.List (dropWhileEnd, intercalate)
import Data.Maybe (isJust)
import Numeric (showGFloat)
import System.Console.GetOpt
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hFlush, hPutStrLn, stdout, stderr)
import System.IO.Error (isEOFError)
import Text.Read (readMaybe)

-- | Runtime configuration for the evaluator.
data EvalConfig = EvalConfig
  { cfgDegrees :: !Bool
  , cfgTrace :: !Bool
  }

-- | Errors raised during evaluation.
newtype RPNError = RPNError { getErrorMessage :: String }
  deriving (Eq, Show)

-- | Trace snapshot emitted after processing each token.
data TraceStep = TraceStep
  { tsToken :: !String
  , tsStack :: ![Double]
  }

-- | Application command line options.
data Options = Options
  { optExpr :: !(Maybe String)
  , optFile :: !(Maybe FilePath)
  , optStdin :: !Bool
  , optRepl :: !Bool
  , optDegrees :: !Bool
  , optTrace :: !Bool
  , optJson :: !Bool
  }
  deriving (Show)

emptyOptions :: Options
emptyOptions = Options Nothing Nothing False False False False False

optionsDescr :: [OptDescr (Options -> Options)]
optionsDescr =
  [ Option [] ["expr"] (ReqArg (\s o -> o { optExpr = Just s }) "EXPR")
      "RPN expression string to evaluate"
  , Option [] ["file"] (ReqArg (\s o -> o { optFile = Just s }) "PATH")
      "Read RPN expression from file"
  , Option [] ["stdin"] (NoArg (\o -> o { optStdin = True }))
      "Read RPN expression from STDIN"
  , Option [] ["repl"] (NoArg (\o -> o { optRepl = True }))
      "Run an interactive REPL"
  , Option [] ["deg"] (NoArg (\o -> o { optDegrees = True }))
      "Interpret trigonometric inputs as degrees"
  , Option [] ["trace"] (NoArg (\o -> o { optTrace = True }))
      "Print stack trace after each token"
  , Option [] ["json"] (NoArg (\o -> o { optJson = True }))
      "Emit JSON output for results, traces, and errors"
  ]

-- | Main entry point.
main :: IO ()
main = do
  argv <- getArgs
  let (optsTransformers, _, errs) = getOpt Permute optionsDescr argv
  when (not (null errs)) $ do
    mapM_ (hPutStrLn stderr) errs
    exitFailure
  let opts = foldl (flip ($)) emptyOptions optsTransformers
      cfg = EvalConfig (optDegrees opts) (optTrace opts)
  if optRepl opts
    then runRepl cfg (optJson opts)
    else runOnce opts cfg

-- | Execute a single-shot evaluation based on CLI flags.
runOnce :: Options -> EvalConfig -> IO ()
runOnce opts cfg = do
  source <- resolveSource opts
  case source of
    Left err -> emitError (optJson opts) err
    Right expr -> do
      let trimmed = trim expr
      case evaluateRPN cfg trimmed of
        Left err -> emitError (optJson opts) err
        Right (value, traceSteps) ->
          if optJson opts
            then emitJsonSuccess trimmed value traceSteps (cfgTrace cfg)
            else do
              when (cfgTrace cfg) $ mapM_ (putStrLn . formatTrace) traceSteps
              putStrLn (formatNumber value)

-- | Launch the REPL loop.
runRepl :: EvalConfig -> Bool -> IO ()
runRepl cfg useJson = do
  putStrLn "RPN REPL. Type 'quit' or Ctrl-D to exit."
  loop
  where
    loop = do
      putStr "> "
      hFlush stdout
      line <- safeGetLine
      case line of
        Nothing -> putStrLn "" >> exitSuccess
        Just rawLine -> do
          let inputLine = trim rawLine
              lowered = map toLower inputLine
          if lowered `elem` ["quit", "exit"]
            then exitSuccess
            else do
              if null inputLine
                then loop
                else
                  case evaluateRPN cfg inputLine of
                    Left err ->
                      if useJson
                        then emitJsonError err >> hFlush stdout
                        else putStrLn ("Error: " ++ getErrorMessage err)
                    Right (value, traceSteps) ->
                      if useJson
                        then emitJsonSuccess inputLine value traceSteps (cfgTrace cfg)
                        else do
                          when (cfgTrace cfg) $ mapM_ (putStrLn . formatTrace) traceSteps
                          putStrLn (formatNumber value)
              loop

-- | Read a line from stdin, returning Nothing on EOF.
safeGetLine :: IO (Maybe String)
safeGetLine = catch (Just <$> getLine) handler
  where
    handler :: IOException -> IO (Maybe String)
    handler e = if isEOFError e then pure Nothing else ioError e

-- | Resolve expression source according to provided options.
resolveSource :: Options -> IO (Either RPNError String)
resolveSource opts
  | optRepl opts = pure (Right "")
  | sourcesCount > 1 = pure (Left (RPNError "Specify only one of --expr/--file/--stdin (or use --repl)"))
  | sourcesCount == 0 = pure (Left (RPNError "No expression source provided (use --expr or --repl)"))
  | otherwise =
      case (optExpr opts, optFile opts, optStdin opts) of
        (Just s, _, _) -> pure (Right s)
        (_, Just path, _) -> readFileSafe path
        (_, _, True) -> do
          contents <- getContents
          pure (Right contents)
        _ -> pure (Left (RPNError "Unhandled input source"))
  where
    sourcesCount = length $ filter id [isJust (optExpr opts), isJust (optFile opts), optStdin opts]

readFileSafe :: FilePath -> IO (Either RPNError String)
readFileSafe path = catch (Right <$> readFile path) handler
  where
    handler :: IOException -> IO (Either RPNError String)
    handler e = pure (Left (RPNError ("Failed to read file: " ++ displayException e)))

-- | Evaluate an RPN expression, optionally recording the stack trace.
evaluateRPN :: EvalConfig -> String -> Either RPNError (Double, [TraceStep])
evaluateRPN cfg expression =
  let tokens = words expression
   in if null tokens
        then Left (RPNError "Empty expression")
        else do
          (stack, stepsRev) <- foldM (stepToken cfg) ([], []) tokens
          case stack of
            [result] ->
              let steps = if cfgTrace cfg then reverse stepsRev else []
               in Right (result, steps)
            [] -> Left (RPNError "Malformed expression (empty stack)")
            _ -> Left (RPNError "Malformed expression (items remaining on stack)")

stepToken :: EvalConfig -> ([Double], [TraceStep]) -> String -> Either RPNError ([Double], [TraceStep])
stepToken cfg (stack, steps) token = do
  stack' <- applyToken cfg token stack
  let newSteps =
        if cfgTrace cfg
          then TraceStep token (reverse stack') : steps
          else steps
  pure (stack', newSteps)

applyToken :: EvalConfig -> String -> [Double] -> Either RPNError [Double]
applyToken cfg token stack =
  case (readMaybe token :: Maybe Double) of
    Just v -> Right (v : stack)
    Nothing -> case token of
      "pi" -> pushValue pi
      "tau" -> pushValue (2 * pi)
      "e" -> pushValue (exp 1)
      "!" -> factorialOp
      "neg" -> unary "neg" (Right . negate)
      "sqrt" -> unary "sqrt" sqrtChecked
      "exp" -> unary "exp" (Right . exp)
      "ln" -> unary "ln" lnChecked
      "log" -> unary "log" log10Checked
      "sin" -> unary "sin" (Right . trigFunc sin)
      "cos" -> unary "cos" (Right . trigFunc cos)
      "tan" -> unary "tan" (Right . trigFunc tan)
      "+" -> binSimple "+" (+)
      "-" -> binSimple "-" (-)
      "*" -> binSimple "*" (*)
      "x" -> binSimple "x" (*)
      "/" -> binOp "/" division
      "//" -> binOp "//" floorDivision
      "%" -> binOp "%" modulo
      "^" -> binSimple "^" (**)
      _ -> Left (RPNError ("Unknown token '" ++ token ++ "'"))
  where
    pushValue v = Right (v : stack)

    unary :: String -> (Double -> Either RPNError Double) -> Either RPNError [Double]
    unary name f = do
      (x, rest) <- popOne name stack
      result <- f x
      Right (result : rest)

    binSimple :: String -> (Double -> Double -> Double) -> Either RPNError [Double]
    binSimple name f = binOp name (\a b -> Right (f a b))

    binOp :: String -> (Double -> Double -> Either RPNError Double) -> Either RPNError [Double]
    binOp name f = do
      (a, b, rest) <- popTwo name stack
      result <- f a b
      Right (result : rest)

    factorialOp :: Either RPNError [Double]
    factorialOp =
      case stack of
        [] -> Left (RPNError "Factorial requires one operand")
        (x : xs) ->
          if x < 0 || not (isWhole x)
            then Left (RPNError "Factorial operand must be a non-negative integer")
            else
              let n = round x :: Integer
                  value = fromIntegral (product [1 .. n])
               in Right (value : xs)

    sqrtChecked :: Double -> Either RPNError Double
    sqrtChecked x
      | x < 0 = Left (RPNError "Math domain error")
      | otherwise = Right (sqrt x)

    lnChecked :: Double -> Either RPNError Double
    lnChecked x
      | x <= 0 = Left (RPNError "Math domain error")
      | otherwise = Right (log x)

    log10Checked :: Double -> Either RPNError Double
    log10Checked x
      | x <= 0 = Left (RPNError "Math domain error")
      | otherwise = Right (logBase 10 x)

    trigFunc :: (Double -> Double) -> Double -> Double
    trigFunc f angle =
      let radians = if cfgDegrees cfg then angle * pi / 180 else angle
       in f radians

    division :: Double -> Double -> Either RPNError Double
    division a b
      | b == 0 = Left (RPNError "Division by zero")
      | otherwise = Right (a / b)

    floorDivision :: Double -> Double -> Either RPNError Double
    floorDivision a b
      | b == 0 = Left (RPNError "Division by zero")
      | otherwise = Right (fromIntegral (floor (a / b) :: Integer))

    modulo :: Double -> Double -> Either RPNError Double
    modulo a b
      | b == 0 = Left (RPNError "Division by zero")
      | otherwise =
          let q = fromIntegral (floor (a / b) :: Integer)
           in Right (a - q * b)

    isWhole :: Double -> Bool
    isWhole x = abs (x - fromInteger (round x)) < 1e-9

-- | Pop a single value from the stack.
popOne :: String -> [Double] -> Either RPNError (Double, [Double])
popOne name stack =
  case stack of
    (x : xs) -> Right (x, xs)
    _ -> Left (RPNError ("Unary operator '" ++ name ++ "' requires one operand"))

-- | Pop two values from the stack (a is lower, b is top).
popTwo :: String -> [Double] -> Either RPNError (Double, Double, [Double])
popTwo name stack =
  case stack of
    (b : a : rest) -> Right (a, b, rest)
    _ -> Left (RPNError ("Operator '" ++ name ++ "' requires two operands"))

-- | Emit a textual or JSON error and terminate.
emitError :: Bool -> RPNError -> IO a
emitError useJson err = do
  if useJson
    then emitJsonError err
    else hPutStrLn stderr ("Error: " ++ getErrorMessage err)
  exitFailure

emitJsonError :: RPNError -> IO ()
emitJsonError (RPNError msg) =
  putStrLn $ unlines
    [ "{"
    , "  \"error\": " ++ quote msg
    , "}"
    ]

emitJsonSuccess :: String -> Double -> [TraceStep] -> Bool -> IO ()
emitJsonSuccess expr value traceSteps traceEnabled =
  putStrLn $ unlines $
    [ "{"
    , "  \"expression\": " ++ quote expr ++ ","
    , "  \"result\": " ++ show value ++ traceSuffix
    ] ++ traceLines ++ ["}"]
  where
    traceSuffix = if traceEnabled && not (null traceSteps) then "," else ""
    traceLines
      | traceEnabled =
          let serialized = traceToJson traceSteps
           in ["  \"trace\": " ++ serialized]
      | otherwise = []

traceToJson :: [TraceStep] -> String
traceToJson steps =
  "[" ++ intercalate ", " (map stepToJson steps) ++ "]"
  where
    stepToJson (TraceStep tok stk) =
      "{\"token\": " ++ quote tok ++ ", \"stack\": [" ++ intercalate ", " (map show stk) ++ "]}"

-- | Format a trace snapshot for textual output.
formatTrace :: TraceStep -> String
formatTrace (TraceStep tok stk) =
  "token=" ++ padLeft 6 tok ++ " stack=" ++ formatStack stk

formatStack :: [Double] -> String
formatStack values =
  "[" ++ intercalate ", " (map formatNumber values) ++ "]"

formatNumber :: Double -> String
formatNumber x = showGFloat Nothing x ""

padLeft :: Int -> String -> String
padLeft width str = replicate (max 0 (width - length str)) ' ' ++ str

trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace

jsonEscape :: String -> String
jsonEscape = concatMap escapeChar
  where
    escapeChar '"' = "\\\""
    escapeChar '\\' = "\\\\"
    escapeChar '\n' = "\\n"
    escapeChar '\r' = "\\r"
    escapeChar '\t' = "\\t"
    escapeChar c
      | c < ' ' = unicodeEscape (fromEnum c)
      | otherwise = [c]
    unicodeEscape n =
      let hex = "0123456789abcdef"
          d1 = hex !! ((n `div` 16) `mod` 16)
          d0 = hex !! (n `mod` 16)
       in ['\\', 'u', '0', '0', d1, d0]

quote :: String -> String
quote s = "\"" ++ jsonEscape s ++ "\""
