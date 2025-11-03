{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : DigitPi
Description : High-precision computation of π using the Chudnovsky series.
Copyright   : (c) Programming Challenges
License     : MIT
Maintainer  : Programming Challenges

This executable mirrors the Python implementation found in @'DigitPi.py',
providing a command line interface with configurable iteration counts,
precision targets, verification helpers and optional progress output.  The
Chudnovsky series converges rapidly (≈14 digits per iteration), making it a
practical choice for computing thousands of digits of π with arbitrary
precision arithmetic provided by Haskell's exact 'Rational' type.
-}
module Main (main) where

import Control.Monad (when)
import Data.Char (isDigit)
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Ratio ((%), Rational)
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import Numeric (showFFloat)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)
import Text.Read (readMaybe)

-- | Command line configuration.
data Options = Options
  { optIterations   :: !Int
  , optPrecision    :: !(Maybe Int)
  , optVerifyDigits :: !(Maybe Int)
  , optVerifyFile   :: !(Maybe FilePath)
  , optProgress     :: !Bool
  }
  deriving (Show)

-- | Default CLI configuration mirrors the Python defaults.
defaultOptions :: Options
defaultOptions = Options
  { optIterations = 7
  , optPrecision = Nothing
  , optVerifyDigits = Just 50
  , optVerifyFile = Nothing
  , optProgress = False
  }

usageText :: String
usageText = unlines
  [ "Usage: DigitPi [OPTIONS]"
  , ""
  , "High-precision π calculator using the Chudnovsky series."
  , ""
  , "Options:"
  , "  --iterations N       Number of Chudnovsky iterations (default: 7)"
  , "  --precision N        Decimal digits to output (defaults to estimated precision)"
  , "  --verify-digits N    Verify the first N digits against a reference (default: 50)"
  , "  --verify-file PATH   Use PATH as the reference digits source"
  , "  --no-verify          Disable digit verification"
  , "  --progress           Print per-iteration progress updates"
  , "  -h, --help           Show this help text"
  ]

-- | Parse CLI arguments into an 'Options' structure.
parseArgs :: [String] -> Either String Options
parseArgs = go defaultOptions
  where
    go opts [] = Right opts
    go opts ("--iterations" : value : rest) =
      case readMaybe value of
        Just (n :: Int) | n > 0 -> go opts{optIterations = n} rest
        _ -> Left "--iterations expects a positive integer"
    go _    ("--iterations" : _) = Left "--iterations requires a value"

    go opts ("--precision" : value : rest) =
      case readMaybe value of
        Just (n :: Int) | n >= 0 -> go opts{optPrecision = Just n} rest
        _ -> Left "--precision expects a non-negative integer"
    go _    ("--precision" : _) = Left "--precision requires a value"

    go opts ("--verify-digits" : value : rest) =
      case readMaybe value of
        Just (n :: Int) | n > 0 -> go opts{optVerifyDigits = Just n} rest
        Just _ -> go opts{optVerifyDigits = Nothing} rest
        _ -> Left "--verify-digits expects an integer"
    go _    ("--verify-digits" : _) = Left "--verify-digits requires a value"

    go opts ("--verify-file" : path : rest) =
      go opts{optVerifyFile = Just path} rest
    go _    ("--verify-file" : _) = Left "--verify-file requires a path"

    go opts ("--no-verify" : rest) = go opts{optVerifyDigits = Nothing} rest
    go opts ("--progress" : rest) = go opts{optProgress = True} rest
    go _    (flag : _) = Left ("Unrecognized option: " ++ flag)

-- | Estimate decimal precision gained from a given number of iterations.
estimatePrecision :: Int -> Int
estimatePrecision iterations = max 1 (floor (fromIntegral iterations * 14.181647462725477 :: Double))

-- | Extra digits used internally to protect against rounding drift.
precisionGuard :: Int
precisionGuard = 10

-- | Newton's method for \(\sqrt{n}\) with a rational stopping criterion.
sqrtRationalPrecision :: Integer -> Int -> Rational
sqrtRationalPrecision n digits
  | n <= 0 = 0
  | otherwise = newton initialGuess
  where
    guardDigits = digits + precisionGuard
    epsilon :: Rational
    epsilon = 1 % (10 ^ (guardDigits + 5))
    target = fromIntegral n % 1

    initialGuess :: Rational
    initialGuess =
      let approx = sqrt (fromIntegral n :: Double)
       in if approx <= 0 then 1 else toRational approx

    newton :: Rational -> Rational
    newton x
      | abs (x - next) < epsilon = next
      | otherwise = newton next
      where
        next = (x + target / x) / 2

-- | Compute π using the Chudnovsky series.
computePi :: Int -> Int -> Bool -> Int -> IO Rational
computePi iterations digits showProgress previewDigits = do
  let sqrtTerm = sqrtRationalPrecision 10005 digits
      constantMultiplier = (426880 % 1) * sqrtTerm
      factorialMultiplier = 545140134 % 1
      exponentialDivisor = 262537412640768000 % 1
      initialLinear = 13591409 % 1
      initialSeries = initialLinear
      initialFactor = 1 % 1
      initialExponential = 1 % 1
      initialPi = constantMultiplier / initialSeries
  start <- getCurrentTime
  when showProgress $ reportProgress start 0 initialPi
  iterateSeries start 1 initialFactor initialLinear initialExponential initialSeries initialPi
  where
    reportProgress :: UTCTime -> Int -> Rational -> IO ()
    reportProgress start iteration approximation = do
      now <- getCurrentTime
      let elapsed :: Double
          elapsed = realToFrac (diffUTCTime now start)
          preview = formatRationalDecimal approximation previewDigits
          timeStamp = showFFloat (Just 3) elapsed "s"
      putStrLn $ "Iteration " ++ show iteration ++ ": " ++ preview ++ " (" ++ timeStamp ++ ")"

    iterateSeries :: UTCTime -> Int -> Rational -> Rational -> Rational -> Rational -> Rational -> IO Rational
    iterateSeries start iteration factorialRatio linearTerm exponentialTerm seriesSum piEstimate
      | iteration >= iterations = pure piEstimate
      | otherwise = do
          let k :: Integer
              k = toInteger iteration
              numerators = product [6 * k - 5 .. 6 * k]
              denominators = product [3 * k - 2 .. 3 * k] * k * k * k
              factorialRatio' = factorialRatio * fromIntegral numerators / fromIntegral denominators
              linearTerm' = linearTerm + factorialMultiplier
              exponentialTerm' = exponentialTerm / exponentialDivisor
              currentTerm = factorialRatio' * linearTerm' * exponentialTerm'
              seriesSum' = if even iteration then seriesSum + currentTerm else seriesSum - currentTerm
              piEstimate' = constantMultiplier / seriesSum'
          when showProgress $ reportProgress start iteration piEstimate'
          iterateSeries start (iteration + 1) factorialRatio' linearTerm' exponentialTerm' seriesSum' piEstimate'

-- | Render a rational number as a decimal string with rounding.
formatRationalDecimal :: Rational -> Int -> String
formatRationalDecimal value digits
  | digits <= 0 = show (round value :: Integer)
  | otherwise =
      let scale = 10 ^ digits
          scaled :: Integer
          scaled = round (value * fromIntegral scale)
          (intPart, fracPartRaw) = quotRem (abs scaled) scale
          fracDigits = show fracPartRaw
          paddedFrac = replicate (digits - length fracDigits) '0' ++ fracDigits
          sign = if scaled < 0 then "-" else ""
       in sign ++ show intPart ++ "." ++ paddedFrac

-- | Built-in reference digits for verification (100 decimal digits).
defaultPiReference :: String
defaultPiReference =
  "3.1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679"

-- | Verify computed digits against a reference string.
runVerification :: Options -> String -> IO ()
runVerification options computed
  | Just digitsToCheck <- optVerifyDigits options
  , digitsToCheck > 0 = do
      reference <- maybe (pure defaultPiReference) loadReference (optVerifyFile options)
      let maxAvailable = min (length reference) (length computed)
          required = min maxAvailable (digitsToCheck + 2)
          prefixComputed = take required computed
          prefixReference = take required reference
          matching = length (takeWhile (uncurry (==)) (zip prefixComputed prefixReference))
          decimalMatches = max 0 (matching - 2)
          expected = min digitsToCheck (max 0 (required - 2))
      if decimalMatches >= expected
        then putStrLn $ "Verification successful: matched " ++ show decimalMatches ++ " digits."
        else do
          hPutStrLn stderr $ "Verification failed: expected " ++ show digitsToCheck ++
            " digits but only matched " ++ show decimalMatches ++ "."
          exitFailure
  | otherwise = pure ()
  where
    loadReference :: FilePath -> IO String
    loadReference path = do
      contents <- readFile path
      case find looksNumeric (map sanitize (lines contents)) of
        Just digitsLine | not (null digitsLine) -> pure digitsLine
        _ -> do
          hPutStrLn stderr $ "Could not locate reference digits in " ++ path
          exitFailure

    looksNumeric :: String -> Bool
    looksNumeric str = '.' `elem` str && all (\c -> isDigit c || c == '.') str

    sanitize :: String -> String
    sanitize = filter (\c -> isDigit c || c == '.')

main :: IO ()
main = do
  rawArgs <- getArgs
  when (any (`elem` ["--help", "-h"]) rawArgs) $ do
    putStr usageText
    exitSuccess

  let args = filter (`notElem` ["--help", "-h"]) rawArgs
  options <- case parseArgs args of
    Left err -> do
      hPutStrLn stderr err
      hPutStrLn stderr usageText
      exitFailure
    Right opts -> pure opts

  let iterations = optIterations options
      requestedPrecision = fromMaybe (estimatePrecision iterations) (optPrecision options)
      verifyRequirement = fromMaybe 0 (optVerifyDigits options)
      finalPrecision = max requestedPrecision verifyRequirement
      workingDigits = finalPrecision + precisionGuard
      previewDigits = max 5 (min 20 finalPrecision)

  result <- computePi iterations workingDigits (optProgress options) previewDigits
  let output = formatRationalDecimal result finalPrecision
  putStrLn output
  runVerification options output
