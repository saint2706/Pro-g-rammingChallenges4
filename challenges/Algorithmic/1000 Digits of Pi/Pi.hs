{-# LANGUAGE RecordWildCards #-}
{-|
Module      : Main
Description : High-precision Pi calculator using the Gauss–Legendre algorithm.

This standalone executable mirrors the Python workflow in @pi.py@ but is
implemented entirely in Haskell.  It exposes a command line interface for
selecting the number of digits to compute as well as an optional verbose mode
that streams iteration details to standard error.  The Gauss–Legendre
iteration is evaluated with arbitrary-precision rational arithmetic and a
Newton–Raphson square-root routine so results can scale to thousands of digits
without losing accuracy.
-}
module Main (main) where

import Data.Char (isDigit)
import Data.List (intercalate)
import Data.Ratio ((%), Rational)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import System.Environment (getArgs)
import System.Exit (die)
import System.IO (hPutStrLn, stderr)
import Text.Printf (printf)

-- | Command line options recognised by the executable.
data Options = Options
    { optDigits  :: !Int
    , optVerbose :: !Bool
    }
    deriving (Show)

-- | Represents a single iteration of the Gauss–Legendre algorithm.
data Iteration = Iteration
    { iterIndex   :: !Int
    , iterA       :: !Rational
    , iterB       :: !Rational
    , iterApprox  :: !Rational
    }

-- | Parse the command line arguments, accepting either a positional digit
-- value or the @--digits/-n@ flag alongside an optional @--verbose/-v@ flag.
parseOptions :: IO Options
parseOptions = do
    args <- getArgs
    case parse args Nothing False of
        Just opts -> pure opts
        Nothing   ->
            die $ unlines
                [ "Usage: Pi [--digits N] [--verbose]"
                , "       Pi N"
                , ""
                , "Calculate Pi to N decimal places using the Gauss–Legendre algorithm."
                ]
  where
    parse :: [String] -> Maybe Int -> Bool -> Maybe Options
    parse [] (Just n) verbose =
        if n > 0 then Just (Options n verbose) else Nothing
    parse [] Nothing verbose  = Just (Options defaultDigits verbose)
    parse (flag:rest) mDigits verbose
        | flag == "--verbose" || flag == "-v" = parse rest mDigits True
        | flag == "--digits" || flag == "-n" =
            case rest of
                (value:xs)
                    | all isDigit value -> parse xs (Just (read value)) verbose
                    | otherwise         -> Nothing
                _ -> Nothing
        | all isDigit flag && mDigits == Nothing = parse rest (Just (read flag)) verbose
        | otherwise = Nothing

    defaultDigits = 1000

-- | Run the Gauss–Legendre iteration until the @a@ and @b@ terms converge
-- within the requested precision.
gaussLegendre :: Int -> (Rational, [Iteration])
gaussLegendre digits = (finalApprox, iterations)
  where
    precisionDigits = digits + max 10 (digits `div` 10)
    epsilon = 1 % (10 ^ (precisionDigits + 2))

    initialA = 1 % 1
    initialB = recip (sqrtRational precisionDigits (2 % 1))
    initialT = 1 % 4
    initialP = 1 % 1

    initialApprox = piEstimate initialA initialB initialT
    initialIter = Iteration 0 initialA initialB initialApprox

    (_, _, _, finalApprox, iterations) = iterateGL 1 initialA initialB initialT initialP [initialIter]

    iterateGL idx a b t p acc
        | abs (a - b) <= epsilon || idx > maxIterations = (a, b, t, piEstimate a b t, reverse acc)
        | otherwise =
            let aNext = (a + b) / 2
                bNext = sqrtRational precisionDigits (a * b)
                tNext = t - p * (a - aNext) ^ (2 :: Int)
                pNext = 2 * p
                approxNext = piEstimate aNext bNext tNext
                nextIter = Iteration idx aNext bNext approxNext
            in iterateGL (idx + 1) aNext bNext tNext pNext (nextIter : acc)

    maxIterations = 50

    piEstimate a b t = (a + b) ^ (2 :: Int) / (4 * t)

-- | Newton–Raphson square root tailored for rationals.
sqrtRational :: Int -> Rational -> Rational
sqrtRational _ 0 = 0
sqrtRational precision value
    | value < 0 = error "sqrtRational: negative input"
    | otherwise = newton initialGuess 0
  where
    epsilon = 1 % (10 ^ (precision + 5))
    initialGuess = toRational (sqrt (fromRational value :: Double))

    newton guess count
        | count >= 100 = guess
        | abs (guess - nextGuess) <= epsilon = nextGuess
        | otherwise = newton nextGuess (count + 1)
      where
        nextGuess = (guess + value / guess) / 2

-- | Format a @Rational@ as a decimal string with the requested number of digits.
formatPi :: Int -> Rational -> String
formatPi digits value
    | digits <= 0 = show (round value :: Integer)
    | otherwise =
        let scale = 10 ^ digits
            scaled = value * fromInteger scale
            rounded = floor (scaled + (1 % 2))
            (intPart, fracPart) = rounded `divMod` scale
            integerDigits = show intPart
            fractionalDigits = padLeft digits '0' (show fracPart)
        in integerDigits ++ "." ++ fractionalDigits
  where
    padLeft width ch str = replicate (width - length str) ch ++ str

-- | Emit verbose iteration logs to standard error when requested.
logIterations :: Bool -> [Iteration] -> IO ()
logIterations False _ = pure ()
logIterations True steps = mapM_ (hPutStrLn stderr . render) steps
  where
    render Iteration{..} =
        let approxDouble = fromRational iterApprox :: Double
            diffAB = abs (iterA - iterB)
        in printf "Iteration %d: π≈%.15f | |a-b|≈%.3e" iterIndex approxDouble (fromRational diffAB :: Double)

-- | Application entry point.
main :: IO ()
main = do
    Options{..} <- parseOptions

    start <- getCurrentTime
    let (piApprox, history) = gaussLegendre optDigits
    end <- getCurrentTime

    logIterations optVerbose history

    let piString = formatPi optDigits piApprox
        elapsed = realToFrac (diffUTCTime end start) :: Double
        iterations = case history of
            [] -> 0
            _  -> length history - 1
        metadata =
            [ "digits=" ++ show optDigits
            , "iterations=" ++ show iterations
            , printf "elapsed_seconds=%.6f" elapsed
            ]

    putStrLn piString
    putStrLn (intercalate " " metadata)
