{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad (when)
import Data.List (intercalate, isPrefixOf, zip4)
import Data.Maybe (fromMaybe)
import System.Environment (getArgs)
import System.Exit (die)
import System.IO (IOMode (WriteMode), hPutStrLn, withFile)
import System.Random (mkStdGen, randomRs)
import Text.Printf (printf)

-------------------------------------------------------------------------------
-- Option parsing
-------------------------------------------------------------------------------

data Options = Options
  { optXs :: Maybe [Double]
  , optYs :: Maybe [Double]
  , optPoints :: Maybe Int
  , optSlope :: Double
  , optIntercept :: Double
  , optNoise :: Maybe Double
  , optSeed :: Maybe Int
  , optJson :: Maybe FilePath
  , optCsv :: Maybe FilePath
  , optPlotData :: Maybe FilePath
  , optPlotScript :: Maybe FilePath
  }
  deriving (Show)

defaultOptions :: Options
defaultOptions =
  Options
    { optXs = Nothing
    , optYs = Nothing
    , optPoints = Nothing
    , optSlope = 2.0
    , optIntercept = 1.0
    , optNoise = Nothing
    , optSeed = Nothing
    , optJson = Nothing
    , optCsv = Nothing
    , optPlotData = Nothing
    , optPlotScript = Nothing
    }

parseArgs :: [String] -> Either String Options
parseArgs = go defaultOptions
  where
    go opts [] = Right opts
    go _ (flag : []) | flag `elem` ["--x", "--y", "--points", "--slope", "--intercept", "--noise", "--seed", "--json", "--csv", "--plot-data", "--plot-script"] =
      Left ("missing value for " ++ flag)
    go opts ("--x" : rest) =
      let (vals, rest') = span (not . isFlag) rest
       in if null vals
            then Left "--x requires at least one value"
            else case traverse parseDouble vals of
              Left err -> Left ("invalid --x value: " ++ err)
              Right xs -> go (opts {optXs = Just xs}) rest'
    go opts ("--y" : rest) =
      let (vals, rest') = span (not . isFlag) rest
       in if null vals
            then Left "--y requires at least one value"
            else case traverse parseDouble vals of
              Left err -> Left ("invalid --y value: " ++ err)
              Right ys -> go (opts {optYs = Just ys}) rest'
    go opts ("--points" : n : rest) =
      case parseInt n of
        Left err -> Left ("invalid --points value: " ++ err)
        Right k | k <= 1 -> Left "--points must be greater than 1"
        Right k -> go (opts {optPoints = Just k}) rest
    go opts ("--slope" : m : rest) =
      case parseDouble m of
        Left err -> Left ("invalid --slope value: " ++ err)
        Right val -> go (opts {optSlope = val}) rest
    go opts ("--intercept" : b : rest) =
      case parseDouble b of
        Left err -> Left ("invalid --intercept value: " ++ err)
        Right val -> go (opts {optIntercept = val}) rest
    go opts ("--noise" : n : rest) =
      case parseDouble n of
        Left err -> Left ("invalid --noise value: " ++ err)
        Right val | val < 0 -> Left "--noise must be non-negative"
        Right val -> go (opts {optNoise = Just val}) rest
    go opts ("--seed" : s : rest) =
      case parseInt s of
        Left err -> Left ("invalid --seed value: " ++ err)
        Right val -> go (opts {optSeed = Just val}) rest
    go opts ("--json" : fp : rest) = go (opts {optJson = Just fp}) rest
    go opts ("--csv" : fp : rest) = go (opts {optCsv = Just fp}) rest
    go opts ("--plot-data" : fp : rest) = go (opts {optPlotData = Just fp}) rest
    go opts ("--plot-script" : fp : rest) = go (opts {optPlotScript = Just fp}) rest
    go _ (flag : _) = Left ("unrecognized option: " ++ flag)

    isFlag s = "--" `isPrefixOf` s

parseDouble :: String -> Either String Double
parseDouble s =
  case reads s of
    [(n, "")] -> Right n
    _ -> Left s

parseInt :: String -> Either String Int
parseInt s =
  case reads s of
    [(n, "")] -> Right n
    _ -> Left s

-------------------------------------------------------------------------------
-- Linear regression core
-------------------------------------------------------------------------------

mean :: [Double] -> Double
mean xs = sum xs / fromIntegral (length xs)

variance :: [Double] -> Double
variance xs =
  let m = mean xs
   in sum (map (\x -> (x - m) ^ (2 :: Int)) xs)

covariance :: [Double] -> [Double] -> Double
covariance xs ys =
  let mx = mean xs
      my = mean ys
   in sum (zipWith (\x y -> (x - mx) * (y - my)) xs ys)

data FitSummary = FitSummary
  { fitSlope :: Double
  , fitIntercept :: Double
  , fitResiduals :: [Double]
  , fitPredicted :: [Double]
  , fitRss :: Double
  , fitR2 :: Double
  }

fitLine :: [Double] -> [Double] -> Either String FitSummary
fitLine xs ys
  | length xs /= length ys = Left "x and y arrays must have the same length"
  | length xs < 2 = Left "at least two points are required"
  | varX == 0 = Left "cannot fit a vertical line (all x values are identical)"
  | otherwise =
      let slope = covXY / varX
          intercept = meanY - slope * meanX
          predicted = map (\x -> slope * x + intercept) xs
          residuals = zipWith (-) ys predicted
          rss = sum (map (^ (2 :: Int)) residuals)
          tss = sum (map (\y -> (y - meanY) ^ (2 :: Int)) ys)
          r2 = if tss == 0 then 1 else 1 - rss / tss
       in Right FitSummary{fitSlope = slope, fitIntercept = intercept, fitResiduals = residuals, fitPredicted = predicted, fitRss = rss, fitR2 = r2}
  where
    meanX = mean xs
    meanY = mean ys
    varX = variance xs
    covXY = covariance xs ys

-------------------------------------------------------------------------------
-- Synthetic data utilities
-------------------------------------------------------------------------------

dataSource :: Options -> String
dataSource Options{..}
  | hasManual = "manual"
  | otherwise = "synthetic"
  where
    hasManual = maybe False (not . null) optXs && maybe False (not . null) optYs

generateSynthetic :: Options -> Either String ([Double], [Double])
generateSynthetic Options{..} = do
  let n = fromMaybe 25 optPoints
      slope = optSlope
      intercept = optIntercept
      noiseAmp = fromMaybe 0 optNoise
      seed = fromMaybe 12345 optSeed
  when (n < 2) $ Left "synthetic dataset requires at least two points"
  let xs = map fromIntegral [0 .. n - 1]
      noiseStream = take n (randomRs (-noiseAmp, noiseAmp) (mkStdGen seed))
      ys = zipWith (\x e -> slope * x + intercept + e) xs noiseStream
  pure (xs, ys)

-------------------------------------------------------------------------------
-- Rendering helpers
-------------------------------------------------------------------------------

printSummary :: String -> [Double] -> [Double] -> FitSummary -> IO ()
printSummary source xs ys FitSummary{..} = do
  putStrLn "Ordinary Least Squares (OLS)"
  putStrLn "============================="
  putStrLn $ "Source: " ++ source
  putStrLn $ "Samples: " ++ show (length xs)
  putStrLn $ printf "Slope: %.6f" fitSlope
  putStrLn $ printf "Intercept: %.6f" fitIntercept
  putStrLn $ printf "Residual sum of squares: %.6f" fitRss
  putStrLn $ printf "R^2: %.6f" fitR2
  putStrLn ""
  putStrLn "x\ty\tfitted\tresidual"
  mapM_ (putStrLn . formatRow) (zip4 xs ys fitPredicted fitResiduals)
  where
    formatRow (x, y, yhat, r) = printf "%.6f\t%.6f\t%.6f\t%.6f" x y yhat r

writeJson :: FilePath -> String -> [Double] -> [Double] -> FitSummary -> IO ()
writeJson fp source xs ys FitSummary{..} =
  withFile fp WriteMode $ \h -> do
    hPutStrLn h "{"
    hPutStrLn h $ "  \"input\": {\"source\": \"" ++ source ++ "\", \"count\": " ++ show (length xs) ++ "},"
    hPutStrLn h $ "  \"coefficients\": {\"slope\": " ++ show fitSlope ++ ", \"intercept\": " ++ show fitIntercept ++ "},"
    hPutStrLn h $ "  \"statistics\": {\"rss\": " ++ show fitRss ++ ", \"r_squared\": " ++ show fitR2 ++ "},"
    hPutStrLn h "  \"samples\": ["
    let rows = zip4 xs ys fitPredicted fitResiduals
        encodeRow (x, y, yhat, r) =
          "    {" ++ intercalate ", "
            [ "\"x\": " ++ show x
            , "\"y\": " ++ show y
            , "\"fitted\": " ++ show yhat
            , "\"residual\": " ++ show r
            ] ++ "}"
    mapM_ (hPutStrLn h) (commaSeparate (map encodeRow rows))
    hPutStrLn h "  ]"
    hPutStrLn h "}"

writeCsv :: FilePath -> [Double] -> [Double] -> FitSummary -> IO ()
writeCsv fp xs ys FitSummary{..} =
  withFile fp WriteMode $ \h -> do
    hPutStrLn h "x,y,fitted,residual"
    let rows = zip4 xs ys fitPredicted fitResiduals
        encode (x, y, yhat, r) = intercalate "," (map show [x, y, yhat, r])
    mapM_ (hPutStrLn h) (map encode rows)

writePlotData :: FilePath -> [Double] -> [Double] -> FitSummary -> IO ()
writePlotData fp xs ys FitSummary{..} =
  withFile fp WriteMode $ \h -> do
    hPutStrLn h "# Original data"
    mapM_ (\(x, y) -> hPutStrLn h (printf "%.6f %.6f" x y)) (zip xs ys)
    hPutStrLn h ""
    hPutStrLn h "# Fitted line"
    mapM_ (\(x, yhat) -> hPutStrLn h (printf "%.6f %.6f" x yhat)) (zip xs fitPredicted)
    hPutStrLn h ""
    hPutStrLn h "# Plot with gnuplot, e.g.:"
    let cmd = "#   gnuplot -e \"set datafile commentschars '#'; plot '" ++ fp ++ "' index 0 using 1:2 with points title 'Samples', '' index 1 using 1:2 with lines title 'Fitted'\""
    hPutStrLn h cmd

writePlotScript :: FilePath -> FilePath -> IO ()
writePlotScript fp dataFile =
  withFile fp WriteMode $ \h -> do
    hPutStrLn h "# Gnuplot script generated by LSF.hs"
    hPutStrLn h "set title 'Least Squares Fit'"
    hPutStrLn h "set key left top"
    hPutStrLn h "set xlabel 'x'"
    hPutStrLn h "set ylabel 'y'"
    hPutStrLn h "set datafile commentschars '#'"
    hPutStrLn h "plot \\\"
    hPutStrLn h $ "  '" ++ dataFile ++ "' index 0 using 1:2 with points title 'Samples', \\\"
    hPutStrLn h $ "  '' index 1 using 1:2 with lines title 'Fitted'"

commaSeparate :: [String] -> [String]
commaSeparate [] = []
commaSeparate [x] = [x]
commaSeparate (x : xs) = (x ++ ",") : commaSeparate xs

-------------------------------------------------------------------------------
-- Main entry point
-------------------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  opts <- case parseArgs args of
    Left err -> die err
    Right parsed -> pure parsed

  (xs, ys) <-
    case (optXs opts, optYs opts) of
      (Just xsVals, Just ysVals) -> pure (xsVals, ysVals)
      (Nothing, Nothing) ->
        case generateSynthetic opts of
          Left err -> die err
          Right dataset -> pure dataset
      (Just _, Nothing) -> die "--x provided without --y"
      (Nothing, Just _) -> die "--y provided without --x"

  summary <- case fitLine xs ys of
    Left err -> die err
    Right s -> pure s

  let source = dataSource opts
  printSummary source xs ys summary

  maybe (pure ()) (\fp -> writeJson fp source xs ys summary) (optJson opts)
  maybe (pure ()) (\fp -> writeCsv fp xs ys summary) (optCsv opts)
  maybe (pure ()) (\fp -> writePlotData fp xs ys summary) (optPlotData opts)

  case (optPlotScript opts, optPlotData opts) of
    (Just script, Just dataFile) -> writePlotScript script dataFile
    (Just _, Nothing) ->
      putStrLn "Warning: --plot-script requires --plot-data to specify the dataset file."
    _ -> pure ()

