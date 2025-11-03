{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Ulam
-- Description : Command-line Ulam spiral generator with JSON metadata and optional PNG export.
--
-- The implementation mirrors the features of the Python reference script (ulam.py),
-- exposing CLI flags for spiral size, JSON metadata output, and optional image export.
-- Rendering is performed via JuicyPixels so the program can run headlessly.
module Main (main) where

import Codec.Picture (Image (..), Pixel8, writePng)
import Control.Monad (when)
import Control.Monad.ST (ST, runST)
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Options.Applicative
import System.Exit (exitFailure, exitSuccess)
import Text.Printf (printf)

-- | Runtime configuration parsed from the command line.
data Config = Config
  { cfgSize :: !Int
  , cfgJSON :: !Bool
  , cfgSave :: !(Maybe FilePath)
  }
  deriving (Show, Eq)

-- | Parser for CLI options, matching the ergonomics of the Python reference.
configParser :: Parser Config
configParser =
  Config
    <$> option
      auto
      ( long "size"
          <> short 's'
          <> metavar "N"
          <> help "Edge length of the spiral (odd values centre the pattern)"
          <> value 101
          <> showDefault
      )
    <*> switch
      ( long "json"
          <> help "Emit JSON metadata describing the generated spiral"
      )
    <*> optional
      ( strOption
          ( long "save"
              <> metavar "PATH"
              <> help "Write the spiral as a PNG image to PATH"
          )
      )

-- | Command-line entry point.
main :: IO ()
main = do
  cfg <- execParser opts
  case validateConfig cfg of
    Just err -> putStrLn ("Error: " <> err) >> exitFailure
    Nothing -> do
      let size = cfgSize cfg
          limit = size * size
          (image, primeCount) = generateUlamImage size
          density = if limit <= 0 then 0 else fromIntegral primeCount / fromIntegral limit :: Double
          saved = maybe False (const True) (cfgSave cfg)

      when (cfgJSON cfg) $ do
        let payload =
              Aeson.object
                [ "size" .= size
                , "limit" .= limit
                , "prime_count" .= primeCount
                , "prime_density" .= density
                , "saved" .= saved
                ]
        BL.putStrLn (Aeson.encode payload)

      case cfgSave cfg of
        Just path -> do
          writePng path image
          putStrLn ("Saved spiral to " <> path)
        Nothing -> pure ()

      when (not (cfgJSON cfg)) $ do
        putStrLn (printf "Generated %dx%d spiral with %d primes (density %.4f)" size size primeCount density)

      exitSuccess
  where
    opts =
      info
        (configParser <**> helper)
        ( fullDesc
            <> progDesc "Generate an Ulam spiral with optional metadata and PNG export"
            <> header "ulam - prime spiral visualisation"
        )

-- | Basic validation for configuration values.
validateConfig :: Config -> Maybe String
validateConfig cfg
  | cfgSize cfg < 1 = Just "size must be a positive integer"
  | otherwise = Nothing

-- | Build an image representing the Ulam spiral, returning both the image and the prime count.
-- Pixels containing primes are encoded as white (255) while non-primes are black (0).
generateUlamImage :: Int -> (Image Pixel8, Int)
generateUlamImage size = runST $ do
  pixels <- VSM.replicate totalPixels 0
  primeVec <- sieve (size * size)
  primeCount <- walkSpiral primeVec pixels
  frozen <- VS.unsafeFreeze pixels
  pure (Image size size frozen, primeCount)
  where
    totalPixels = size * size
    centre = size `div` 2

    walkSpiral :: VU.Vector Bool -> VSM.MVector s Pixel8 -> ST s Int
    walkSpiral primes pixels = go 1 centre centre 0 1 0 0 0
      where
        limit = size * size
        directions = [(1, 0), (0, -1), (-1, 0), (0, 1)] :: [(Int, Int)]

        go :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> ST s Int
        go n x y dirIndex stepLength stepsTaken directionChanges primeCount
          | n > limit = pure primeCount
          | otherwise = do
              let primeHere = n <= limit && primes VU.! n
              primeCount' <-
                if primeHere && inBounds x y
                  then do
                    VSM.write pixels (indexOf x y) 255
                    pure (primeCount + 1)
                  else pure primeCount

              if n == limit
                then pure primeCount'
                else do
                  let (dx, dy) = directions !! dirIndex
                      x' = x + dx
                      y' = y + dy
                      stepsTaken' = stepsTaken + 1
                  let (dirIndex', stepLength', stepsTaken'', directionChanges') =
                        if stepsTaken' == stepLength
                          then
                            let dirIndexNext = (dirIndex + 1) `mod` 4
                                directionChangesNext = directionChanges + 1
                                stepLengthNext =
                                  if directionChangesNext `mod` 2 == 0
                                    then stepLength + 1
                                    else stepLength
                             in (dirIndexNext, stepLengthNext, 0, directionChangesNext)
                          else (dirIndex, stepLength, stepsTaken', directionChanges)
                  go (n + 1) x' y' dirIndex' stepLength' stepsTaken'' directionChanges' primeCount'

        indexOf :: Int -> Int -> Int
        indexOf x y = y * size + x

        inBounds :: Int -> Int -> Bool
        inBounds x y = x >= 0 && x < size && y >= 0 && y < size

-- | Sieve of Eratosthenes producing a boolean vector of primality up to the given limit (inclusive).
sieve :: Int -> ST s (VU.Vector Bool)
sieve limit = do
  vec <- VUM.replicate (limit + 1) True
  when (limit >= 0) $ VUM.write vec 0 False
  when (limit >= 1) $ VUM.write vec 1 False
  let root = integerSqrt limit
  let loop p
        | p > root = pure ()
        | otherwise = do
            isPrime <- VUM.read vec p
            when isPrime $ do
              let start = p * p
              let goComposite k
                    | k > limit = pure ()
                    | otherwise = VUM.write vec k False >> goComposite (k + p)
              goComposite start
            loop (p + 1)
  loop 2
  VU.unsafeFreeze vec

-- | Integer square root (floor).
integerSqrt :: Int -> Int
integerSqrt n = floor (sqrt (fromIntegral (max 0 n :: Int)))
