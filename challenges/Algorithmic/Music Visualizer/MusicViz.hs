{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import           Control.Monad               (when)
import qualified Data.Aeson                  as Aeson
import qualified Data.ByteString.Lazy        as BL
import qualified Data.ByteString.Lazy.Char8  as BL8
import           Data.Complex                (Complex(..))
import qualified Data.Vector.Storable        as VS
import           Numeric.FFT                 (fft)
import           Options.Applicative
import           System.Directory            (getTemporaryDirectory, removeFile)
import           System.IO                   (hPutStrLn, stderr)
import           System.Process              (callCommand)
import qualified Codec.Picture               as JP

import qualified Data.Text                   as T
import qualified Data.Text.IO                as TIO

--------------------------------------------------------------------------------
-- CLI configuration
--------------------------------------------------------------------------------

data Config = Config
  { cfgFile        :: !(Maybe FilePath)
  , cfgSynthetic   :: !Bool
  , cfgFrequency   :: !Double
  , cfgDuration    :: !Double
  , cfgSampleRate  :: !Int
  , cfgWindowSize  :: !Int
  , cfgHopSize     :: !Int
  , cfgUseMel      :: !Bool
  , cfgMelBands    :: !Int
  , cfgHeadless    :: !Bool
  , cfgSavePath    :: !(Maybe FilePath)
  , cfgJsonPath    :: !(Maybe FilePath)
  }
  deriving (Show)

configParser :: Parser Config
configParser = Config
  <$> optional (strOption (long "file" <> short 'f' <> metavar "PATH" <> help "Audio file to load (any format supported by sox)"))
  <*> switch (long "synthetic" <> help "Generate a synthetic sine tone instead of loading a file")
  <*> option auto (long "freq" <> metavar "HZ" <> help "Frequency for the synthetic tone" <> value 440 <> showDefault)
  <*> option auto (long "duration" <> metavar "SECONDS" <> help "Duration for the synthetic tone" <> value 2.0 <> showDefault)
  <*> option auto (long "rate" <> metavar "HZ" <> help "Sample rate for synthetic tone or when resampling" <> value 22050 <> showDefault)
  <*> option auto (long "window" <> metavar "SAMPLES" <> help "STFT window size" <> value 1024 <> showDefault)
  <*> option auto (long "hop" <> metavar "SAMPLES" <> help "STFT hop length" <> value 256 <> showDefault)
  <*> switch (long "mel" <> help "Apply mel filter bank to generate mel-spectrogram")
  <*> option auto (long "mel-bands" <> metavar "N" <> help "Number of mel bands" <> value 64 <> showDefault)
  <*> switch (long "headless" <> help "Disable desktop preview and only save the image")
  <*> optional (strOption (long "save" <> metavar "PNG" <> help "Write the visualization to this PNG file"))
  <*> optional (strOption (long "json" <> metavar "PATH" <> help "Write JSON metadata to the provided path"))

--------------------------------------------------------------------------------
-- Audio loading
--------------------------------------------------------------------------------

data AudioBuffer = AudioBuffer
  { abSamples    :: !(VS.Vector Double)
  , abSampleRate :: !Int
  }

loadAudio :: Config -> IO AudioBuffer
loadAudio Config{..}
  | cfgSynthetic = pure $ synthesizeTone cfgFrequency cfgDuration cfgSampleRate
  | Just path <- cfgFile = loadViaSox path cfgSampleRate
  | otherwise = fail "Either provide --file or --synthetic"

loadViaSox :: FilePath -> Int -> IO AudioBuffer
loadViaSox path targetRate = do
  tmpDir <- getTemporaryDirectory
  let tmpDat = tmpDir <> "/musicviz_tmp.dat"
      cmd = "sox \"" <> path <> "\" -r " <> show targetRate <> " -c 1 -t dat \"" <> tmpDat <> "\""
  callCommand cmd
  samples <- parseDat tmpDat
  removeFile tmpDat
  pure $ AudioBuffer (VS.fromList samples) targetRate

parseDat :: FilePath -> IO [Double]
parseDat path = do
  contents <- TIO.readFile path
  let ls = filter (not . T.isPrefixOf ";") (T.lines contents)
      parsed = [ readDouble amp | line <- ls, let ws = T.words line, length ws >= 2, let amp = ws !! 1 ]
  pure parsed
  where
    readDouble txt = case reads (T.unpack txt) of
      (x,_):_ -> x
      _       -> 0

--------------------------------------------------------------------------------
-- Synthetic tone generation
--------------------------------------------------------------------------------

synthesizeTone :: Double -> Double -> Int -> AudioBuffer
synthesizeTone freq duration rate =
  let sampleCount = floor (duration * fromIntegral rate)
      samples = VS.generate sampleCount $ \i ->
        let t = fromIntegral i / fromIntegral rate
        in sin (2 * pi * freq * t)
  in AudioBuffer samples rate

--------------------------------------------------------------------------------
-- STFT and spectral features
--------------------------------------------------------------------------------

stft :: Int -> Int -> VS.Vector Double -> [[Double]]
stft windowSize hopSize samples
  | windowSize <= 0 = []
  | VS.null samples = []
  | otherwise = go 0
  where
    len = VS.length samples
    windowVec = hamming windowSize
    go idx
      | idx + windowSize > len = []
      | otherwise =
          let frame = VS.slice idx windowSize samples
              windowed = VS.zipWith (*) windowVec frame
              complexFrame = map (:+ 0) (VS.toList windowed)
              spectrum = fft complexFrame
              magnitudes = map (logEnergy . magnitude) spectrum
          in magnitudes : go (idx + hopSize)
    magnitude (x :+ y) = sqrt (x * x + y * y)
    logEnergy x = logBase 10 (1e-12 + x)

hamming :: Int -> VS.Vector Double
hamming n
  | n <= 1    = VS.singleton 1
  | otherwise = VS.generate n $ \i -> 0.54 - 0.46 * cos (2 * pi * fromIntegral i / fromIntegral (n - 1))

applyMel :: Int -> Int -> [[Double]] -> [[Double]]
applyMel sampleRate melBands spectrogram
  | null spectrogram = []
  | otherwise =
      let fftSize = length (head spectrogram)
          bank = melFilterBank melBands (fromIntegral sampleRate) fftSize
      in map (applyBank bank) spectrogram
  where
    applyBank bank frame =
      [ sum [ w * binAt k | (k, w) <- row ] | row <- bank ]
      where
        frameLen = length frame
        binAt k
          | k < 0 || k >= frameLen = 0
          | otherwise = frame !! k

melFilterBank :: Int -> Double -> Int -> [[(Int, Double)]]
melFilterBank bands sampleRate fftSize =
  let melMin = hzToMel 0
      melMax = hzToMel (sampleRate / 2)
      melPoints = [melMin + (melMax - melMin) * fromIntegral i / fromIntegral (bands + 2) | i <- [0 .. bands + 1]]
      hzPoints = map melToHz melPoints
      binFreq hz = floor (fromIntegral fftSize * hz / sampleRate)
      bins = map binFreq hzPoints
  in [ [ (k, tri i k) | k <- [bins !! i .. bins !! (i + 2)]]
     | i <- [0 .. bands - 1]
     ]
  where
    tri i k
      | k < bins !! i = 0
      | k == bins !! i = 0
      | k <= bins !! (i + 1) = (fromIntegral k - fromIntegral (bins !! i)) / fromIntegral (bins !! (i + 1) - bins !! i)
      | k <= bins !! (i + 2) = (fromIntegral (bins !! (i + 2)) - fromIntegral k) / fromIntegral (bins !! (i + 2) - bins !! (i + 1))
      | otherwise = 0

hzToMel, melToHz :: Double -> Double
hzToMel hz = 2595 * logBase 10 (1 + hz / 700)
melToHz mel = 700 * (10 ** (mel / 2595) - 1)

--------------------------------------------------------------------------------
-- Visualization
--------------------------------------------------------------------------------

data Visualization = Visualization
  { visImage    :: !JP.Image JP.PixelRGB8
  , visMetadata :: !Aeson.Value
  }

renderSpectrogram :: Config -> AudioBuffer -> [[Double]] -> Visualization
renderSpectrogram Config{..} AudioBuffer{..} frames =
  let matrix = if cfgUseMel then applyMel abSampleRate cfgMelBands frames else frames
      normalized = normalize2D matrix
      (width, safeHeight, safeMatrix) =
        if null normalized
          then (1, 1, [[0]])
          else (length normalized, max 1 (length (head normalized)), normalized)
      image = JP.generateImage (pixelAt safeMatrix) width safeHeight
      metadata = Aeson.object
        [ "sample_rate" .= abSampleRate
        , "frame_count" .= width
        , "bin_count" .= safeHeight
        , "window_size" .= cfgWindowSize
        , "hop_size" .= cfgHopSize
        , "mel" .= cfgUseMel
        , "mel_bands" .= (if cfgUseMel then Just cfgMelBands else Nothing)
        ]
  in Visualization image metadata
  where
    pixelAt matrix x y =
      let column = matrix !! x
          value = if y < length column then column !! y else 0
      in colorMap value

colorMap :: Double -> JP.PixelRGB8
colorMap t =
  let clamped = clamp01 t
      r = floor (255 * clamp01 (1.8 * clamped))
      g = floor (255 * clamp01 (sin (pi * clamped)))
      b = floor (255 * clamp01 (1 - clamped))
  in JP.PixelRGB8 r g b
  where
    clamp01 x = max 0 (min 1 x)

normalize2D :: [[Double]] -> [[Double]]
normalize2D rows
  | null rows = []
  | otherwise =
      let allVals = concat rows
          mn = minimum allVals
          mx = maximum allVals
          range = if mx - mn < 1e-9 then 1 else mx - mn
      in map (map (\v -> (v - mn) / range)) rows

--------------------------------------------------------------------------------
-- JSON metadata helpers
--------------------------------------------------------------------------------

writeMetadata :: Config -> Aeson.Value -> IO ()
writeMetadata Config{..} value =
  case cfgJsonPath of
    Nothing         -> BL8.putStrLn (Aeson.encode value)
    Just "-"        -> BL8.putStrLn (Aeson.encode value)
    Just path       -> BL.writeFile path (Aeson.encode value)

--------------------------------------------------------------------------------
-- Main program
--------------------------------------------------------------------------------

main :: IO ()
main = do
  cfg <- execParser opts
  buffer <- loadAudio cfg
  let frames = stft (cfgWindowSize cfg) (cfgHopSize cfg) (abSamples buffer)
      Visualization img meta = renderSpectrogram cfg buffer frames
  case cfgSavePath cfg of
    Just path -> JP.savePngImage path (JP.ImageRGB8 img)
    Nothing   -> when (cfgHeadless cfg) $ hPutStrLn stderr "--headless set but no --save path provided"

  writeMetadata cfg meta

  when (not (cfgHeadless cfg)) $ do
    tempDir <- getTemporaryDirectory
    let tmpFile = tempDir <> "/musicviz_preview.png"
    JP.savePngImage tmpFile (JP.ImageRGB8 img)
    callCommand $ "xdg-open \"" <> tmpFile <> "\""

  where
    opts = info (configParser <**> helper)
      ( fullDesc
     <> progDesc "Generate STFT or mel-spectrogram visualizations (Haskell edition)"
     <> header "MusicViz.hs - music visualizer"
      )

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

clamp01 :: Double -> Double
clamp01 x = max 0 (min 1 x)
