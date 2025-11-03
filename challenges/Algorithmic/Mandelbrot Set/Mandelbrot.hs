{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Codec.Picture (Image(..), PixelRGB8(..), generateImage, writePng)
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Vector as V
import Numeric (showFFloat)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)
import Text.Read (readMaybe)

data Config = Config
  { cfgWidth :: !Int
  , cfgHeight :: !Int
  , cfgMaxIter :: !Int
  , cfgBounds :: !(Maybe (Double, Double, Double, Double))
  , cfgCenter :: !(Maybe (Double, Double))
  , cfgScale :: !Double
  , cfgSmooth :: !Bool
  , cfgPaletteName :: !String
  , cfgSavePath :: !(Maybe FilePath)
  , cfgSummary :: !Bool
  }
  deriving (Show)

defaultConfig :: Config
defaultConfig =
  Config
    { cfgWidth = 800
    , cfgHeight = 600
    , cfgMaxIter = 500
    , cfgBounds = Nothing
    , cfgCenter = Nothing
    , cfgScale = 3.0
    , cfgSmooth = False
    , cfgPaletteName = "hot"
    , cfgSavePath = Nothing
    , cfgSummary = True
    }

usage :: String
usage =
  unlines
    [ "Mandelbrot.hs - Native Mandelbrot renderer comparable to mandel.py"
    , ""
    , "Usage: Mandelbrot [OPTIONS]"
    , ""
    , "Options:"
    , "  --width INT           Image width (default 800)"
    , "  --height INT          Image height (default 600)"
    , "  --max-iter INT        Maximum iterations (default 500)"
    , "  --bounds XMIN XMAX YMIN YMAX"
    , "                        Explicit window bounds"
    , "  --center CX CY        Centre point (used with --scale)"
    , "  --scale FLOAT         Real axis span when using --center (default 3.0)"
    , "  --smooth              Enable smooth colouring"
    , "  --cmap NAME           Colour palette (hot, plasma, viridis, turbo)"
    , "  --save PATH           Write PNG to PATH"
    , "  --no-summary          Suppress textual summary output"
    , "  --no-show             Accepted for parity; no effect (no GUI)"
    , "  --no-bar              Accepted for parity; no effect"
    , "  -h, --help            Show this help text"
    ]

parseArgs :: [String] -> Either String Config
parseArgs = go defaultConfig
  where
    go cfg [] = Right cfg
    go cfg ("--width" : v : rest) =
      case readMaybe v of
        Just w -> go cfg {cfgWidth = w} rest
        Nothing -> Left "Invalid integer for --width"
    go _ ("--width" : []) = Left "--width expects an integer"
    go cfg ("--height" : v : rest) =
      case readMaybe v of
        Just h -> go cfg {cfgHeight = h} rest
        Nothing -> Left "Invalid integer for --height"
    go _ ("--height" : []) = Left "--height expects an integer"
    go cfg ("--max-iter" : v : rest) =
      case readMaybe v of
        Just m -> go cfg {cfgMaxIter = m} rest
        Nothing -> Left "Invalid integer for --max-iter"
    go _ ("--max-iter" : []) = Left "--max-iter expects an integer"
    go cfg ("--bounds" : xmin : xmax : ymin : ymax : rest) =
      case (readMaybe xmin, readMaybe xmax, readMaybe ymin, readMaybe ymax) of
        (Just a, Just b, Just c, Just d) ->
          go cfg {cfgBounds = Just (a, b, c, d), cfgCenter = Nothing} rest
        _ -> Left "Invalid float supplied to --bounds"
    go _ ("--bounds" : _) = Left "--bounds expects four floats"
    go cfg ("--center" : cx : cy : rest) =
      case (readMaybe cx, readMaybe cy) of
        (Just a, Just b) ->
          go cfg {cfgCenter = Just (a, b), cfgBounds = Nothing} rest
        _ -> Left "Invalid float supplied to --center"
    go _ ("--center" : _) = Left "--center expects two floats"
    go cfg ("--scale" : v : rest) =
      case readMaybe v of
        Just s -> go cfg {cfgScale = s} rest
        Nothing -> Left "Invalid float for --scale"
    go _ ("--scale" : []) = Left "--scale expects a float"
    go cfg ("--smooth" : rest) = go cfg {cfgSmooth = True} rest
    go cfg ("--cmap" : name : rest) = go cfg {cfgPaletteName = name} rest
    go _ ("--cmap" : []) = Left "--cmap expects a palette name"
    go cfg ("--save" : path : rest) = go cfg {cfgSavePath = Just path} rest
    go _ ("--save" : []) = Left "--save expects a path"
    go cfg ("--no-summary" : rest) = go cfg {cfgSummary = False} rest
    go cfg ("--summary" : rest) = go cfg {cfgSummary = True} rest
    go cfg ("--no-show" : rest) = go cfg rest
    go cfg ("--no-bar" : rest) = go cfg rest
    go _ ("-h" : _) = Left "help"
    go _ ("--help" : _) = Left "help"
    go _ (unknown : _) = Left ("Unknown option: " ++ unknown)

validateConfig :: Config -> Either String Config
validateConfig cfg@Config {..}
  | cfgWidth <= 0 || cfgHeight <= 0 = Left "width/height must be positive"
  | cfgMaxIter <= 0 = Left "max-iter must be positive"
  | cfgScale <= 0 = Left "scale must be positive"
  | isJust cfgBounds && isJust cfgCenter = Left "Use either --bounds or --center"
  | otherwise = Right cfg

deriveBounds :: Config -> (Double, Double, Double, Double)
deriveBounds Config {..} =
  case cfgBounds of
    Just b -> b
    Nothing ->
      let (cx, cy) = fromMaybe (-0.75, 0.0) cfgCenter
          aspect = fromIntegral cfgHeight / fromIntegral cfgWidth
          halfW = cfgScale / 2.0
          halfH = halfW * aspect
       in (cx - halfW, cx + halfW, cy - halfH, cy + halfH)

data Sample = Sample
  { spEscaped :: !Bool
  , spValue :: !Double
  }

computeGrid :: Config -> V.Vector Sample
computeGrid cfg@Config {..} = V.generate (cfgWidth * cfgHeight) pixel
  where
    (xmin, xmax, ymin, ymax) = deriveBounds cfg
    dx = if cfgWidth <= 1 then 0 else (xmax - xmin) / fromIntegral (cfgWidth - 1)
    dy = if cfgHeight <= 1 then 0 else (ymax - ymin) / fromIntegral (cfgHeight - 1)
    pixel idx =
      let (y, x) = idx `divMod` cfgWidth
          cr = xmin + fromIntegral x * dx
          ci = ymin + fromIntegral y * dy
       in iteratePoint 0 0.0 0.0 cr ci
    iteratePoint iter zr zi cr ci
      | iter >= cfgMaxIter = Sample False (fromIntegral cfgMaxIter)
      | magnitude2 > 4.0 =
          if cfgSmooth
            then
              let magnitude = sqrt magnitude2
                  safeMag = max magnitude 1.0e-12
                  nu = fromIntegral iter + 1 - logBase 2 (log safeMag)
               in Sample True nu
            else Sample True (fromIntegral iter)
      | otherwise =
          let zr' = zr2 - zi2 + cr
              zi' = 2 * zr * zi + ci
           in iteratePoint (iter + 1) zr' zi' cr ci
      where
        zr2 = zr * zr
        zi2 = zi * zi
        magnitude2 = zr2 + zi2

normalise :: Config -> V.Vector Sample -> V.Vector Double
normalise Config {..} samples
  | not cfgSmooth = V.map toFrac samples
  | otherwise =
      case escapeMax samples of
        Nothing -> V.replicate (V.length samples) 1.0
        Just maxVal -> V.map (smoothVal maxVal) samples
  where
    toFrac Sample {..}
      | spEscaped = spValue / fromIntegral cfgMaxIter
      | otherwise = 1.0
    smoothVal maxVal Sample {..}
      | not spEscaped = 1.0
      | maxVal <= 0 = 1.0
      | otherwise = spValue / maxVal

escapeMax :: V.Vector Sample -> Maybe Double
escapeMax = V.foldl' step Nothing
  where
    step acc Sample {..}
      | not spEscaped = acc
      | otherwise = Just (maybe spValue (max spValue) acc)

type Palette = Double -> PixelRGB8

clamp01 :: Double -> Double
clamp01 = max 0 . min 1

lerp :: Double -> Double -> Double -> Double
lerp a b t = a + (b - a) * t

lerpColour :: Double -> (Double, Double, Double) -> (Double, Double, Double) -> (Double, Double, Double)
lerpColour t (r1, g1, b1) (r2, g2, b2) =
  ( lerp r1 r2 t
  , lerp g1 g2 t
  , lerp b1 b2 t
  )

mkGradient :: [(Double, (Double, Double, Double))] -> Palette
mkGradient stops x =
  let t = clamp01 x
      (lower, upper) = pickStops stops t
      spanT = if fst upper == fst lower then 0 else (t - fst lower) / (fst upper - fst lower)
      (r, g, b) = lerpColour spanT (snd lower) (snd upper)
   in PixelRGB8 (toWord8 r) (toWord8 g) (toWord8 b)
  where
    toWord8 = floor . (* 255) . clamp01
    pickStops (s1 : s2 : rest) value
      | value <= fst s2 = (s1, s2)
      | otherwise = pickStops (s2 : rest) value
    pickStops [s] _ = (s, s)
    pickStops [] _ = ((0, (0, 0, 0)), (1, (1, 1, 1)))

palettes :: [(String, Palette)]
palettes =
  [ ("hot", mkGradient hotStops)
  , ("plasma", mkGradient plasmaStops)
  , ("viridis", mkGradient viridisStops)
  , ("turbo", mkGradient turboStops)
  ]

lookupPalette :: String -> Palette
lookupPalette name = fromMaybe defaultPalette (lookup name palettes)
  where
    defaultPalette = mkGradient hotStops

hotStops, plasmaStops, viridisStops, turboStops :: [(Double, (Double, Double, Double))]
hotStops =
  [ (0.0, (0.0, 0.0, 0.0))
  , (0.3, (1.0, 0.0, 0.0))
  , (0.6, (1.0, 1.0, 0.0))
  , (1.0, (1.0, 1.0, 1.0))
  ]

plasmaStops =
  [ (0.0, (0.050, 0.030, 0.527))
  , (0.25, (0.470, 0.010, 0.700))
  , (0.5, (0.780, 0.250, 0.500))
  , (0.75, (0.970, 0.620, 0.280))
  , (1.0, (0.940, 0.975, 0.131))
  ]

viridisStops =
  [ (0.0, (0.267, 0.005, 0.329))
  , (0.25, (0.283, 0.141, 0.458))
  , (0.5, (0.254, 0.265, 0.530))
  , (0.75, (0.207, 0.372, 0.553))
  , (1.0, (0.993, 0.906, 0.144))
  ]

turboStops =
  [ (0.0, (0.18995, 0.07176, 0.23217))
  , (0.25, (0.25154, 0.25237, 0.63374))
  , (0.5, (0.27628, 0.51409, 0.71569))
  , (0.75, (0.51115, 0.81748, 0.60114))
  , (1.0, (0.91530, 0.91415, 0.35564))
  ]
renderImage :: Config -> V.Vector Double -> Image PixelRGB8
renderImage Config {..} values = generateImage pixel cfgWidth cfgHeight
  where
    palette = lookupPalette cfgPaletteName
    pixel x y =
      let idx = y * cfgWidth + x
       in palette (values V.! idx)

printSummary :: Config -> V.Vector Double -> V.Vector Sample -> IO ()
printSummary cfg values samples = do
  let (xmin, xmax, ymin, ymax) = deriveBounds cfg
      total = V.length samples
      insideCount = V.length (V.filter (not . spEscaped) samples)
      escapedCount = total - insideCount
      coverage = if total == 0 then 0 else fromIntegral insideCount / fromIntegral total * 100
      minVal = V.minimum values
      maxVal = V.maximum values
  putStrLn "Mandelbrot parameters"
  putStrLn $ "  width x height : " ++ show (cfgWidth cfg) ++ " x " ++ show (cfgHeight cfg)
  putStrLn $ "  iterations      : " ++ show (cfgMaxIter cfg)
  putStrLn $ "  bounds          : " ++ prettyBounds (xmin, xmax, ymin, ymax)
  putStrLn $ "  smooth colouring: " ++ show (cfgSmooth cfg)
  putStrLn $ "  palette         : " ++ cfgPaletteName cfg
  putStrLn "Grid statistics"
  putStrLn $ "  escaped points  : " ++ show escapedCount
  putStrLn $ "  interior points : " ++ show insideCount ++ " (" ++ fmtDouble coverage ++ "%)"
  putStrLn $ "  normalised min  : " ++ fmtDouble minVal
  putStrLn $ "  normalised max  : " ++ fmtDouble maxVal

prettyBounds :: (Double, Double, Double, Double) -> String
prettyBounds (xmin, xmax, ymin, ymax) =
  concat
    [ "[", fmtDouble xmin, ", ", fmtDouble xmax, "] x [", fmtDouble ymin, ", ", fmtDouble ymax, "]"
    ]

fmtDouble :: Double -> String
fmtDouble x = showFFloat (Just 4) x ""

runProgram :: Config -> IO ()
runProgram cfg = do
  let samples = computeGrid cfg
      normalised = normalise cfg samples
  case cfgSavePath cfg of
    Just path -> do
      writePng path (renderImage cfg normalised)
      putStrLn $ "[info] Saved image -> " ++ path
    Nothing -> pure ()
  if cfgSummary cfg
    then printSummary cfg normalised samples
    else pure ()

main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    Left "help" -> putStr usage >> exitSuccess
    Left err -> hPutStrLn stderr ("[error] " ++ err) >> exitFailure
    Right rawCfg ->
      case validateConfig rawCfg of
        Left err -> hPutStrLn stderr ("[error] " ++ err) >> exitFailure
        Right cfg -> runProgram cfg
