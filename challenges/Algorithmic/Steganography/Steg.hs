{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Codec.Picture
import Codec.Picture.Types (Image(..), PixelRGB8(..), Pixel8)
import Control.Monad (when)
import Control.Monad.ST (ST, runST)
import Data.Bits ((.&.), (.|.), shiftL, testBit, xor)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Aeson (Value(..), encode, object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.List (foldl', stripPrefix)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Text.Encoding.Error (lenientDecode)
import qualified Data.Text.IO as TIO
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import Data.Word (Word8)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

--------------------------------------------------------------------------------
-- Constants & escaping helpers
--------------------------------------------------------------------------------

eomMarker :: Text
eomMarker = "||EOM||"

escapeSeq :: Text
escapeSeq = "\\E"

escapeMessage :: Text -> Text
escapeMessage = T.replace escapeSeq (escapeSeq <> escapeSeq)
               . T.replace eomMarker (escapeSeq <> eomMarker)

unescapeMessage :: Text -> Text
unescapeMessage txt =
  case T.breakOn escapeSeq txt of
    (prefix, remainder)
      | T.null remainder -> prefix
      | otherwise ->
          let afterEsc = T.drop (T.length escapeSeq) remainder
           in case T.uncons afterEsc of
                Nothing -> prefix
                Just (c, rest) -> prefix <> T.singleton c <> unescapeMessage rest

--------------------------------------------------------------------------------
-- Bitwise conversions
--------------------------------------------------------------------------------

textToBits :: Text -> [Word8]
textToBits = concatMap byteToBits . BS.unpack . TE.encodeUtf8

    byteToBits :: Word8 -> [Word8]
    byteToBits w = [if testBit w shift then 1 else 0 | shift <- [7,6..0]]

bitsToText :: [Word8] -> Text
bitsToText bits =
  let bytes = chunk bits
      bs = BS.pack bytes
   in TE.decodeUtf8With lenientDecode bs
  where
    chunk :: [Word8] -> [Word8]
    chunk [] = []
    chunk xs =
      let (current, rest) = splitAt 8 xs
       in if length current == 8
            then bitsToWord8 current : chunk rest
            else []

    bitsToWord8 :: [Word8] -> Word8
    bitsToWord8 = foldl' shiftInsert 0

    shiftInsert :: Word8 -> Word8 -> Word8
    shiftInsert acc bit = (acc `shiftL` 1) .|. bit

--------------------------------------------------------------------------------
-- Capacity helper
--------------------------------------------------------------------------------

imageCapacityChars :: Int -> Int -> Int
imageCapacityChars width height = (width * height * 3) `div` 8

--------------------------------------------------------------------------------
-- Image IO helpers
--------------------------------------------------------------------------------

loadImageRGB8 :: FilePath -> IO (Image PixelRGB8)
loadImageRGB8 path = do
  result <- readImage path
  case result of
    Left err -> do
      hPutStrLn stderr ("Failed to load image: " ++ err)
      exitFailure
    Right dyn -> pure (convertRGB8 dyn)

savePngRGB8 :: FilePath -> Image PixelRGB8 -> IO ()
savePngRGB8 = writePng

savePngGrey :: FilePath -> Image Pixel8 -> IO ()
savePngGrey = writePng

--------------------------------------------------------------------------------
-- Embedding / Extraction
--------------------------------------------------------------------------------

hideMessageInImage :: Image PixelRGB8 -> Text -> Either String (Image PixelRGB8)
hideMessageInImage img message =
  let escaped = escapeMessage message <> eomMarker
      bitStream = textToBits escaped
      capacityBits = VS.length (imageData img)
   in if length bitStream > capacityBits
        then Left "Message too large for image capacity"
        else Right img { imageData = embedBits (imageData img) bitStream }

embedBits :: Vector Word8 -> [Word8] -> Vector Word8
embedBits pixels bits = runSTVector pixels $ \mvec -> do
  let go _ [] = pure ()
      go idx (b:bs) = do
        current <- VSM.read mvec idx
        VSM.write mvec idx ((current .&. 0xFE) .|. b)
        go (idx + 1) bs
  go 0 bits

extractMessageFromImage :: Image PixelRGB8 -> Either String Text
extractMessageFromImage img =
  let bits = map (.&. 1) (VS.toList (imageData img))
      decoded = bitsToText bits
      (beforeMarker, afterMarker) = T.breakOn eomMarker decoded
   in if T.null afterMarker
        then Left "No hidden message marker found"
        else Right (unescapeMessage beforeMarker)

runSTVector :: Vector Word8 -> (VSM.MVector s Word8 -> ST s ()) -> Vector Word8
runSTVector vec action = runST $ do
  mvec <- VS.thaw vec
  action mvec
  VS.freeze mvec

--------------------------------------------------------------------------------
-- Diff metrics for visualisation
--------------------------------------------------------------------------------

data ChannelAcc = ChannelAcc
  { chModified :: !Int
  , chBitCounts :: [Int]
  }

emptyChannel :: ChannelAcc
emptyChannel = ChannelAcc 0 (replicate 8 0)

data DiffAcc = DiffAcc
  { accMask :: [Word8]
  , accModifiedPixels :: !Int
  , accBitsModified :: !Int
  , accAggregate :: [Int]
  , accR :: ChannelAcc
  , accG :: ChannelAcc
  , accB :: ChannelAcc
  }

initialDiffAcc :: DiffAcc
initialDiffAcc = DiffAcc [] 0 0 (replicate 8 0) emptyChannel emptyChannel emptyChannel

computeDiffMetrics :: Image PixelRGB8 -> Image PixelRGB8 -> Either String (Value, Vector Word8)
computeDiffMetrics cover stego
  | imageWidth cover /= imageWidth stego || imageHeight cover /= imageHeight stego =
      Left "Cover and stego images must share dimensions"
  | otherwise =
      let width = imageWidth cover
          height = imageHeight cover
          totalPixels = width * height
          coverData = imageData cover
          stegoData = imageData stego
          finalAcc = foldl' (step coverData stegoData) initialDiffAcc [0 .. totalPixels - 1]
          DiffAcc maskRev modifiedPixels bitsModifiedTotal aggregate rAcc gAcc bAcc = finalAcc
          maskVector = VS.fromList (reverse maskRev)
          capacity = imageCapacityChars width height
          metrics = object
            [ "width" .= width
            , "height" .= height
            , "capacity_chars" .= capacity
            , "total_pixels" .= totalPixels
            , "modified_pixels" .= modifiedPixels
            , "modified_ratio" .= ratio modifiedPixels totalPixels
            , "bits_modified_total" .= bitsModifiedTotal
            , "bit_planes" .= object
                [ "aggregate" .= aggregate
                ]
            , "per_channel" .= object
                [ "R" .= channelMetrics rAcc
                , "G" .= channelMetrics gAcc
                , "B" .= channelMetrics bAcc
                ]
            , "has_modifications" .= (modifiedPixels > 0)
            ]
       in Right (metrics, maskVector)
  where
    ratio :: Int -> Int -> Double
    ratio a b
      | b == 0 = 0
      | otherwise = fromIntegral a / fromIntegral b

    channelMetrics :: ChannelAcc -> Value
    channelMetrics (ChannelAcc m counts) =
      object
        [ "modified_pixels" .= m
        , "bit_counts" .= counts
        , "bits_modified_total" .= sum counts
        ]

    step :: Vector Word8 -> Vector Word8 -> DiffAcc -> Int -> DiffAcc
    step coverVec stegoVec acc idx =
      let base = idx * 3
          r1 = coverVec VS.! base
          g1 = coverVec VS.! (base + 1)
          b1 = coverVec VS.! (base + 2)
          r2 = stegoVec VS.! base
          g2 = stegoVec VS.! (base + 1)
          b2 = stegoVec VS.! (base + 2)
          diffR = r1 `xor` r2
          diffG = g1 `xor` g2
          diffB = b1 `xor` b2
          changed = diffR /= 0 || diffG /= 0 || diffB /= 0
          (rAcc', agg1, bits1) = updateChannel diffR (accR acc) (accAggregate acc) (accBitsModified acc)
          (gAcc', agg2, bits2) = updateChannel diffG (accG acc) agg1 bits1
          (bAcc', agg3, bits3) = updateChannel diffB (accB acc) agg2 bits2
          maskVal = if changed then 255 else 0
          modifiedPixels' = if changed then accModifiedPixels acc + 1 else accModifiedPixels acc
       in acc
            { accMask = maskVal : accMask acc
            , accModifiedPixels = modifiedPixels'
            , accBitsModified = bits3
            , accAggregate = agg3
            , accR = rAcc'
            , accG = gAcc'
            , accB = bAcc'
            }

    updateChannel :: Word8 -> ChannelAcc -> [Int] -> Int -> (ChannelAcc, [Int], Int)
    updateChannel diff channelAcc aggregate bitsTotal
      | diff == 0 = (channelAcc, aggregate, bitsTotal)
      | otherwise =
          let increments = [if testBit diff bit then 1 else 0 | bit <- [0..7]]
              bitSum = sum increments
              counts' = zipWith (+) (chBitCounts channelAcc) increments
              aggregate' = zipWith (+) aggregate increments
           in ( ChannelAcc (chModified channelAcc + 1) counts'
              , aggregate'
              , bitsTotal + bitSum
              )

--------------------------------------------------------------------------------
-- CLI parsing helpers
--------------------------------------------------------------------------------

data MessageSource = MsgLiteral Text | MsgFile FilePath | MsgStdin

data Command
  = CmdCapacity FilePath Bool
  | CmdHide FilePath FilePath MessageSource Bool
  | CmdExtract FilePath Bool
  | CmdAnalyse FilePath FilePath AnalyseOptions
  deriving (Eq, Show)

data AnalyseOptions = AnalyseOptions
  { optJsonStdout :: !Bool
  , optMetricsOut :: !(Maybe FilePath)
  , optMaskOut :: !(Maybe FilePath)
  , optOverlayOut :: !(Maybe FilePath)
  } deriving (Eq, Show)

defaultAnalyseOptions :: AnalyseOptions
defaultAnalyseOptions = AnalyseOptions False Nothing Nothing Nothing

parseCommand :: [String] -> Either String Command
parseCommand ("capacity":rest) = parseCapacity rest
parseCommand ("hide":rest) = parseHide rest
parseCommand ("extract":rest) = parseExtract rest
parseCommand ("analyse":rest) = parseAnalyse rest
parseCommand [] = Left usage
parseCommand (cmd:_) = Left ("Unknown command: " ++ cmd ++ "\n" ++ usage)

parseCapacity :: [String] -> Either String Command
parseCapacity (img:flags) = do
  jsonFlag <- parseJsonFlag flags
  pure (CmdCapacity img jsonFlag)
parseCapacity _ = Left "Usage: Steg.hs capacity <image> [--json]"

parseHide :: [String] -> Either String Command
parseHide (img:out:flags) = do
  (maybeSource, jsonFlag) <- parseHideFlags Nothing False flags
  source <- maybe (Left "Specify one of --message, --message-file, or --stdin") Right maybeSource
  pure (CmdHide img out source jsonFlag)
  where
    parseHideFlags :: Maybe MessageSource -> Bool -> [String] -> Either String (Maybe MessageSource, Bool)
    parseHideFlags src json [] = Right (src, json)
    parseHideFlags src json (flag:rest)
      | flag == "--json" = parseHideFlags src True rest
      | flag == "--stdin" =
          case src of
            Nothing -> parseHideFlags (Just MsgStdin) json rest
            Just _ -> Left "Multiple message sources specified"
      | flag == "--message" =
          case rest of
            (value:xs) ->
              case src of
                Nothing -> parseHideFlags (Just (MsgLiteral (T.pack value))) json xs
                Just _ -> Left "Multiple message sources specified"
            [] -> Left "--message requires a value"
      | flag == "--message-file" =
          case rest of
            (value:xs) ->
              case src of
                Nothing -> parseHideFlags (Just (MsgFile value)) json xs
                Just _ -> Left "Multiple message sources specified"
            [] -> Left "--message-file requires a path"
      | Just value <- stripPrefix "--message=" flag =
          case src of
            Nothing -> parseHideFlags (Just (MsgLiteral (T.pack value))) json rest
            Just _ -> Left "Multiple message sources specified"
      | Just value <- stripPrefix "--message-file=" flag =
          case src of
            Nothing -> parseHideFlags (Just (MsgFile value)) json rest
            Just _ -> Left "Multiple message sources specified"
      | otherwise = Left ("Unknown flag for hide: " ++ flag)
parseHide _ = Left "Usage: Steg.hs hide <image> <output> [--message TEXT | --message-file PATH | --stdin] [--json]"

parseExtract :: [String] -> Either String Command
parseExtract (img:flags) = do
  jsonFlag <- parseJsonFlag flags
  pure (CmdExtract img jsonFlag)
parseExtract _ = Left "Usage: Steg.hs extract <image> [--json]"

parseAnalyse :: [String] -> Either String Command
parseAnalyse (cover:stego:flags) = do
  options <- parseAnalyseFlags flags defaultAnalyseOptions
  pure (CmdAnalyse cover stego options)
parseAnalyse _ = Left "Usage: Steg.hs analyse <cover> <stego> [--json] [--metrics-out FILE] [--export-mask FILE] [--export-overlay FILE]"

parseAnalyseFlags :: [String] -> AnalyseOptions -> Either String AnalyseOptions
parseAnalyseFlags [] opts = Right opts
parseAnalyseFlags (flag:rest) opts = case flag of
  "--json" -> parseAnalyseFlags rest opts { optJsonStdout = True }
  "--metrics-out" -> case rest of
    (path:xs) -> parseAnalyseFlags xs opts { optMetricsOut = Just path }
    _ -> Left "--metrics-out requires a file path"
  "--export-mask" -> case rest of
    (path:xs) -> parseAnalyseFlags xs opts { optMaskOut = Just path }
    _ -> Left "--export-mask requires a file path"
  "--export-overlay" -> case rest of
    (path:xs) -> parseAnalyseFlags xs opts { optOverlayOut = Just path }
    _ -> Left "--export-overlay requires a file path"
  _ -> Left ("Unknown flag for analyse: " ++ flag)

parseJsonFlag :: [String] -> Either String Bool
parseJsonFlag flags = case flags of
  [] -> Right False
  ["--json"] -> Right True
  _ -> Left "Unrecognised flags. Only --json is supported here."

usage :: String
usage = unlines
  [ "Usage: Steg.hs <command> [options]"
  , "Commands:"
  , "  capacity <image> [--json]"
  , "  hide <image> <output> [--message TEXT | --message-file PATH | --stdin] [--json]"
  , "  extract <image> [--json]"
  , "  analyse <cover> <stego> [--json] [--metrics-out FILE] [--export-mask FILE] [--export-overlay FILE]"
  ]

--------------------------------------------------------------------------------
-- Command execution
--------------------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  case parseCommand args of
    Left err -> do
      hPutStrLn stderr err
      exitFailure
    Right command -> runCommand command

runCommand :: Command -> IO ()
runCommand (CmdCapacity path jsonFlag) = do
  img <- loadImageRGB8 path
  let capacity = imageCapacityChars (imageWidth img) (imageHeight img)
  if jsonFlag
    then BL.putStrLn (encode (object ["width" .= imageWidth img, "height" .= imageHeight img, "capacity_chars" .= capacity]))
    else putStrLn ("Capacity: " ++ show capacity ++ " characters")

runCommand (CmdHide coverPath outputPath source jsonFlag) = do
  img <- loadImageRGB8 coverPath
  message <- resolveMessage source
  case hideMessageInImage img message of
    Left err -> do
      hPutStrLn stderr ("Error: " ++ err)
      exitFailure
    Right stego -> do
      savePngRGB8 outputPath stego
      if jsonFlag
        then do
          let payload = object
                [ "status" .= ("ok" :: Text)
                , "output" .= outputPath
                , "width" .= imageWidth img
                , "height" .= imageHeight img
                , "capacity_chars" .= imageCapacityChars (imageWidth img) (imageHeight img)
                , "message_length" .= T.length message
                ]
          BL.putStrLn (encode payload)
        else putStrLn ("Message hidden in " ++ outputPath)

runCommand (CmdExtract stegoPath jsonFlag) = do
  img <- loadImageRGB8 stegoPath
  case extractMessageFromImage img of
    Left err -> do
      hPutStrLn stderr ("Error: " ++ err)
      exitFailure
    Right msg ->
      if jsonFlag
        then BL.putStrLn (encode (object ["status" .= ("ok" :: Text), "length" .= T.length msg, "message_sample" .= T.take 120 msg]))
        else TIO.putStrLn msg

runCommand (CmdAnalyse coverPath stegoPath opts) = do
  cover <- loadImageRGB8 coverPath
  stego <- loadImageRGB8 stegoPath
  case computeDiffMetrics cover stego of
    Left err -> do
      hPutStrLn stderr ("Error: " ++ err)
      exitFailure
    Right (metrics, maskVector) -> do
      let metricsWithPaths = addPaths metrics
      when (optJsonStdout opts) (BL.putStrLn (encode metricsWithPaths))
      case optMetricsOut opts of
        Just path -> BL.writeFile path (encode metricsWithPaths)
        Nothing -> pure ()
      case optMaskOut opts of
        Just path -> savePngGrey path (maskToImage (imageWidth cover) (imageHeight cover) maskVector)
        Nothing -> pure ()
      case optOverlayOut opts of
        Just path -> savePngRGB8 path (overlayImage stego maskVector)
        Nothing -> pure ()
      when (not (optJsonStdout opts) && optMetricsOut opts == Nothing) $ do
        putStrLn ("Modified pixels: " ++ show (countNonZero maskVector))

  where
    addPaths :: Value -> Value
    addPaths (Object obj) =
      let objWithCover = KM.insert (Key.fromString "cover_path") (String (T.pack coverPath)) obj
          objWithStego = KM.insert (Key.fromString "stego_path") (String (T.pack stegoPath)) objWithCover
       in Object objWithStego
    addPaths other = other

    maskToImage :: Int -> Int -> Vector Word8 -> Image Pixel8
    maskToImage width height vec = Image width height vec

    overlayImage :: Image PixelRGB8 -> Vector Word8 -> Image PixelRGB8
    overlayImage stego maskVec = generateImage pixelFn (imageWidth stego) (imageHeight stego)
      where
        pixelFn x y =
          let PixelRGB8 r g b = pixelAt stego x y
              idx = y * imageWidth stego + x
              maskVal = maskVec VS.! idx
           in if maskVal == 0
                then PixelRGB8 r g b
                else tintRed r g b

        tintRed :: Word8 -> Word8 -> Word8 -> PixelRGB8
        tintRed r g b = PixelRGB8 (blend 255 r) (blend 0 g) (blend 0 b)

        blend :: Word8 -> Word8 -> Word8
        blend overlay orig =
          let value = round ((fromIntegral overlay + fromIntegral orig) / 2.0 :: Double) :: Int
              clamped = max 0 (min 255 value)
           in fromIntegral clamped

    countNonZero :: Vector Word8 -> Int
    countNonZero = VS.foldl' (\acc v -> if v /= 0 then acc + 1 else acc) 0

resolveMessage :: MessageSource -> IO Text
resolveMessage source = case source of
  MsgLiteral txt -> pure txt
  MsgFile path -> TIO.readFile path
  MsgStdin -> TIO.getContents
