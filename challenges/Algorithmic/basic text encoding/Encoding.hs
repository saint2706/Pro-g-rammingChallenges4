{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Monad (unless, when)
import qualified Data.ByteString as BS
import Data.Char (chr, intToDigit, isPrint, isSpace, ord)
import Data.List (intercalate)
import Data.Word (Word8)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Numeric (showIntAtBase)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hFlush, stdout)
import Text.Printf (printf)

-------------------------------------------------------------------------------
-- Encoding primitives
-------------------------------------------------------------------------------

data EncodingChoice
  = EncUtf8
  | EncUtf16
  | EncUtf16LE
  | EncUtf16BE
  | EncAscii
  deriving (Eq, Show)

parseEncoding :: String -> Either String EncodingChoice
parseEncoding name =
  case fmap toLowerAscii name of
    "utf-8" -> Right EncUtf8
    "utf8" -> Right EncUtf8
    "utf-16" -> Right EncUtf16
    "utf16" -> Right EncUtf16
    "utf-16le" -> Right EncUtf16LE
    "utf16le" -> Right EncUtf16LE
    "utf-16be" -> Right EncUtf16BE
    "utf16be" -> Right EncUtf16BE
    "ascii" -> Right EncAscii
    other -> Left $ "Unsupported encoding: " ++ other

encodeText :: EncodingChoice -> T.Text -> Either String BS.ByteString
encodeText choice txt =
  case choice of
    EncUtf8 -> Right $ TE.encodeUtf8 txt
    EncUtf16 -> Right $ addBomLittleEndian (TE.encodeUtf16LE txt)
    EncUtf16LE -> Right $ TE.encodeUtf16LE txt
    EncUtf16BE -> Right $ TE.encodeUtf16BE txt
    EncAscii -> traverseAscii txt
  where
    addBomLittleEndian bs = BS.concat [BS.pack [0xFF, 0xFE], bs]
    traverseAscii t = BS.pack <$> traverse encodeAscii (T.unpack t)
    encodeAscii c
      | ord c <= 0x7F = Right (fromIntegral (ord c))
      | otherwise = Left $ "Character out of ASCII range: " ++ show c

stripBom :: BS.ByteString -> (Maybe EncodingChoice, BS.ByteString)
stripBom bs
  | BS.isPrefixOf (BS.pack [0xFF, 0xFE]) bs = (Just EncUtf16LE, BS.drop 2 bs)
  | BS.isPrefixOf (BS.pack [0xFE, 0xFF]) bs = (Just EncUtf16BE, BS.drop 2 bs)
  | otherwise = (Nothing, bs)

decodeBytes :: EncodingChoice -> BS.ByteString -> Either String T.Text
decodeBytes choice bytes =
  case choice of
    EncUtf8 -> decode TE.decodeUtf8'
    EncUtf16 ->
      case stripBom bytes of
        (Just enc, rest) -> decodeBytes enc rest
        (Nothing, _) -> Left "UTF-16 input missing byte order mark"
    EncUtf16LE -> decode TE.decodeUtf16LE'
    EncUtf16BE -> decode TE.decodeUtf16BE'
    EncAscii -> traverseAscii bytes
  where
    decode f =
      case f bytes of
        Left err -> Left (show err)
        Right txt -> Right txt
    traverseAscii bs = T.pack <$> traverse decodeAscii (BS.unpack bs)
    decodeAscii w
      | w <= 0x7F = Right (chr (fromIntegral w))
      | otherwise = Left $ "Byte not valid ASCII: " ++ show w

encodingName :: EncodingChoice -> T.Text
encodingName enc =
  case enc of
    EncUtf8 -> "utf-8"
    EncUtf16 -> "utf-16"
    EncUtf16LE -> "utf-16le"
    EncUtf16BE -> "utf-16be"
    EncAscii -> "ascii"

-------------------------------------------------------------------------------
-- Conversion helpers
-------------------------------------------------------------------------------

data ConversionMode
  = ModeTextToHex
  | ModeTextToBin
  | ModeHexToText
  | ModeBinToText
  deriving (Eq, Show)

parseMode :: String -> Either String ConversionMode
parseMode name =
  case fmap toLowerAscii name of
    "hex" -> Right ModeTextToHex
    "bin" -> Right ModeTextToBin
    "hex2text" -> Right ModeHexToText
    "bin2text" -> Right ModeBinToText
    other -> Left $ "Unsupported mode: " ++ other

bytesToHex :: BS.ByteString -> [String]
bytesToHex = map (printf "%02x") . BS.unpack

bytesToBinary :: BS.ByteString -> [String]
bytesToBinary = map wordToBin . BS.unpack
  where
    wordToBin w = padLeft 8 '0' (showIntAtBase 2 intToDigit (fromIntegral w) "")

padLeft :: Int -> Char -> String -> String
padLeft width filler str
  | length str >= width = str
  | otherwise = replicate (width - length str) filler ++ str

formatJoined :: String -> [String] -> String
formatJoined sep values = intercalate sep values

wrapLines :: Int -> String -> String
wrapLines limit text
  | limit <= 0 = text
  | otherwise = unlines (chunk limit text)
  where
    chunk _ [] = []
    chunk n xs = take n xs : chunk n (drop n xs)

hexStringToBytes :: String -> Either String BS.ByteString
hexStringToBytes input =
  let cleaned = filter (not . isSpace) input
   in if null cleaned
        then Right BS.empty
        else
          if odd (length cleaned)
            then Left "Input length must be a multiple of two"
            else BS.pack <$> traverse parseByte (chunksOf 2 cleaned)
  where
    parseByte str =
      case readHexByte str of
        Just val -> Right val
        Nothing -> Left $ "Invalid hex byte: " ++ str

readHexByte :: String -> Maybe Word8
readHexByte str = do
  digits <- traverse hexDigitValue str
  let value = foldl (\acc d -> acc * 16 + d) 0 digits
  if value <= 0xFF then Just (fromIntegral value) else Nothing

hexDigitValue :: Char -> Maybe Int
hexDigitValue c
  | '0' <= c && c <= '9' = Just (ord c - ord '0')
  | 'a' <= lc && lc <= 'f' = Just (10 + ord lc - ord 'a')
  | otherwise = Nothing
  where
    lc = toLowerAsciiChar c

binStringToBytes :: String -> Either String BS.ByteString
binStringToBytes input =
  let cleaned = filter (not . isSpace) input
   in if null cleaned
        then Right BS.empty
        else
          if length cleaned `mod` 8 /= 0
            then Left "Binary input must be aligned to 8-bit chunks"
            else BS.pack <$> traverse parseByte (chunksOf 8 cleaned)
  where
    parseByte bits =
      case traverse bitValue bits of
        Nothing -> Left $ "Invalid binary token: " ++ bits
        Just values -> Right (fromIntegral (foldl (\acc b -> acc * 2 + b) 0 values))

bitValue :: Char -> Maybe Int
bitValue '0' = Just 0
bitValue '1' = Just 1
bitValue _ = Nothing

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

-------------------------------------------------------------------------------
-- Visualisation helpers
-------------------------------------------------------------------------------

data RoundTrip = RoundTrip
  { rtFromHex :: T.Text
  , rtFromBin :: T.Text
  , rtHexMatches :: Bool
  , rtBinMatches :: Bool
  }

data AsciiDifference = AsciiDifference
  { adIndex :: Int
  , adByte :: Maybe Word8
  , adAsciiByte :: Maybe Word8
  , adDelta :: Maybe Int
  }

data AsciiComparison = AsciiComparison
  { acSupported :: Bool
  , acReason :: Maybe String
  , acByteValues :: [Word8]
  , acHexValues :: [String]
  , acBinaryValues :: [String]
  , acDifferences :: [AsciiDifference]
  , acNote :: Maybe String
  }

data Metadata = Metadata
  { mdByteCount :: Int
  , mdBitCount :: Int
  , mdIsEmpty :: Bool
  }

data Visualisation = Visualisation
  { visText :: T.Text
  , visEncoding :: EncodingChoice
  , visBytes :: [Word8]
  , visHexValues :: [String]
  , visBinaryValues :: [String]
  , visBitMatrix :: [[Int]]
  , visRoundTrip :: RoundTrip
  , visAscii :: AsciiComparison
  , visMetadata :: Metadata
  }

buildVisualisation :: T.Text -> EncodingChoice -> Either String Visualisation
buildVisualisation txt enc = do
  encodedBytes <- encodeText enc txt
  let hexVals = bytesToHex encodedBytes
      binVals = bytesToBinary encodedBytes
      bitMatrix = map (map charToBit) binVals
      byteList = BS.unpack encodedBytes
  fromHex <-
    if null hexVals
      then Right T.empty
      else hexToText (formatJoined " " hexVals) enc
  fromBin <-
    if null binVals
      then Right T.empty
      else binToText (formatJoined " " binVals) enc
  asciiComparison <- buildAsciiComparison txt enc encodedBytes
  let roundTrip = RoundTrip
        { rtFromHex = fromHex
        , rtFromBin = fromBin
        , rtHexMatches = fromHex == txt
        , rtBinMatches = fromBin == txt
        }
      metadata = Metadata
        { mdByteCount = length byteList
        , mdBitCount = length byteList * 8
        , mdIsEmpty = T.null txt
        }
  pure
    Visualisation
      { visText = txt
      , visEncoding = enc
      , visBytes = byteList
      , visHexValues = hexVals
      , visBinaryValues = binVals
      , visBitMatrix = bitMatrix
      , visRoundTrip = roundTrip
      , visAscii = asciiComparison
      , visMetadata = metadata
      }

charToBit :: Char -> Int
charToBit '1' = 1
charToBit _ = 0

buildAsciiComparison :: T.Text -> EncodingChoice -> BS.ByteString -> Either String AsciiComparison
buildAsciiComparison txt enc encodedBytes =
  case encodeText EncAscii txt of
    Left reason ->
      Right
        AsciiComparison
          { acSupported = False
          , acReason = Just reason
          , acByteValues = []
          , acHexValues = []
          , acBinaryValues = []
          , acDifferences = []
          , acNote = Nothing
          }
    Right asciiBytes ->
      let asciiList = BS.unpack asciiBytes
          asciiHex = bytesToHex asciiBytes
          asciiBin = bytesToBinary asciiBytes
          encodedList = BS.unpack encodedBytes
          differences = buildDifferences encodedList asciiList
          note =
            if enc == EncAscii
              then Just "Input already encoded as ASCII."
              else Nothing
       in Right
            AsciiComparison
              { acSupported = True
              , acReason = Nothing
              , acByteValues = asciiList
              , acHexValues = asciiHex
              , acBinaryValues = asciiBin
              , acDifferences = differences
              , acNote = note
              }

buildDifferences :: [Word8] -> [Word8] -> [AsciiDifference]
buildDifferences encoded asciiBytes =
  let lengthMax = max (length encoded) (length asciiBytes)
   in [ makeDifference idx | idx <- [0 .. lengthMax - 1] ]
  where
    makeDifference idx =
      let encodedByte = indexMaybe encoded idx
          asciiByte = indexMaybe asciiBytes idx
          delta = case (encodedByte, asciiByte) of
            (Just e, Just a) -> Just (fromIntegral e - fromIntegral a)
            _ -> Nothing
       in AsciiDifference
            { adIndex = idx
            , adByte = encodedByte
            , adAsciiByte = asciiByte
            , adDelta = delta
            }
    indexMaybe xs i
      | i < length xs = Just (xs !! i)
      | otherwise = Nothing

-------------------------------------------------------------------------------
-- CLI argument parsing
-------------------------------------------------------------------------------

data Options = Options
  { optMode :: Maybe ConversionMode
  , optEncoding :: EncodingChoice
  , optSeparator :: String
  , optNoFormat :: Bool
  , optInteractive :: Bool
  , optText :: Maybe String
  , optJson :: Maybe String
  , optVisualize :: Bool
  }

defaultOptions :: Options
defaultOptions = Options
  { optMode = Nothing
  , optEncoding = EncUtf8
  , optSeparator = " "
  , optNoFormat = False
  , optInteractive = False
  , optText = Nothing
  , optJson = Nothing
  , optVisualize = False
  }

parseArgs :: [String] -> Either String Options
parseArgs = go defaultOptions
  where
    go opts [] = Right opts
    go _ ("--mode":[]) = Left "Missing value for --mode"
    go opts ("--mode":mode:rest) = do
      parsedMode <- parseMode mode
      go opts {optMode = Just parsedMode} rest
    go _ ("-m":[]) = Left "Missing value for -m"
    go opts ("-m":mode:rest) = go opts ("--mode":mode:rest)
    go _ ("--encoding":[]) = Left "Missing value for --encoding"
    go opts ("--encoding":enc:rest) = do
      parsedEnc <- parseEncoding enc
      go opts {optEncoding = parsedEnc} rest
    go _ ("-e":[]) = Left "Missing value for -e"
    go opts ("-e":enc:rest) = go opts ("--encoding":enc:rest)
    go _ ("--separator":[]) = Left "Missing value for --separator"
    go opts ("--separator":sep:rest) = go opts {optSeparator = sep} rest
    go _ ("-s":[]) = Left "Missing value for -s"
    go opts ("-s":sep:rest) = go opts ("--separator":sep:rest)
    go opts ("--no-format":rest) = go opts {optNoFormat = True} rest
    go opts ("--interactive":rest) = go opts {optInteractive = True} rest
    go opts ("-i":rest) = go opts ("--interactive":rest)
    go _ ("--text":[]) = Left "Missing value for --text"
    go opts ("--text":txt:rest) = go opts {optText = Just txt} rest
    go _ ("-t":[]) = Left "Missing value for -t"
    go opts ("-t":txt:rest) = go opts ("--text":txt:rest)
    go _ ("--json":[]) = Left "Missing value for --json"
    go opts ("--json":path:rest) = go opts {optJson = Just path} rest
    go opts ("--visualize":rest) = go opts {optVisualize = True} rest
    go opts ("--":rest) =
      case optText opts of
        Nothing ->
          case rest of
            [] -> Right opts {optText = Just ""}
            (txt:remaining) -> go opts {optText = Just txt} remaining
        Just _ -> Left "Unexpected -- delimiter"
    go opts (value:rest)
      | isOption value = Left $ "Unknown option: " ++ value
      | otherwise =
          case optText opts of
            Nothing -> go opts {optText = Just value} rest
            Just _ ->
              case optMode opts of
                Nothing -> do
                  parsedMode <- parseMode value
                  go opts {optMode = Just parsedMode} rest
                Just _ -> Left $ "Unrecognised argument: " ++ value

isOption :: String -> Bool
isOption [] = False
isOption (x:_) = x == '-'

usageMessage :: String
usageMessage = unlines
  [ "Usage: Encoding [--mode MODE] [--encoding NAME] [--separator SEP]"
  , "                  [--no-format] [--interactive] [--text INPUT]"
  , "                  [--json PATH] [--visualize]"
  , "Modes: hex, bin, hex2text, bin2text"
  , "Encodings: utf-8, utf-16, utf-16le, utf-16be, ascii"
  , "Examples:"
  , "  Encoding --mode hex --text \"Hello\""
  , "  Encoding --mode bin2text --text \"01001000 01101001\" --encoding ascii"
  , "  Encoding --interactive"
  , "  Encoding --mode hex --text Hello --json -"
  ]

-------------------------------------------------------------------------------
-- JSON serialisation helpers
-------------------------------------------------------------------------------

visualisationToJson :: Visualisation -> T.Text
visualisationToJson vis =
  jsonObject
    [ ("text", jsonString (visText vis))
    , ("encoding", jsonString (encodingName (visEncoding vis)))
    , ("byte_values", jsonArray (map (jsonNumber . word8ToInt) (visBytes vis)))
    , ("hex_values", jsonArray (map (jsonString . T.pack) (visHexValues vis)))
    , ("binary_values", jsonArray (map (jsonString . T.pack) (visBinaryValues vis)))
    , ("bit_matrix", jsonArray (map jsonBitRow (visBitMatrix vis)))
    , ("roundtrip", roundTripToJson (visRoundTrip vis))
    , ("comparisons", asciiComparisonToJson (visAscii vis))
    , ("metadata", metadataToJson (visMetadata vis))
    ]

jsonBitRow :: [Int] -> T.Text
jsonBitRow row = jsonArray (map jsonNumber row)

roundTripToJson :: RoundTrip -> T.Text
roundTripToJson rt =
  jsonObject
    [ ("from_hex", jsonString (rtFromHex rt))
    , ("from_bin", jsonString (rtFromBin rt))
    , ("hex_matches", jsonBool (rtHexMatches rt))
    , ("bin_matches", jsonBool (rtBinMatches rt))
    ]

asciiComparisonToJson :: AsciiComparison -> T.Text
asciiComparisonToJson cmp =
  jsonObject
    [ ("encoding", jsonString "ascii")
    , ("supported", jsonBool (acSupported cmp))
    , ("reason", maybe jsonNull (jsonString . T.pack) (acReason cmp))
    , ("byte_values", jsonArray (map (jsonNumber . word8ToInt) (acByteValues cmp)))
    , ("hex_values", jsonArray (map (jsonString . T.pack) (acHexValues cmp)))
    , ("binary_values", jsonArray (map (jsonString . T.pack) (acBinaryValues cmp)))
    , ("differences", jsonArray (map asciiDifferenceToJson (acDifferences cmp)))
    , ("note", maybe jsonNull (jsonString . T.pack) (acNote cmp))
    ]

asciiDifferenceToJson :: AsciiDifference -> T.Text
asciiDifferenceToJson diff =
  jsonObject
    [ ("index", jsonNumber (adIndex diff))
    , ("byte", maybe jsonNull (jsonNumber . word8ToInt) (adByte diff))
    , ("ascii_byte", maybe jsonNull (jsonNumber . word8ToInt) (adAsciiByte diff))
    , ("delta", maybe jsonNull jsonNumber (adDelta diff))
    ]

metadataToJson :: Metadata -> T.Text
metadataToJson meta =
  jsonObject
    [ ("byte_count", jsonNumber (mdByteCount meta))
    , ("bit_count", jsonNumber (mdBitCount meta))
    , ("is_empty", jsonBool (mdIsEmpty meta))
    ]

jsonObject :: [(T.Text, T.Text)] -> T.Text
jsonObject fields =
  T.concat
    [ "{"
    , T.intercalate "," (map encodeField fields)
    , "}"
    ]
  where
    encodeField (key, value) = jsonString key <> ":" <> value

jsonArray :: [T.Text] -> T.Text
jsonArray items = T.concat ["[", T.intercalate "," items, "]"]

jsonString :: T.Text -> T.Text
jsonString txt = T.cons '"' (escape txt <> T.singleton '"')
  where
    escape = T.concatMap escapeChar
    escapeChar '\\' = "\\\\"
    escapeChar '"' = "\\\""
    escapeChar '\n' = "\\n"
    escapeChar '\r' = "\\r"
    escapeChar '\t' = "\\t"
    escapeChar c
      | isPrint c && ord c < 0x80 = T.singleton c
      | otherwise = T.pack (printf "\\u%04x" (ord c))

jsonNumber :: (Show a) => a -> T.Text
jsonNumber = T.pack . show

jsonBool :: Bool -> T.Text
jsonBool True = "true"
jsonBool False = "false"

jsonNull :: T.Text
jsonNull = "null"

-------------------------------------------------------------------------------
-- Interactive mode
-------------------------------------------------------------------------------

interactiveLoop :: IO ()
interactiveLoop = do
  putStrLn "============================================================"
  putStrLn "    Text Encoding Converter - Haskell Edition"
  putStrLn "============================================================"
  loop EncUtf8
  where
    loop currentEnc = do
      putStrLn "Available operations:"
      putStrLn "1. Text to Hexadecimal"
      putStrLn "2. Text to Binary"
      putStrLn "3. Hexadecimal to Text"
      putStrLn "4. Binary to Text"
      putStrLn $ "5. Change encoding (current: " ++ T.unpack (encodingName currentEnc) ++ ")"
      putStrLn "6. Exit"
      putStr "Select an operation (1-6): "
      hFlush stdout
      choice <- getLine
      case choice of
        "6" -> putStrLn "Goodbye!"
        "5" -> do
          putStr "Enter encoding (utf-8, ascii, utf-16, etc.): "
          hFlush stdout
          encName <- getLine
          case parseEncoding encName of
            Left err -> putStrLn err >> loop currentEnc
            Right newEnc -> do
              putStrLn $ "Encoding changed to: " ++ T.unpack (encodingName newEnc)
              loop newEnc
        option | option `elem` ["1", "2", "3", "4"] -> do
          putStr "Enter text/data: "
          hFlush stdout
          input <- getLine
          unless (null input) $ do
            case option of
              "1" -> performAndPrint ModeTextToHex input currentEnc
              "2" -> performAndPrint ModeTextToBin input currentEnc
              "3" -> performAndPrint ModeHexToText input currentEnc
              "4" -> performAndPrint ModeBinToText input currentEnc
          putStrLn "----------------------------------------"
          loop currentEnc
        _ -> putStrLn "Invalid choice. Please select 1-6." >> loop currentEnc

performAndPrint :: ConversionMode -> String -> EncodingChoice -> IO ()
performAndPrint mode input enc =
  case runConversion mode enc " " input of
    Left err -> putStrLn $ "Error: " ++ err
    Right (output, _) -> putStrLn output

-------------------------------------------------------------------------------
-- Core conversion routing
-------------------------------------------------------------------------------

runConversion :: ConversionMode -> EncodingChoice -> String -> String -> Either String (String, T.Text)
runConversion mode enc sep input =
  case mode of
    ModeTextToHex -> do
      bytes <- encodeText enc (T.pack input)
      let result = formatJoined sep (bytesToHex bytes)
      pure (result, T.pack input)
    ModeTextToBin -> do
      bytes <- encodeText enc (T.pack input)
      let result = formatJoined sep (bytesToBinary bytes)
      pure (result, T.pack input)
    ModeHexToText -> do
      bytes <- hexStringToBytes input
      txt <- decodeBytes enc bytes
      pure (T.unpack txt, txt)
    ModeBinToText -> do
      bytes <- binStringToBytes input
      txt <- decodeBytes enc bytes
      pure (T.unpack txt, txt)

-------------------------------------------------------------------------------
-- CLI execution
-------------------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  when ("--help" `elem` args || "-h" `elem` args) $ do
    putStrLn usageMessage
    exitSuccess
  case parseArgs args of
    Left err -> putStrLn err >> exitFailure
    Right opts ->
      if optInteractive opts
        then interactiveLoop
        else runOnce opts

runOnce :: Options -> IO ()
runOnce opts = do
  mode <- case optMode opts of
    Nothing -> putStrLn "Error: --mode is required unless --interactive is used." >> putStrLn usageMessage >> exitFailure
    Just m -> pure m
  input <- case optText opts of
    Nothing -> putStrLn "Error: --text or positional input is required." >> exitFailure
    Just txt -> pure txt
  let enc = optEncoding opts
      separator = optSeparator opts
  case runConversion mode enc separator input of
    Left err -> putStrLn ("Error: " ++ err) >> exitFailure
    Right (value, visualText) -> do
      let output = if optNoFormat opts then value else wrapLines 80 value
      putStrLn (resultLabel mode)
      putStrLn output
      when (optJson opts /= Nothing || optVisualize opts) $
        case buildVisualisation visualText enc of
          Left err -> putStrLn ("Visualisation error: " ++ err)
          Right vis -> do
            when (optVisualize opts) (renderVisualisation vis)
            case optJson opts of
              Nothing -> pure ()
              Just path ->
                let payload = visualisationToJson vis
                 in if path == "-"
                      then TIO.putStrLn payload
                      else TIO.writeFile path payload

resultLabel :: ConversionMode -> String
resultLabel ModeTextToHex = "Hexadecimal representation:"
resultLabel ModeTextToBin = "Binary representation:"
resultLabel ModeHexToText = "Decoded text:"
resultLabel ModeBinToText = "Decoded text:"

renderVisualisation :: Visualisation -> IO ()
renderVisualisation vis = do
  putStrLn "\nVisualisation"
  putStrLn "------------"
  putStrLn $ "Encoding: " ++ T.unpack (encodingName (visEncoding vis))
  putStrLn $ "Bytes: " ++ show (visBytes vis)
  putStrLn $ "Hex: " ++ intercalate " " (visHexValues vis)
  putStrLn $ "Binary: " ++ intercalate " | " (visBinaryValues vis)
  if null (visBitMatrix vis)
    then putStrLn "Bit matrix: <empty>"
    else do
      putStrLn "Bit matrix:"
      mapM_ (putStrLn . concatMap show) (visBitMatrix vis)
  putStrLn "Metadata:"
  putStrLn $ "  Byte count: " ++ show (mdByteCount (visMetadata vis))
  putStrLn $ "  Bit count: " ++ show (mdBitCount (visMetadata vis))
  putStrLn $ "  Is empty: " ++ show (mdIsEmpty (visMetadata vis))
  case visAscii vis of
    AsciiComparison { acSupported = False, acReason = reason } ->
      putStrLn $ "ASCII comparison unavailable: " ++ fromMaybe "" reason
    AsciiComparison { acSupported = True
                    , acByteValues = bytes
                    , acHexValues = hexVals
                    , acBinaryValues = binVals
                    , acDifferences = diffs
                    , acNote = note
                    } -> do
      putStrLn "ASCII comparison:"
      putStrLn $ "  Bytes: " ++ show bytes
      putStrLn $ "  Hex: " ++ intercalate " " hexVals
      putStrLn $ "  Binary: " ++ intercalate " " binVals
      mapM_ printDifference diffs
      maybe (pure ()) (putStrLn . ("  Note: " ++)) note
  where
    fromMaybe fallback = maybe fallback id
    printDifference diff =
      putStrLn $ "  Index " ++ show (adIndex diff)
        ++ ": byte=" ++ maybe "null" show (adByte diff)
        ++ ", ascii_byte=" ++ maybe "null" show (adAsciiByte diff)
        ++ ", delta=" ++ maybe "null" show (adDelta diff)

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

word8ToInt :: Word8 -> Int
word8ToInt = fromIntegral

toLowerAscii :: String -> String
toLowerAscii = map toLowerAsciiChar

toLowerAsciiChar :: Char -> Char
toLowerAsciiChar c
  | 'A' <= c && c <= 'Z' = chr (ord c + 32)
  | otherwise = c

hexToText :: String -> EncodingChoice -> Either String T.Text
hexToText input enc = do
  bytes <- hexStringToBytes input
  decodeBytes enc bytes

binToText :: String -> EncodingChoice -> Either String T.Text
binToText input enc = do
  bytes <- binStringToBytes input
  decodeBytes enc bytes

-------------------------------------------------------------------------------
