{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Main
-- Description : Educational Master Boot Record parser implemented in Haskell.
--
-- This standalone tool mirrors the Python reference implementation by reading
-- a 512-byte sector, decoding legacy partition entries, and emitting either a
-- human readable summary or JSON. It also shares the deterministic dummy image
-- generator so both languages stay interoperable with the bundled visualiser.
module Main (main) where

import Control.Monad (foldM, unless, when)
import Data.Bits ((.&.), shiftL, shiftR)
import qualified Data.ByteString as BS
import Data.Char (toLower)
import Data.List (intercalate)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Word (Word16, Word32, Word8)
import Numeric (showHex)
import System.Console.GetOpt
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitFailure, exitWith)
import System.IO (hPutStrLn, stderr)
import Text.Printf (printf)

--------------------------------------------------------------------------------
-- Constants and data models ---------------------------------------------------
--------------------------------------------------------------------------------

sectorBytesDefault :: Int
sectorBytesDefault = 512

mbrSize :: Int
mbrSize = 512

partitionTableOffset :: Int
partitionTableOffset = 446

partitionEntrySize :: Int
partitionEntrySize = 16

partitionEntryCount :: Int
partitionEntryCount = 4

mbrSignatureOffset :: Int
mbrSignatureOffset = 510

mbrSignature :: Word16
mbrSignature = 0xAA55

data LogLevel
  = LogError
  | LogWarn
  | LogInfo
  | LogDebug
  deriving (Eq, Ord, Enum, Bounded, Show)

data Options = Options
  { optFile :: Maybe FilePath
  , optJson :: Bool
  , optShowEmpty :: Bool
  , optCreateDummy :: Maybe FilePath
  , optForce :: Bool
  , optSectorSize :: Int
  , optLogLevel :: LogLevel
  }

defaultOptions :: Options
defaultOptions =
  Options
    { optFile = Nothing
    , optJson = False
    , optShowEmpty = False
    , optCreateDummy = Nothing
    , optForce = False
    , optSectorSize = sectorBytesDefault
    , optLogLevel = LogWarn
    }

data PartitionEntry = PartitionEntry
  { partIndex :: Int
  , partBootable :: Bool
  , partStatusRaw :: Word8
  , partTypeCode :: Word8
  , partStartCHS :: (Int, Int, Int)
  , partEndCHS :: (Int, Int, Int)
  , partStartLBA :: Word32
  , partSectors :: Word32
  , partSectorBytes :: Int
  }
  deriving (Show)

data MBRParseResult = MBRParseResult
  { mbrSignatureValue :: Word16
  , mbrSignatureValid :: Bool
  , mbrPartitions :: [PartitionEntry]
  , mbrSectorBytes :: Int
  }
  deriving (Show)

--------------------------------------------------------------------------------
-- Logging ---------------------------------------------------------------------
--------------------------------------------------------------------------------

parseLogLevel :: String -> Either String LogLevel
parseLogLevel raw =
  case map toLower raw of
    "error" -> Right LogError
    "warn" -> Right LogWarn
    "warning" -> Right LogWarn
    "info" -> Right LogInfo
    "debug" -> Right LogDebug
    _ -> Left $ "Unknown log level '" ++ raw ++ "' (expected error|warn|info|debug)"

logMessage :: LogLevel -> Options -> String -> IO ()
logMessage level opts msg =
  when (level <= optLogLevel opts) $ do
    let prefix = case level of
          LogError -> "[ERROR] "
          LogWarn -> "[WARN ] "
          LogInfo -> "[INFO ] "
          LogDebug -> "[DEBUG] "
    hPutStrLn stderr (prefix ++ msg)

--------------------------------------------------------------------------------
-- CLI -------------------------------------------------------------------------
--------------------------------------------------------------------------------

optionsSpec :: [OptDescr (Options -> Either String Options)]
optionsSpec =
  [ Option ['f'] ["file"] (ReqArg setFile "PATH") "Path to disk image containing an MBR (default: dummy_mbr.bin)"
  , Option [] ["json"] (NoArg setJson) "Output JSON instead of human readable text"
  , Option [] ["show-empty"] (NoArg setShowEmpty) "Show empty entries explicitly in human readable mode"
  , Option [] ["create-dummy"] (ReqArg setCreate "PATH") "Create a deterministic dummy MBR at PATH"
  , Option [] ["force"] (NoArg setForce) "Overwrite when using --create-dummy"
  , Option [] ["sector-size"] (ReqArg setSector "BYTES") "Logical sector size in bytes (default 512)"
  , Option [] ["log"] (ReqArg setLog "LEVEL") "Set logging verbosity (error|warn|info|debug)"
  ]
  where
    setFile path opts = Right $ opts {optFile = Just path}
    setJson opts = Right $ opts {optJson = True}
    setShowEmpty opts = Right $ opts {optShowEmpty = True}
    setCreate path opts = Right $ opts {optCreateDummy = Just path}
    setForce opts = Right $ opts {optForce = True}
    setSector arg opts =
      case reads arg of
        [(n, "")] | n > 0 -> Right $ opts {optSectorSize = n}
        _ -> Left $ "Invalid sector size '" ++ arg ++ "'"
    setLog arg opts =
      case parseLogLevel arg of
        Right lvl -> Right $ opts {optLogLevel = lvl}
        Left err -> Left err

parseOptions :: [String] -> IO Options
parseOptions argv = do
  let (actions, nonOpts, errs) = getOpt Permute optionsSpec argv
  unless (null errs) $ do
    mapM_ (hPutStrLn stderr) errs
    hPutStrLn stderr (usageInfo "Usage: mbr [OPTIONS]" optionsSpec)
    exitFailure
  unless (null nonOpts) $ do
    hPutStrLn stderr $ "Unexpected positional arguments: " ++ unwords nonOpts
    hPutStrLn stderr (usageInfo "Usage: mbr [OPTIONS]" optionsSpec)
    exitFailure
  foldM apply defaultOptions actions
  where
    apply opts action =
      case action opts of
        Right updated -> pure updated
        Left err -> do
          hPutStrLn stderr err
          hPutStrLn stderr (usageInfo "Usage: mbr [OPTIONS]" optionsSpec)
          exitFailure

--------------------------------------------------------------------------------
-- Partition helpers -----------------------------------------------------------
--------------------------------------------------------------------------------

typeMap :: [(Word8, String)]
typeMap =
  [ (0x00, "Unused")
  , (0x01, "FAT12")
  , (0x04, "FAT16 <32M")
  , (0x05, "Extended")
  , (0x06, "FAT16")
  , (0x07, "NTFS/ExFAT/HPFS")
  , (0x0B, "FAT32")
  , (0x0C, "FAT32 LBA")
  , (0x0E, "FAT16 LBA")
  , (0x0F, "Extended LBA")
  , (0x82, "Linux Swap")
  , (0x83, "Linux Filesystem")
  , (0x84, "Hibernate")
  , (0x85, "Linux Extended")
  , (0x8E, "Linux LVM")
  , (0xA5, "FreeBSD")
  , (0xA8, "macOS UFS")
  , (0xAB, "macOS Boot")
  , (0xAF, "macOS HFS/HFS+")
  , (0xEE, "GPT Protective")
  , (0xEF, "EFI System")
  ]

typeDescription :: Word8 -> String
typeDescription code = fromMaybe "Unknown" (lookup code typeMap)

decodeCHS :: BS.ByteString -> (Int, Int, Int)
decodeCHS triple
  | BS.length triple /= 3 = (0, 0, 0)
  | otherwise =
      let h = fromIntegral (BS.index triple 0)
          s = fromIntegral (BS.index triple 1 .&. 0x3F)
          cylHigh = fromIntegral ((BS.index triple 1 .&. 0xC0) `shiftL` 2)
          cylLow = fromIntegral (BS.index triple 2)
          c = cylHigh + cylLow
       in (c, h, s)

leWord32 :: BS.ByteString -> Word32
leWord32 bs =
  foldl
    (\acc (i, b) -> acc + (fromIntegral b `shiftL` (8 * i)))
    (0 :: Word32)
    (zip [0 .. 3] (map (BS.index bs) [0 .. 3]))

leWord16 :: BS.ByteString -> Word16
leWord16 bs =
  foldl
    (\acc (i, b) -> acc + (fromIntegral b `shiftL` (8 * i)))
    (0 :: Word16)
    (zip [0 .. 1] (map (BS.index bs) [0 .. 1]))

parsePartitionEntry :: BS.ByteString -> Int -> Int -> Maybe PartitionEntry
parsePartitionEntry entry idx sectorBytes
  | BS.length entry /= partitionEntrySize = Nothing
  | all (== 0) (BS.unpack entry) = Nothing
  | otherwise =
      let status = BS.index entry 0
          startChs = decodeCHS (BS.take 3 (BS.drop 1 entry))
          typeCode = BS.index entry 4
          endChs = decodeCHS (BS.take 3 (BS.drop 5 entry))
          startLba = leWord32 (BS.take 4 (BS.drop 8 entry))
          sectors = leWord32 (BS.take 4 (BS.drop 12 entry))
       in Just
            PartitionEntry
              { partIndex = idx
              , partBootable = status == 0x80
              , partStatusRaw = status
              , partTypeCode = typeCode
              , partStartCHS = startChs
              , partEndCHS = endChs
              , partStartLBA = startLba
              , partSectors = sectors
              , partSectorBytes = sectorBytes
              }

parseMBR :: BS.ByteString -> Int -> Either String MBRParseResult
parseMBR raw sectorBytes
  | BS.length raw /= mbrSize = Left $ "MBR must be exactly " ++ show mbrSize ++ " bytes"
  | otherwise =
      let signature = leWord16 (BS.drop mbrSignatureOffset raw)
          signatureValid = signature == mbrSignature
          table = BS.take (partitionEntryCount * partitionEntrySize) (BS.drop partitionTableOffset raw)
          entries =
            [ parsePartitionEntry (BS.take partitionEntrySize (BS.drop (i * partitionEntrySize) table)) (i + 1) sectorBytes
            | i <- [0 .. partitionEntryCount - 1]
            ]
       in Right
            MBRParseResult
              { mbrSignatureValue = signature
              , mbrSignatureValid = signatureValid
              , mbrPartitions = catMaybes entries
              , mbrSectorBytes = sectorBytes
              }

--------------------------------------------------------------------------------
-- Rendering -------------------------------------------------------------------
--------------------------------------------------------------------------------

sizeBytes :: PartitionEntry -> Integer
sizeBytes part = fromIntegral (partSectors part) * fromIntegral (partSectorBytes part)

humanSize :: Integer -> String
humanSize n
  | n <= 0 = "0 B"
  | otherwise = go (fromIntegral n :: Double) units
  where
    units = ["B", "KiB", "MiB", "GiB", "TiB", "PiB"]
    go value (u : rest)
      | value < 1024 || null rest = printf "%.2f %s" value u
      | otherwise = go (value / 1024) rest
    go value [] = printf "%.2f TiB" value

formatHex :: (Integral a) => a -> String
formatHex v = "0x" ++ pad (map toLower (showHex v ""))
  where
    targetWidth
      | v < 0x100 = 2
      | v < 0x10000 = 4
      | otherwise = length (showHex v "")
    pad hexDigits = replicate (targetWidth - length hexDigits) '0' ++ hexDigits

showWithCommas :: (Integral a, Show a) => a -> String
showWithCommas n =
  let s = show n
      groups = reverse (chunk (reverse s))
   in intercalate "," (map reverse groups)
  where
    chunk [] = []
    chunk xs = take 3 xs : chunk (drop 3 xs)

printHuman :: Options -> MBRParseResult -> IO ()
printHuman opts result = do
  putStrLn $ "MBR Signature: " ++ formatHex (mbrSignatureValue result) ++ if mbrSignatureValid result then " (valid)" else " (INVALID)"
  let parts = mbrPartitions result
  if null parts && not (optShowEmpty opts)
    then putStrLn "No partitions present."
    else do
      putStrLn ""
      putStrLn "Partition Table:"
      if null parts && optShowEmpty opts
        then mapM_ (\i -> putStrLn ("  [Entry " ++ show i ++ "] <empty>")) [1 .. partitionEntryCount]
        else mapM_ printPartition parts
  where
    printPartition part = do
      let boot = if partBootable part then "BOOT " else "     "
          desc = typeDescription (partTypeCode part)
          typeCodeStr = formatHex (partTypeCode part)
      putStrLn $ "  [Entry " ++ show (partIndex part) ++ "] " ++ boot ++ "Type " ++ desc ++ " (" ++ typeCodeStr ++ ")"
      putStrLn $
        "      Start LBA: "
          ++ showWithCommas (partStartLBA part)
          ++ "  Sectors: "
          ++ showWithCommas (partSectors part)
          ++ "  Size: "
          ++ humanSize (sizeBytes part)
      let (cylS, headS, secS) = partStartCHS part
          (cylE, headE, secE) = partEndCHS part
      when (any (/= 0) [cylS, headS, secS, cylE, headE, secE]) $ do
        putStrLn $
          "      CHS: start (C="
            ++ show cylS
            ++ " H="
            ++ show headS
            ++ " S="
            ++ show secS
            ++ ") end (C="
            ++ show cylE
            ++ " H="
            ++ show headE
            ++ " S="
            ++ show secE
            ++ ")"

jsonString :: String -> String
jsonString s = "\"" ++ concatMap escape s ++ "\""
  where
    escape '"' = "\\\""
    escape '\\' = "\\\\"
    escape '\n' = "\\n"
    escape '\r' = "\\r"
    escape '\t' = "\\t"
    escape c = [c]

partitionToJson :: PartitionEntry -> [String]
partitionToJson part =
  [ "    {"
  , "      \"index\": " ++ show (partIndex part) ++ ","
  , "      \"bootable\": " ++ map toLower (show (partBootable part)) ++ ","
  , "      \"status_raw\": " ++ jsonString (formatHex (partStatusRaw part)) ++ ","
  , "      \"type_code\": " ++ jsonString (formatHex (partTypeCode part)) ++ ","
  , "      \"type_description\": " ++ jsonString (typeDescription (partTypeCode part)) ++ ","
  , "      \"start_chs\": {"
  , "        \"cylinder\": " ++ show (cyl (partStartCHS part)) ++ ","
  , "        \"head\": " ++ show (hed (partStartCHS part)) ++ ","
  , "        \"sector\": " ++ show (sec (partStartCHS part))
  , "      },"
  , "      \"end_chs\": {"
  , "        \"cylinder\": " ++ show (cyl (partEndCHS part)) ++ ","
  , "        \"head\": " ++ show (hed (partEndCHS part)) ++ ","
  , "        \"sector\": " ++ show (sec (partEndCHS part))
  , "      },"
  , "      \"start_lba\": " ++ show (partStartLBA part) ++ ","
  , "      \"sectors\": " ++ show (partSectors part) ++ ","
  , "      \"size_bytes\": " ++ show (sizeBytes part) ++ ","
  , "      \"size_human\": " ++ jsonString (humanSize (sizeBytes part))
  , "    }"
  ]
  where
    cyl (c, _, _) = c
    hed (_, h, _) = h
    sec (_, _, s) = s

resultToJson :: MBRParseResult -> String
resultToJson result =
  let header =
        [ "{"
        , "  \"signature\": " ++ show (mbrSignatureValue result) ++ ","
        , "  \"signature_valid\": " ++ map toLower (show (mbrSignatureValid result)) ++ ","
        , "  \"sector_bytes\": " ++ show (mbrSectorBytes result) ++ ","
        ]
      blocks = map partitionToJson (mbrPartitions result)
      partitionLines =
        case blocks of
          [] -> ["  \"partitions\": []"]
          _ ->
            ["  \"partitions\": ["]
              ++ concat
                ( zipWith
                    (\idx block -> (if idx > 0 then ["  ,"] else []) ++ block)
                    [0 :: Int ..]
                    blocks
                )
              ++ ["  ]"]
      footer = ["}"]
   in intercalate "\n" (header ++ partitionLines ++ footer)

--------------------------------------------------------------------------------
-- Dummy image -----------------------------------------------------------------
--------------------------------------------------------------------------------

createDummy :: Options -> FilePath -> IO ()
createDummy opts path = do
  exists <- doesFileExist path
  if exists && not (optForce opts)
    then do
      logMessage LogError opts $ "File '" ++ path ++ "' already exists (use --force to overwrite)"
      exitFailure
    else do
      logMessage LogInfo opts $ "Writing dummy MBR to '" ++ path ++ "'"
      let entry status typeCode startLba sectors =
            BS.concat
              [ BS.pack [status, 0, 0, 0, typeCode, 0, 0, 0]
              , le32 startLba
              , le32 sectors
              ]
          part1 = entry 0x80 0x0C 2048 1000000
          part2 = entry 0x00 0x83 1002048 4000000
          emptyEntries = BS.replicate (partitionEntrySize * 2) 0
          prefix = BS.replicate partitionTableOffset 0
          signatureBytes = BS.pack [lowByte mbrSignature, highByte mbrSignature]
          mbr = BS.concat [prefix, part1, part2, emptyEntries, signatureBytes]
      BS.writeFile path mbr
      logMessage LogInfo opts "Dummy image created"
  where
    lowByte :: Word16 -> Word8
    lowByte v = fromIntegral (v .&. 0x00FF)
    highByte :: Word16 -> Word8
    highByte v = fromIntegral ((v `shiftR` 8) .&. 0x00FF)
    le32 :: Word32 -> BS.ByteString
    le32 v =
      BS.pack
        [ fromIntegral (v .&. 0xFF)
        , fromIntegral ((v `shiftR` 8) .&. 0xFF)
        , fromIntegral ((v `shiftR` 16) .&. 0xFF)
        , fromIntegral ((v `shiftR` 24) .&. 0xFF)
        ]

--------------------------------------------------------------------------------
-- File IO ---------------------------------------------------------------------
--------------------------------------------------------------------------------

readMBR :: FilePath -> IO BS.ByteString
readMBR path = do
  raw <- BS.readFile path
  if BS.length raw < mbrSize
    then ioError (userError $ "File '" ++ path ++ "' shorter than " ++ show mbrSize ++ " bytes")
    else pure (BS.take mbrSize raw)

--------------------------------------------------------------------------------
-- Main ------------------------------------------------------------------------
--------------------------------------------------------------------------------

main :: IO ()
main = do
  argv <- getArgs
  optsBase <- parseOptions argv
  opts <-
    case optCreateDummy optsBase of
      Just target -> do
        createDummy optsBase target
        let newFile = Just (fromMaybe target (optFile optsBase))
        pure optsBase {optFile = newFile, optCreateDummy = Nothing}
      Nothing -> pure optsBase
  runWithDefault opts

runWithDefault :: Options -> IO ()
runWithDefault opts = do
  let path = fromMaybe "dummy_mbr.bin" (optFile opts)
  exists <- doesFileExist path
  unless exists $ do
    if path == "dummy_mbr.bin"
      then do
        logMessage LogInfo opts "Generating default dummy_mbr.bin"
        let forced = opts {optForce = True}
        createDummy forced path
      else do
        logMessage LogError opts $ "File not found: " ++ path
        exitFailure
  runWithFile opts path

runWithFile :: Options -> FilePath -> IO ()
runWithFile opts path = do
  logMessage LogInfo opts $ "Reading MBR from '" ++ path ++ "'"
  raw <- readMBR path
  case parseMBR raw (optSectorSize opts) of
    Left err -> do
      logMessage LogError opts err
      exitFailure
    Right result -> do
      logMessage LogDebug opts $ "Decoded " ++ show (length (mbrPartitions result)) ++ " partition entries"
      if optJson opts
        then putStrLn (resultToJson result)
        else printHuman opts result
      let exitCode = if mbrSignatureValid result then ExitSuccess else ExitFailure 2
      exitWith exitCode
