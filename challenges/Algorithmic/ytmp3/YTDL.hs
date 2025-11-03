{-# LANGUAGE DeriveGeneric #-}

-- | Minimal yt-dlp powered audio downloader in Haskell.
-- Mirrors the functionality of challenges/Algorithmic/ytmp3/cringe.py.
module Main (main) where

import Control.Exception (SomeException, try)
import Control.Monad (foldM, unless, when)
import Data.Aeson (ToJSON, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Char (isSpace, toLower)
import Data.List (dropWhileEnd, isSuffixOf)
import GHC.Generics (Generic)
import System.Console.GetOpt
import System.Directory (createDirectoryIfMissing, doesFileExist, findExecutable, makeAbsolute)
import System.Environment (getArgs, getProgName)
import System.Exit (ExitCode (..), exitFailure, exitSuccess)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import System.Process (readProcessWithExitCode)

-- | Command-line options for the downloader.
data Options = Options
  { optFormat :: String
  , optQuality :: String
  , optOutDir :: FilePath
  , optInput :: Maybe FilePath
  , optJSON :: Bool
  , optQuiet :: Bool
  }

-- | Defaults mirror the Python script.
defaultOptions :: Options
defaultOptions = Options
  { optFormat = "mp3"
  , optQuality = "192"
  , optOutDir = "."
  , optInput = Nothing
  , optJSON = False
  , optQuiet = False
  }

-- | Supported formats to validate user input.
validFormats :: [String]
validFormats = ["mp3", "m4a", "opus", "vorbis", "wav", "flac"]

-- | Representation of a per-URL download result.
data ItemResult = ItemResult
  { itemUrl :: String
  , itemSuccess :: Bool
  , itemError :: Maybe String
  }
  deriving (Generic, Show)

instance ToJSON ItemResult where
  toJSON (ItemResult url ok errMsg) =
    Aeson.object
      [ "url" .= url
      , "success" .= ok
      , "error" .= errMsg
      ]

-- | Summary structure emitted as JSON (compatible with cringe.py).
data Summary = Summary
  { sumBackend :: String
  , sumFormat :: String
  , sumQuality :: String
  , sumOutDir :: FilePath
  , sumTotal :: Int
  , sumSuccess :: Int
  , sumFailed :: Int
  , sumItems :: [ItemResult]
  }
  deriving (Generic, Show)

instance ToJSON Summary where
  toJSON (Summary backend fmt q dir total ok failed items) =
    Aeson.object
      [ "backend" .= backend
      , "format" .= fmt
      , "quality" .= q
      , "out_dir" .= dir
      , "total" .= total
      , "success" .= ok
      , "failed" .= failed
      , "items" .= items
      ]

-- | Modify options via GetOpt descriptors.
optionDescriptors :: [OptDescr (Options -> IO Options)]
optionDescriptors =
  [ Option ['f', 'F'] ["format"] (ReqArg setFormat "FORMAT")
      "Output audio format (mp3,m4a,opus,vorbis,wav,flac)"
  , Option ['q'] ["quality", "bitrate"] (ReqArg setQuality "KBPS")
      "Audio quality/bitrate target (e.g. 192)"
  , Option ['o'] ["out-dir"] (ReqArg setOutDir "PATH")
      "Directory where audio files are written"
  , Option ['i'] ["input"] (ReqArg setInput "FILE")
      "File containing URLs (one per line; # comments ignored)"
  , Option [] ["json"] (NoArg setJSON)
      "Emit JSON summary to stdout"
  , Option [] ["quiet"] (NoArg setQuiet)
      "Suppress progress lines (errors still reported in JSON)"
  ]
  where
    setFormat arg opts = pure opts {optFormat = map toLower arg}
    setQuality arg opts = pure opts {optQuality = arg}
    setOutDir arg opts = pure opts {optOutDir = arg}
    setInput arg opts = pure opts {optInput = Just arg}
    setJSON opts = pure opts {optJSON = True}
    setQuiet opts = pure opts {optQuiet = True}

-- | Simple whitespace trimming helper.
trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace

-- | Reads URLs from a file, ignoring blank lines and comments.
readUrlFile :: FilePath -> IO [String]
readUrlFile path = do
  exists <- doesFileExist path
  unless exists $ dieWith $ "URL file not found: " ++ path
  contents <- readFile path
  let urls =
        [ trimmed
        | raw <- lines contents
        , let trimmed = trim raw
        , not (null trimmed)
        , not ("#" `isPrefixOf` trimmed)
        ]
  pure urls
  where
    isPrefixOf prefix str = prefix == take (length prefix) str

-- | Identify the download backend (prefers yt-dlp, fallback to youtube-dl).
detectBackend :: IO String
detectBackend = do
  yt <- findExecutable "yt-dlp"
  case yt of
    Just path -> pure path
    Nothing -> do
      ydl <- findExecutable "youtube-dl"
      case ydl of
        Just path -> pure path
        Nothing -> dieWith "Neither 'yt-dlp' nor 'youtube-dl' is available in PATH."

-- | Execute yt-dlp/youtube-dl for a single URL and capture success/error state.
downloadOne :: String -> Options -> FilePath -> String -> IO ItemResult
downloadOne backend opts outTemplate url = do
  when (not (optQuiet opts)) $ putStrLn $ "Downloading: " ++ url
  let baseArgs =
        [ "-f", "bestaudio/best"
        , "--extract-audio"
        , "--audio-format", optFormat opts
        , "--audio-quality", optQuality opts
        , "--output", outTemplate
        , "--no-playlist"
        , "--no-check-certificate"
        ]
      quietArgs =
        if optQuiet opts then ["--quiet", "--no-progress"] else []
      args = baseArgs ++ quietArgs ++ [url]
  result <- try $ readProcessWithExitCode backend args ""
  case result of
    Left (err :: SomeException) -> pure $ ItemResult url False (Just (show err))
    Right (exitCode, _stdoutTxt, stderrTxt) ->
      case exitCode of
        ExitSuccess -> pure $ ItemResult url True Nothing
        ExitFailure _ -> do
          when (not (optQuiet opts) && not (null (trim stderrTxt))) $
            hPutStrLn stderr $ "  Failed: " ++ trim stderrTxt
          let errMsg = if null (trim stderrTxt) then Just "Download failed" else Just (trim stderrTxt)
          pure $ ItemResult url False errMsg

-- | Utility to abort with a message printed to stderr.
dieWith :: String -> IO a
dieWith msg = do
  hPutStrLn stderr msg
  exitFailure

-- | Parse command line arguments and run downloads.
main :: IO ()
main = do
  argv <- getArgs
  progName <- getProgName
  let (actions, urls, errs) = getOpt Permute optionDescriptors argv
  unless (null errs) $ do
    mapM_ (hPutStrLn stderr) errs
    hPutStrLn stderr $ usageInfo ("Usage: " ++ progName ++ " [OPTIONS] URL [URL ...]") optionDescriptors
    exitFailure
  opts <- foldM (\acc action -> action acc) defaultOptions actions
  let fmt = map toLower (optFormat opts)
  unless (fmt `elem` validFormats) $
    dieWith $ "Unsupported format (choose one of: " ++ unwords validFormats ++ ")"
  let opts' = opts {optFormat = fmt}
  extraUrls <- case optInput opts' of
    Nothing -> pure []
    Just path -> readUrlFile path
  let allUrls = urls ++ extraUrls
  when (null allUrls) $ dieWith "Provide at least one URL or --input file"
  createDirectoryIfMissing True (optOutDir opts')
  outDirAbs <- makeAbsolute (optOutDir opts')
  backendPath <- detectBackend
  let outTemplate = outDirAbs </> "%(title)s.%(ext)s"
  items <- mapM (downloadOne backendPath opts' outTemplate) allUrls
  let successCount = length (filter itemSuccess items)
      totalCount = length items
      summary =
        Summary
          { sumBackend = backendLabel backendPath
          , sumFormat = optFormat opts'
          , sumQuality = optQuality opts'
          , sumOutDir = outDirAbs
          , sumTotal = totalCount
          , sumSuccess = successCount
          , sumFailed = totalCount - successCount
          , sumItems = items
          }
  if optJSON opts'
    then BL8.putStrLn (Aeson.encode summary)
    else putStrLn $ "Completed: " ++ show successCount ++ "/" ++ show totalCount ++ " succeeded (backend=" ++ backendLabel backendPath ++ ")"
  if successCount == totalCount
    then exitSuccess
    else exitFailure
  where
    backendLabel path
      | "yt-dlp" `isSuffixOf` path = "yt-dlp"
      | "youtube-dl" `isSuffixOf` path = "youtube-dl"
      | otherwise = path

