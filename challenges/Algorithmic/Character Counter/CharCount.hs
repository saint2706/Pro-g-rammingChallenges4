{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Monad (foldM, when)
import Data.Char
    ( GeneralCategory(..)
    , generalCategory
    , isDigit
    , isLetter
    , isPunctuation
    , isSpace
    , ord
    , toLower
    , toUpper
    )
import Data.List (foldl', intercalate, sortOn)
import Data.Ord (Down(..))
import qualified Data.Map.Strict as M
import Numeric (showFFloat)
import System.Console.GetOpt
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hSetEncoding, stdin, stdout, utf8, stderr, hPutStrLn)
import Text.Printf (printf)

-- | Supported output formats.
data Format = FormatText | FormatJSON | FormatCSV deriving (Eq, Show)

-- | CLI options.
data Options = Options
    { optFile :: Maybe FilePath
    , optInlineText :: Maybe String
    , optCaseSensitive :: Bool
    , optTop :: Int
    , optFormat :: Format
    , optOutput :: Maybe FilePath
    , optShowHelp :: Bool
    }

-- | Default options.
defaultOptions :: Options
defaultOptions = Options
    { optFile = Nothing
    , optInlineText = Nothing
    , optCaseSensitive = True
    , optTop = 10
    , optFormat = FormatText
    , optOutput = Nothing
    , optShowHelp = False
    }

type OptSetter = Options -> Either String Options

-- | CLI option descriptors.
optionDescrs :: [OptDescr OptSetter]
optionDescrs =
    [ Option ['h'] ["help"] (NoArg setHelp) "Show this help message and exit"
    , Option ['f'] ["file"] (ReqArg setFile "PATH") "Read input from file"
    , Option ['t'] ["text"] (ReqArg setText "STRING") "Analyse the provided literal string"
    , Option ['c'] ["case-insensitive"] (NoArg setCaseInsensitive) "Convert input to lowercase before counting"
    , Option ['n'] ["top"] (ReqArg setTop "N") "Number of most common characters to display (default: 10)"
    , Option [] ["format"] (ReqArg setFormat "FORMAT") "Output format: text, json, csv"
    , Option ['o'] ["output"] (ReqArg setOutput "PATH") "Write results to PATH instead of stdout"
    , Option [] ["json"] (OptArg setJson "[PATH]") "Shortcut for --format json with optional output file"
    , Option [] ["csv"] (OptArg setCsv "[PATH]") "Shortcut for --format csv with optional output file"
    ]
  where
    setHelp opts = Right opts { optShowHelp = True }
    setFile path opts = Right opts { optFile = Just path }
    setText txt opts = Right opts { optInlineText = Just txt }
    setCaseInsensitive opts = Right opts { optCaseSensitive = False }
    setTop str opts =
        case reads str of
            [(n, "")] | n > 0 -> Right opts { optTop = n }
            _ -> Left $ "Invalid value for --top: " ++ str
    setFormat fmt opts =
        case parseFormat fmt of
            Just format -> Right opts { optFormat = format }
            Nothing -> Left $ "Unsupported format: " ++ fmt
    setOutput path opts = Right opts { optOutput = Just path }
    setJson mPath opts =
        let optsWithOutput = maybe opts (\p -> opts { optOutput = Just p }) mPath
         in Right optsWithOutput { optFormat = FormatJSON }
    setCsv mPath opts =
        let optsWithOutput = maybe opts (\p -> opts { optOutput = Just p }) mPath
         in Right optsWithOutput { optFormat = FormatCSV }

    parseFormat str =
        case map toLower str of
            "json" -> Just FormatJSON
            "csv" -> Just FormatCSV
            "text" -> Just FormatText
            _ -> Nothing

-- | Represents statistics calculated from the input text.
data Stats = Stats
    { statTotalChars :: Int
    , statUniqueChars :: Int
    , statCounts :: [(Char, Int)]
    , statMostCommon :: [(Char, Int)]
    , statTopRequested :: Int
    , statCategories :: M.Map String Int
    , statEntropy :: Double
    , statDiversity :: Double
    , statCaseSensitive :: Bool
    }

main :: IO ()
main = do
    hSetEncoding stdin utf8
    hSetEncoding stdout utf8
    progName <- getProgName
    argv <- getArgs
    opts <- case parseOptions argv of
        Left err -> do
            hPutStrLn stderr err
            usage progName
            exitFailure
        Right o -> pure o

    when (optShowHelp opts) $ do
        usage progName
        exitSuccess

    when (hasBoth (optFile opts) (optInlineText opts)) $ do
        hPutStrLn stderr "Please provide either --file or --text, not both."
        usage progName
        exitFailure

    input <- case (optInlineText opts, optFile opts) of
        (Just txt, _) -> pure txt
        (_, Just path) -> readFileUtf8 path
        _ -> getContentsUtf8

    let processed = if optCaseSensitive opts then input else map toLower input
        counts = countCharacters processed
        stats = computeStats input processed counts (optCaseSensitive opts) (optTop opts)
        output = case optFormat opts of
            FormatText -> formatText stats
            FormatJSON -> formatJSON stats
            FormatCSV -> formatCSV stats

    case optOutput opts of
        Just path -> writeFileUtf8 path output
        Nothing -> putStrLn output

-- | Parse command line options.
parseOptions :: [String] -> Either String Options
parseOptions argv =
    case getOpt Permute optionDescrs argv of
        (setters, nonOpts, []) -> do
            when (not (null nonOpts)) $ Left $ "Unexpected arguments: " ++ unwords nonOpts
            foldM (flip ($)) defaultOptions setters
        (_, _, errs) -> Left (concat errs)

usage :: String -> IO ()
usage progName = putStrLn $ usageInfo header optionDescrs
  where
    header = "Usage: " ++ progName ++ " [OPTIONS]\n\nAnalyse Unicode text from a file or standard input."

-- | Determine if both sources provided.
hasBoth :: Maybe a -> Maybe b -> Bool
hasBoth (Just _) (Just _) = True
hasBoth _ _ = False

-- | Read a UTF-8 encoded file.
readFileUtf8 :: FilePath -> IO String
readFileUtf8 path = do
    handle <- readFile path
    pure handle

-- | Read UTF-8 from stdin.
getContentsUtf8 :: IO String
getContentsUtf8 = getContents

-- | Write UTF-8 encoded file.
writeFileUtf8 :: FilePath -> String -> IO ()
writeFileUtf8 = writeFile

-- | Count occurrences of each character in the processed text.
countCharacters :: String -> M.Map Char Int
countCharacters = foldl' (\acc ch -> M.insertWith (+) ch 1 acc) M.empty

-- | Compute statistics for the analysis.
computeStats :: String -> String -> M.Map Char Int -> Bool -> Int -> Stats
computeStats original processed counts caseSensitive topN =
    let total = length processed
        uniqueCount = M.size counts
        sortedCounts = sortOn (Down . snd) $ M.toList counts
        mostCommon = take topN sortedCounts
        entropyVal = calculateEntropy processed counts total
        diversityVal = calculateDiversity counts total
        categories = M.filter (> 0) $ countCategories original
     in Stats
            { statTotalChars = total
            , statUniqueChars = uniqueCount
            , statCounts = sortedCounts
            , statMostCommon = mostCommon
            , statTopRequested = topN
            , statCategories = categories
            , statEntropy = entropyVal
            , statDiversity = diversityVal
            , statCaseSensitive = caseSensitive
            }

-- | Calculate Shannon entropy.
calculateEntropy :: String -> M.Map Char Int -> Int -> Double
calculateEntropy _ _ 0 = 0.0
calculateEntropy _ counts total =
    let total' = fromIntegral total
     in negate . sum $ [ p * logBase 2 p | count <- M.elems counts, let p = fromIntegral count / total', p > 0 ]

-- | Calculate Simpson's diversity index.
calculateDiversity :: M.Map Char Int -> Int -> Double
calculateDiversity _ total | total <= 1 = 0.0
calculateDiversity counts total =
    let numerator = sum [ fromIntegral count * fromIntegral (count - 1) | count <- M.elems counts ]
        denom = fromIntegral total * fromIntegral (total - 1)
     in 1 - (numerator / denom)

-- | Count characters by category using Unicode-aware heuristics.
countCategories :: String -> M.Map String Int
countCategories = foldl' update initial
  where
    initial = M.fromList $ zip categories (repeat 0)
    categories = ["letters", "digits", "punctuation", "whitespace", "symbols", "other"]
    update acc ch = M.adjust (+ 1) (categorise ch) acc

-- | Determine the category label for a character.
categorise :: Char -> String
categorise ch
    | isLetter ch = "letters"
    | isDigit ch = "digits"
    | isPunctuation ch = "punctuation"
    | isSpace ch = "whitespace"
    | isSymbolCategory (generalCategory ch) = "symbols"
    | otherwise = "other"
  where
    isSymbolCategory gc = gc `elem` [MathSymbol, CurrencySymbol, ModifierSymbol, OtherSymbol]

-- | Format statistics in a human-readable way.
formatText :: Stats -> String
formatText Stats {..}
    | statTotalChars == 0 = "No input provided."
    | otherwise =
        let header = [
                "=== TEXT ANALYSIS RESULTS ==="
              , printf "Total characters: %s" (formatInt statTotalChars)
              , printf "Unique characters: %s" (formatInt statUniqueChars)
              , printf "Shannon entropy: %.3f bits" statEntropy
              , printf "Diversity index: %.3f" statDiversity
              , "Case sensitive: " ++ show statCaseSensitive
              , ""
              , "Character categories:"
              ]
            categoryLines =
                [ printf "  %s: %s (%.1f%%)"
                    (capitalise name)
                    (formatInt count)
                    (percentage count)
                | (name, count) <- M.toList statCategories
                , count > 0
                ]
            topLines =
                ["", printf "Most common characters (top %d):" (min statTopRequested (length statMostCommon))]
                    ++ map formatEntry (takeWhilePositive statMostCommon)
         in intercalate "\n" (header ++ categoryLines ++ topLines)
  where
    percentage count = fromIntegral count / fromIntegral statTotalChars * 100
    takeWhilePositive = takeWhile ((> 0) . snd)
    formatEntry (ch, count) =
        let pct = percentage count
         in printf "  - %s: %s (%.1f%%)" (formatCharDisplay ch) (formatInt count) pct

-- | Convert integer to a string with thousands separators.
formatInt :: Int -> String
formatInt = reverse . intercalate "," . chunksOf 3 . reverse . show
  where
    chunksOf _ [] = []
    chunksOf n xs = let (a, b) = splitAt n xs in a : chunksOf n b

-- | Capitalise the first letter.
capitalise :: String -> String
capitalise [] = []
capitalise (x:xs) = toUpper x : map toLower xs

-- | Display characters in a friendly form.
formatCharDisplay :: Char -> String
formatCharDisplay ' ' = "' ' (Space)"
formatCharDisplay '\\n' = "'\\n' (Newline)"
formatCharDisplay '\\t' = "'\\t' (Tab)"
formatCharDisplay '\\r' = "'\\r' (Carriage Return)"
formatCharDisplay ch
    | ord ch < 32 || ord ch == 127 = printf "'\\x%02x' (Control)" (ord ch)
    | otherwise = [ch]

-- | Render statistics as JSON.
formatJSON :: Stats -> String
formatJSON Stats {..} =
    let pairs =
            [ jsonKV "total_characters" (show statTotalChars)
            , jsonKV "unique_characters" (show statUniqueChars)
            , jsonKV "character_counts" (jsonObject (map countPair statCounts))
            , jsonKV "character_categories" (jsonObject (map categoryPair (M.toList statCategories)))
            , jsonKV "entropy" (jsonFloat statEntropy)
            , jsonKV "diversity_index" (jsonFloat statDiversity)
            , jsonKV "most_common" (jsonArray (map mostCommonPair statMostCommon))
            , jsonKV "case_sensitive" (if statCaseSensitive then "true" else "false")
            ]
     in "{\n" ++ intercalate ",\n" (map ("  " ++) pairs) ++ "\n}"
  where
    countPair (ch, count) = ([ch], show count)
    categoryPair (name, count) = (name, show count)
    mostCommonPair (ch, count) =
        "[" ++ jsonString [ch] ++ ", " ++ show count ++ "]"

-- | Render statistics as CSV.
formatCSV :: Stats -> String
formatCSV Stats {..}
    | statTotalChars == 0 = "Character,Count,Percentage"
    | otherwise =
        let header = "Character,Count,Percentage"
            rows =
                [ intercalate "," [csvCell [ch], show count, printf "%.2f" (pct count)]
                | (ch, count) <- statCounts
                ]
         in intercalate "\n" (header : rows)
  where
    pct count = fromIntegral count / fromIntegral statTotalChars * 100

-- | Helper for JSON key/value pair.
jsonKV :: String -> String -> String
jsonKV key value = jsonString key ++ ": " ++ value

-- | Render a JSON object from key/value pairs.
jsonObject :: [(String, String)] -> String
jsonObject [] = "{}"
jsonObject kvs =
    "{\n" ++ intercalate ",\n" (map render kvs) ++ "\n  }"
  where
    render (k, v) = "  " ++ jsonString k ++ ": " ++ v

jsonArray :: [String] -> String
jsonArray [] = "[]"
jsonArray xs = "[" ++ intercalate ", " xs ++ "]"

jsonString :: String -> String
jsonString s = "\"" ++ concatMap escape s ++ "\""
  where
    escape '"' = "\\\""
    escape '\\' = "\\\\"
    escape '\n' = "\\n"
    escape '\r' = "\\r"
    escape '\t' = "\\t"
    escape c
        | ord c < 0x20 = printf "\\u%04x" (ord c)
        | otherwise = [c]

jsonFloat :: Double -> String
jsonFloat x =
    let rendered = showFFloat (Just 6) x ""
     in if '.' `elem` rendered then rendered else rendered ++ ".0"

-- | Format CSV cells with proper escaping.
csvCell :: String -> String
csvCell s
    | needsQuotes = '"' : escaped ++ "\""
    | otherwise = s
  where
    escaped = concatMap escape s
    needsQuotes = any (`elem` ",\"\n\r") s
    escape '"' = """""
    escape c = [c]
