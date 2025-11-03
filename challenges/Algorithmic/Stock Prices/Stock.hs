{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception (IOException, try)
import Control.Monad (unless, when)
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Csv as Csv
import Data.Foldable (asum)
import Data.List (mapAccumL)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (catMaybes, fromMaybe, isJust)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Data.Time.Calendar (Day)
import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Network.HTTP.Simple
  ( HttpException
  , Request
  , Response
  , getResponseBody
  , httpLBS
  , parseRequest
  )
import Options.Applicative
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Text.Read (readMaybe)

-------------------------------------------------------------------------------
-- Configuration & CLI -------------------------------------------------------
-------------------------------------------------------------------------------

data Config = Config
  { cfgSource :: !(Maybe FilePath)
  , cfgUrl :: !(Maybe String)
  , cfgSma :: ![Int]
  , cfgEma :: ![Int]
  , cfgReturns :: !Bool
  , cfgJson :: !Bool
  , cfgHtml :: !(Maybe FilePath)
  , cfgNoShow :: !Bool
  , cfgTicker :: !(Maybe String)
  }
  deriving (Show, Generic)

instance Aeson.ToJSON Config

configParser :: Parser Config
configParser =
  Config
    <$> optional
      ( strOption
          ( long "source"
              <> metavar "CSV"
              <> help "Local CSV file path"
          )
      )
    <*> optional
      ( strOption
          ( long "url"
              <> metavar "URL"
              <> help "Remote CSV URL"
          )
      )
    <*> many
      ( option auto
          ( long "sma"
              <> metavar "WINDOW"
              <> help "Simple moving average window size (repeat flag for multiple)"
          )
      )
    <*> many
      ( option auto
          ( long "ema"
              <> metavar "WINDOW"
              <> help "Exponential moving average window size (repeat flag for multiple)"
          )
      )
    <*> switch
      ( long "returns"
          <> help "Include daily percent returns"
      )
    <*> switch
      ( long "json"
          <> help "Emit JSON summary"
      )
    <*> optional
      ( strOption
          ( long "html"
              <> metavar "FILE"
              <> help "Export chart to an HTML file"
          )
      )
    <*> switch
      ( long "no-show"
          <> help "Suppress textual output (headless mode)"
      )
    <*> optional
      ( strOption
          ( long "ticker"
              <> metavar "SYMBOL"
              <> help "Ticker symbol for chart title"
          )
      )

validateConfig :: Config -> Either String Config
validateConfig cfg
  | not (xor (isJust (cfgSource cfg)) (isJust (cfgUrl cfg))) =
      Left "Provide exactly one data source: --source or --url"
  | otherwise = do
      mapM_ (ensurePositive "SMA") (cfgSma cfg)
      mapM_ (ensurePositive "EMA") (cfgEma cfg)
      pure cfg
  where
    xor a b = (a || b) && not (a && b)
    ensurePositive label n
      | n <= 0 = Left (label <> " window must be positive")
      | otherwise = Right ()

-------------------------------------------------------------------------------
-- Data model ----------------------------------------------------------------
-------------------------------------------------------------------------------

data StockRow = StockRow
  { srDate :: !T.Text
  , srOpen :: !(Maybe Double)
  , srHigh :: !(Maybe Double)
  , srLow :: !(Maybe Double)
  , srClose :: !(Maybe Double)
  , srVolume :: !(Maybe Double)
  }
  deriving (Show)

type IndicatorSeries = HM.HashMap T.Text (Vector (Maybe Double))

-------------------------------------------------------------------------------
-- Loading helpers -----------------------------------------------------------
-------------------------------------------------------------------------------

loadBytes :: Config -> IO (Either String BL.ByteString)
loadBytes cfg =
  case cfgSource cfg of
    Just fp -> do
      result <- try (BL.readFile fp) :: IO (Either IOException BL.ByteString)
      pure $ either (Left . show) Right result
    Nothing ->
      case cfgUrl cfg of
        Nothing -> pure $ Left "Missing data source"
        Just u -> do
          reqResult <- try (parseRequest u) :: IO (Either HttpException Request)
          case reqResult of
            Left e -> pure $ Left (show e)
            Right req -> do
              httpResult <- try (httpLBS req) :: IO (Either HttpException (Response BL.ByteString))
              pure $ fmap getResponseBody (either (Left . show) Right httpResult)

-------------------------------------------------------------------------------
-- CSV decoding --------------------------------------------------------------
-------------------------------------------------------------------------------

normalizeName :: ByteString -> T.Text
normalizeName bs =
  case T.splitOn "." (TE.decodeUtf8 bs) of
    [] -> TE.decodeUtf8 bs
    parts -> last parts

buildHeaderMap :: Csv.Header -> HM.HashMap T.Text ByteString
buildHeaderMap header =
  HM.fromListWith keepFirst [(normalizeName h, h) | h <- V.toList header]
  where
    keepFirst old _ = old

lookupField :: HM.HashMap T.Text ByteString -> Csv.NamedRecord -> T.Text -> Maybe ByteString
lookupField mapping record key = do
  original <- HM.lookup key mapping
  HM.lookup original record

parseMaybeDouble :: ByteString -> Maybe Double
parseMaybeDouble raw =
  let text = T.strip (TE.decodeUtf8 raw)
   in if T.null text then Nothing else readMaybe (T.unpack text)

parseDate :: ByteString -> Either String T.Text
parseDate raw =
  let text = T.strip (TE.decodeUtf8 raw)
   in if T.null text
        then Left "Encountered blank Date cell"
        else
          let formats = ["%Y-%m-%d", "%m/%d/%Y", "%Y/%m/%d", "%d-%m-%Y"]
              parsed = asum (map (`parseFormat` text) formats)
           in Right $ maybe text id parsed
  where
    parseFormat fmt txt =
      fmap (T.pack . formatTime defaultTimeLocale "%Y-%m-%d")
        (parseTimeM True defaultTimeLocale fmt (T.unpack txt) :: Maybe Day)

decodeStockRows :: BL.ByteString -> Either String (Vector StockRow)
decodeStockRows bytes = do
  (header, rows) <- Csv.decodeByName bytes
  let mapping = buildHeaderMap header
  if HM.member "Date" mapping
    then traverse (decodeRow mapping) rows
    else Left "CSV missing Date column"
  where
    decodeRow mapping record = do
      dateBytes <- maybe (Left "CSV missing Date column") Right (lookupField mapping record "Date")
      dateText <- parseDate dateBytes
      let lookupD name = lookupField mapping record name >>= parseMaybeDouble
      pure
        StockRow
          { srDate = dateText
          , srOpen = lookupD "Open"
          , srHigh = lookupD "High"
          , srLow = lookupD "Low"
          , srClose = lookupD "Close"
          , srVolume = lookupD "Volume"
          }

-------------------------------------------------------------------------------
-- Indicator calculations ----------------------------------------------------
-------------------------------------------------------------------------------

seriesFromRows :: (StockRow -> Maybe Double) -> Vector StockRow -> Vector (Maybe Double)
seriesFromRows f = V.map f

calcSma :: Int -> Vector (Maybe Double) -> Vector (Maybe Double)
calcSma window values
  | window <= 0 = V.replicate (V.length values) Nothing
  | otherwise =
      let doubles = V.map (fromMaybe 0) values
          counts = V.map (maybe 0 (const 1 :: Double)) values
          prefixSum = V.scanl' (+) 0 doubles
          prefixCount = V.scanl' (+) 0 counts
          len = V.length values
       in V.generate len $ \i ->
            let endIdx = i + 1
                startIdx = max 0 (endIdx - window)
                total = (prefixSum V.! endIdx) - (prefixSum V.! startIdx)
                count = (prefixCount V.! endIdx) - (prefixCount V.! startIdx)
             in if count <= 0 then Nothing else Just (total / count)

calcEma :: Int -> Vector (Maybe Double) -> Vector (Maybe Double)
calcEma window values
  | window <= 0 = V.replicate (V.length values) Nothing
  | otherwise =
      let alpha = 2 / (fromIntegral window + 1)
          step Nothing Nothing = (Nothing, Nothing)
          step Nothing (Just x) = (Just x, Just x)
          step (Just prev) Nothing = (Just prev, Just prev)
          step (Just prev) (Just x) =
            let ema = alpha * x + (1 - alpha) * prev
             in (Just ema, Just ema)
          (_, result) = mapAccumL step Nothing (V.toList values)
       in V.fromList result

calcReturns :: Vector (Maybe Double) -> Vector (Maybe Double)
calcReturns values =
  let step Nothing Nothing = (Nothing, Nothing)
      step Nothing (Just x) = (Just x, Just 0)
      step (Just prev) Nothing = (Just prev, Nothing)
      step (Just prev) (Just x)
        | prev == 0 = (Just x, Nothing)
        | otherwise =
            let pct = (x - prev) / prev
             in (Just x, Just pct)
      (_, result) = mapAccumL step Nothing (V.toList values)
   in V.fromList result

buildIndicators :: Config -> Vector StockRow -> IndicatorSeries
buildIndicators cfg rows =
  let closeSeries = seriesFromRows srClose rows
      smaSeries =
        HM.fromList
          [ (T.pack ("SMA_" ++ show window), calcSma window closeSeries)
            | window <- cfgSma cfg
          ]
      emaSeries =
        HM.fromList
          [ (T.pack ("EMA_" ++ show window), calcEma window closeSeries)
            | window <- cfgEma cfg
          ]
      returnsSeries =
        if cfgReturns cfg
          then HM.singleton "Returns" (calcReturns closeSeries)
          else HM.empty
   in smaSeries <> emaSeries <> returnsSeries

-------------------------------------------------------------------------------
-- Summaries -----------------------------------------------------------------
-------------------------------------------------------------------------------

statsPair :: T.Text -> [Maybe Double] -> Maybe Aeson.Pair
statsPair label values =
  let xs = catMaybes values
   in if null xs
        then Nothing
        else
          let minVal = minimum xs
              maxVal = maximum xs
              meanVal = sum xs / fromIntegral (length xs)
           in Just (label .= Aeson.object ["min" .= minVal, "max" .= maxVal, "mean" .= meanVal])

buildSummary :: Vector StockRow -> IndicatorSeries -> Aeson.Value
buildSummary rows indicators =
  let rowsList = V.toList rows
      basePairs =
        catMaybes
          [ statsPair "Open" (map srOpen rowsList)
          , statsPair "High" (map srHigh rowsList)
          , statsPair "Low" (map srLow rowsList)
          , statsPair "Close" (map srClose rowsList)
          ]
      returnsPair =
        case HM.lookup "Returns" indicators of
          Nothing -> []
          Just vec -> maybeToList (statsPair "Returns" (V.toList vec))
   in Aeson.object ("rows" .= V.length rows : basePairs <> returnsPair)

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

-------------------------------------------------------------------------------
-- HTML export ---------------------------------------------------------------
-------------------------------------------------------------------------------

renderHtml :: Config -> Vector StockRow -> IndicatorSeries -> T.Text
renderHtml cfg rows indicators =
  let rowsList = V.toList rows
      baseSeriesPairs =
        catMaybes
          [ seriesPair "Open" srOpen
          , seriesPair "High" srHigh
          , seriesPair "Low" srLow
          , seriesPair "Close" srClose
          , seriesPair "Volume" srVolume
          ]
      payload =
        Aeson.object
          [ "dates" .= map srDate rowsList
          , "series" .= Aeson.object baseSeriesPairs
          , "indicators"
              .= Aeson.object
                [ name .= V.toList vec
                  | (name, vec) <- HM.toList indicators
                ]
          , "ticker" .= cfgTicker cfg
          ]
      seriesPair name accessor =
        let vals = map accessor rowsList
         in if any isJust vals then Just (name .= vals) else Nothing
      payloadText = TE.decodeUtf8 (BL.toStrict (Aeson.encode payload))
      titleText = maybe "Stock Data" T.pack (cfgTicker cfg)
      styleBlock = T.concat
        [ "body{font-family:system-ui,sans-serif;margin:0;padding:1rem;}"
        , "#chart{width:100%;height:80vh;}"
        , "table{border-collapse:collapse;margin-top:1rem;}"
        , "th,td{padding:0.4rem 0.8rem;border:1px solid #ccc;text-align:right;}"
        , "th:first-child,td:first-child{text-align:left;}"
        ]
   in T.concat
        [ "<!DOCTYPE html><html><head><meta charset=\"utf-8\"/>"
        , "<title>", titleText, "</title>"
        , "<script src=\"https://cdn.plot.ly/plotly-2.26.0.min.js\"></script>"
        , "<style>", styleBlock, "</style>"
        , "</head><body>"
        , "<h1>", titleText, "</h1><div id=\"chart\"></div>"
        , "<script>const payload = ", payloadText, ";"
        , "const traces=[];const baseSeries=payload.series;"
        , "for (const [name, values] of Object.entries(baseSeries)) {"
        , "const trace={name,x:payload.dates,y:values,type:name==='Volume'?'bar':'scatter'};"
        , "if (name==='Volume') trace.yaxis='y2';traces.push(trace);}" 
        , "for (const [name, values] of Object.entries(payload.indicators)) {"
        , "traces.push({name,x:payload.dates,y:values,type:'scatter'});}" 
        , "const layout={title:payload.ticker||'Stock Data',xaxis:{title:'Date'},"
        , "yaxis:{title:'Price'},yaxis2:{title:'Volume',overlaying:'y',side:'right',showgrid:false}};"
        , "Plotly.newPlot('chart',traces,layout);"
        , "</script></body></html>"
        ]

-------------------------------------------------------------------------------
-- Execution -----------------------------------------------------------------
-------------------------------------------------------------------------------

runProgram :: Config -> IO (Either String (Vector StockRow, IndicatorSeries))
runProgram cfg = do
  bytesResult <- loadBytes cfg
  case bytesResult of
    Left err -> pure $ Left err
    Right bytes ->
      case decodeStockRows bytes of
        Left err -> pure $ Left err
        Right rows ->
          let indicators = buildIndicators cfg rows
           in pure $ Right (rows, indicators)

outputJson :: Config -> Vector StockRow -> IndicatorSeries -> IO ()
outputJson cfg rows indicators = do
  let summaryVal = buildSummary rows indicators
      payload = Aeson.object ["summary" .= summaryVal, "config" .= cfg]
  BL8.putStrLn (Aeson.encode payload)

writeHtml :: FilePath -> T.Text -> IO (Either String ())
writeHtml path content = do
  result <- try (TIO.writeFile path content) :: IO (Either IOException ())
  pure $ either (Left . show) Right result

main :: IO ()
main = do
  cfgRaw <- execParser opts
  case validateConfig cfgRaw of
    Left err -> hPutStrLn stderr ("Error: " <> err) >> exitFailure
    Right cfg -> do
      result <- runProgram cfg
      case result of
        Left err -> hPutStrLn stderr ("Error: " <> err) >> exitFailure
        Right (rows, indicators) -> do
          when (cfgJson cfg) (outputJson cfg rows indicators)
          case cfgHtml cfg of
            Nothing -> pure ()
            Just fp -> do
              let htmlText = renderHtml cfg rows indicators
              htmlResult <- writeHtml fp htmlText
              case htmlResult of
                Left err -> hPutStrLn stderr ("Error writing HTML: " <> err) >> exitFailure
                Right _ -> unless (cfgNoShow cfg) $ putStrLn ("Chart exported to " <> fp)
          unless (cfgJson cfg || cfgNoShow cfg) $ do
            let summaryVal = buildSummary rows indicators
            BL8.putStrLn (Aeson.encode summaryVal)

  where
    opts = info (configParser <**> helper) (fullDesc <> progDesc "Stock indicator helper")
