{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, try)
import Control.Monad (filterM, when)
import Data.Aeson (ToJSON (..), object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isSpace, toLower)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.List (isInfixOf, isPrefixOf)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Sequence (Seq (..), ViewL (..))
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import Network.HTTP.Client (Manager, responseTimeoutMicro)
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Simple
import Network.URI
import Options.Applicative
import System.IO (hPutStrLn, stderr)
import Text.HTML.TagSoup (Tag (..), parseTags)

-- | Command-line configuration
-- Rate limit expressed in seconds between requests.
data Config = Config
  { cfgStartUrl :: !String
  , cfgMaxDepth :: !Int
  , cfgMaxPages :: !Int
  , cfgSameDomain :: !Bool
  , cfgRateLimit :: !(Maybe NominalDiffTime)
  , cfgRobots :: !Bool
  , cfgJson :: !Bool
  , cfgEdgesPath :: !(Maybe FilePath)
  , cfgUserAgent :: !BS.ByteString
  , cfgTimeout :: !Int
  }
  deriving (Show)

normalizeConfig :: Config -> Either String Config
normalizeConfig cfg = do
  normalizedUrl <- case parseURI (cfgStartUrl cfg) of
    Just uri | uriScheme uri `elem` ["http:", "https:"] -> pure (cfgStartUrl cfg)
    Just _ -> Left "Unsupported URI scheme (only http/https allowed)"
    Nothing ->
      case parseURI ("http://" <> cfgStartUrl cfg) of
        Just uri' | uriScheme uri' `elem` ["http:", "https:"] -> pure (uriToString id uri' "")
        _ -> Left "Unable to parse start URL"
  when (cfgMaxDepth cfg < 0) $ Left "--depth must be >= 0"
  when (cfgMaxPages cfg < 1) $ Left "--max-pages must be >= 1"
  when (maybe False (< 0) (cfgRateLimit cfg)) $ Left "--rate must be >= 0"
  when (cfgTimeout cfg <= 0) $ Left "--timeout must be > 0"
  pure cfg{cfgStartUrl = normalizedUrl}

-- | Crawl result used for JSON reporting
-- Contains visited URLs, edges, and error messages.
data CrawlResult = CrawlResult
  { crVisited :: !(Set String)
  , crEdges :: ![(String, String)]
  , crErrors :: !(Map String String)
  }

data CrawlSummary = CrawlSummary
  { csStartUrl :: !String
  , csMaxDepth :: !Int
  , csMaxPages :: !Int
  , csVisitedCount :: !Int
  , csEdgesCount :: !Int
  , csSameDomain :: !Bool
  , csRateLimit :: !(Maybe NominalDiffTime)
  , csRobots :: !Bool
  , csErrors :: !(Map String String)
  }

instance ToJSON CrawlSummary where
  toJSON CrawlSummary{..} =
    object
      [ "start_url" .= csStartUrl
      , "max_depth" .= csMaxDepth
      , "max_pages" .= csMaxPages
      , "visited_count" .= csVisitedCount
      , "edges_count" .= csEdgesCount
      , "same_domain" .= csSameDomain
      , "rate_limit" .= fmap realToFrac csRateLimit
      , "robots" .= csRobots
      , "errors" .= Map.toList csErrors
      ]

-- | Minimal robots.txt representation.
data RobotsRules = RobotsRules
  { rrAllows :: [String]
  , rrDisallows :: [String]
  }
  deriving (Show)

parseRobots :: String -> RobotsRules
parseRobots raw = go (lines raw) False (RobotsRules [] [])
 where
  go [] _ rules = rules
  go (ln : rest) capturing rules =
    let line = takeWhile (/= '#') (dropWhile isSpace ln)
    in case break (== ':') line of
        (key, ':' : value) ->
          let lowerKey = map toLower key
              trimmed = trim value
              loweredValue = map toLower trimmed
           in case lowerKey of
                "user-agent" ->
                  let capture = loweredValue == "*" || "minicrawler" `isInfixOf` loweredValue
                   in go rest capture rules
                "allow" | capturing -> go rest capturing rules{rrAllows = rrAllows rules ++ [trimmed]}
                "disallow" | capturing -> go rest capturing rules{rrDisallows = rrDisallows rules ++ [trimmed]}
                _ -> go rest capturing rules
        _ -> go rest capturing rules

  trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

allowedByRobots :: RobotsRules -> String -> Bool
allowedByRobots rules path =
  let bestDis = longestMatch (rrDisallows rules)
      bestAllow = longestMatch (rrAllows rules)
  in case compare bestAllow bestDis of
      GT -> True
      EQ -> bestDis == 0
      LT -> bestDis == 0
 where
  longestMatch pats = maximum (0 : [length p | p <- pats, not (null p), p `isPrefixOf` path])

crawl :: Config -> IO CrawlResult
crawl Config{..} = do
  manager <- HTTP.newManager HTTP.defaultManagerSettings{HTTP.managerResponseTimeout = responseTimeoutMicro (cfgTimeout * 1000000)}
  let rootHost = fmap uriRegName (uriAuthority =<< parseURI cfgStartUrl)
      initialQueue = Seq.singleton (cfgStartUrl, 0)
  seenRef <- newIORef (Set.singleton cfgStartUrl)
  lastRequestRef <- newIORef Nothing
  robotsCache <- newIORef Map.empty
  go manager rootHost initialQueue Set.empty [] Map.empty 0 seenRef lastRequestRef robotsCache
 where
  go :: Manager -> Maybe String -> Seq (String, Int) -> Set String -> [(String, String)] -> Map String String -> Int -> IORef (Set String) -> IORef (Maybe UTCTime) -> IORef (Map String (Maybe RobotsRules)) -> IO CrawlResult
  go manager rootHost queue visited edges errors pages seenRef lastRequestRef robotsRef =
    case Seq.viewl queue of
      EmptyL -> finalize visited edges errors
      (url, depth) :< rest
        | depth > cfgMaxDepth -> go manager rootHost rest visited edges errors pages seenRef lastRequestRef robotsRef
        | Set.member url visited -> go manager rootHost rest visited edges errors pages seenRef lastRequestRef robotsRef
        | pages >= cfgMaxPages -> finalize visited edges errors
        | otherwise -> do
            enforceRate lastRequestRef cfgRateLimit
            fetchResult <- fetchPage manager lastRequestRef url
            case fetchResult of
              Left err ->
                go manager rootHost rest (Set.insert url visited) edges (Map.insert url err errors) (pages + 1) seenRef lastRequestRef robotsRef
              Right html -> do
                let links = extractLinks url html
                    edges' = foldr (\link acc -> (url, link) : acc) edges links
                allowedLinks <- filterM (acceptLink manager rootHost seenRef robotsRef) links
                let newQueue = rest <> Seq.fromList [(link, depth + 1) | link <- allowedLinks]
                modifyIORef' seenRef (\s -> foldr Set.insert s allowedLinks)
                go manager rootHost newQueue (Set.insert url visited) edges' errors (pages + 1) seenRef lastRequestRef robotsRef

  finalize visited edges errors = pure (CrawlResult visited (reverse edges) errors)

  enforceRate _ Nothing = pure ()
  enforceRate lastRef (Just limit) = do
    mLast <- readIORef lastRef
    case mLast of
      Nothing -> pure ()
      Just lastTime -> do
        now <- getCurrentTime
        let elapsed = diffUTCTime now lastTime
        when (elapsed < limit) $ do
          let remaining :: Double
              remaining = realToFrac (limit - elapsed)
          threadDelay (floor (remaining * 1000000))

  acceptLink :: Manager -> Maybe String -> IORef (Set String) -> IORef (Map String (Maybe RobotsRules)) -> String -> IO Bool
  acceptLink manager rootHost seenRef robotsRef link = do
    seen <- readIORef seenRef
    if Set.member link seen
      then pure False
      else case parseURI link of
        Nothing -> pure False
        Just uri -> do
          let schemeOk = uriScheme uri `elem` ["http:", "https:"]
              sameDomainOk = case (cfgSameDomain, rootHost, uriAuthority uri) of
                (False, _, _) -> True
                (True, Just root, Just auth) -> uriRegName auth == root
                (True, _, _) -> False
          if not (schemeOk && sameDomainOk)
            then pure False
            else do
              robotsAllowed <- if cfgRobots then allowedByRobotsFor manager uri robotsRef else pure True
              pure robotsAllowed

  allowedByRobotsFor :: Manager -> URI -> IORef (Map String (Maybe RobotsRules)) -> IO Bool
  allowedByRobotsFor manager uri robotsRef =
    case uriAuthority uri of
      Nothing -> pure True
      Just auth -> do
        cache <- readIORef robotsRef
        let host = uriRegName auth
        case Map.lookup host cache of
          Just rules -> pure (check rules)
          Nothing -> do
            rules <- fetchRobots manager cfgUserAgent cfgTimeout (uriScheme uri) host
            modifyIORef' robotsRef (Map.insert host rules)
            pure (check rules)
   where
    path = case uriPath uri of
      "" -> "/"
      p -> p
    check Nothing = True
    check (Just rules) = allowedByRobots rules path

  fetchPage :: Manager -> IORef (Maybe UTCTime) -> String -> IO (Either String String)
  fetchPage manager lastRef url = do
    request0 <- parseRequest url
    let request =
          setRequestResponseTimeout (responseTimeoutMicro (cfgTimeout * 1000000))
            . setRequestHeader "User-Agent" [cfgUserAgent]
            . setRequestManager manager
            $ request0
    result <- try (httpLBS request)
    now <- getCurrentTime
    writeIORef lastRef (Just now)
    case result of
      Left (e :: SomeException) -> pure (Left (show e))
      Right response ->
        let status = getResponseStatusCode response
            headers = map (map toLower . BS.unpack) (getResponseHeader "Content-Type" response)
            body = LBS.take (2 * 1024 * 1024) (getResponseBody response)
            htmlOk = any ("text/html" `isInfixOf`) headers
         in if status >= 200 && status < 300 && htmlOk
              then pure (Right (BS.unpack (LBS.toStrict body)))
              else pure (Left ("HttpStatus:" <> show status))

  fetchRobots :: Manager -> BS.ByteString -> Int -> String -> String -> IO (Maybe RobotsRules)
  fetchRobots manager ua timeoutSeconds scheme host = do
    let robotsUrl = scheme <> "//" <> host <> "/robots.txt"
    req0 <- parseRequest robotsUrl
    let request =
          setRequestResponseTimeout (responseTimeoutMicro (timeoutSeconds * 1000000))
            . setRequestHeader "User-Agent" [ua]
            . setRequestManager manager
            $ req0
    result <- try (httpLBS request)
    case result of
      Left (_ :: SomeException) -> pure Nothing
      Right response ->
        if getResponseStatusCode response >= 400
          then pure Nothing
          else pure . Just . parseRobots . BS.unpack . LBS.toStrict $ LBS.take 65536 (getResponseBody response)

  extractLinks :: String -> String -> [String]
  extractLinks base html =
    let tags = parseTags html
        baseUri = parseURI base
     in mapMaybe (resolveLink baseUri) [href | TagOpen "a" attrs <- tags, Just href <- [lookup "href" attrs]]

  resolveLink :: Maybe URI -> String -> Maybe String
  resolveLink Nothing _ = Nothing
  resolveLink (Just baseUri) href = do
    ref <- parseURIReference href
    let combined = ref `relativeTo` baseUri
        cleaned = combined{uriFragment = "", uriQuery = ""}
        final = uriToString id cleaned ""
    if uriScheme cleaned `elem` ["http:", "https:"] then Just final else Nothing

renderSummary :: Config -> CrawlResult -> CrawlSummary
renderSummary Config{..} CrawlResult{..} =
  CrawlSummary
    { csStartUrl = cfgStartUrl
    , csMaxDepth = cfgMaxDepth
    , csMaxPages = cfgMaxPages
    , csVisitedCount = Set.size crVisited
    , csEdgesCount = length crEdges
    , csSameDomain = cfgSameDomain
    , csRateLimit = cfgRateLimit
    , csRobots = cfgRobots
    , csErrors = crErrors
    }

configParser :: Parser Config
configParser =
  Config
    <$> argument str (metavar "URL" <> help "Starting URL (scheme optional)")
    <*> option auto (long "depth" <> short 'd' <> metavar "N" <> value 2 <> showDefault <> help "Maximum crawl depth")
    <*> option auto (long "max-pages" <> metavar "N" <> value 500 <> showDefault <> help "Maximum number of pages to fetch")
    <*> (not <$> switch (long "no-same-domain" <> help "Allow following external domains"))
    <*> optional (fmap (realToFrac :: Double -> NominalDiffTime) (option auto (long "rate" <> metavar "SECONDS" <> help "Seconds between requests")))
    <*> switch (long "robots" <> help "Respect robots.txt (best-effort)")
    <*> switch (long "json" <> help "Emit JSON summary to stdout")
    <*> optional (strOption (long "edges" <> metavar "PATH" <> help "Write discovered edges to file"))
    <*> fmap BS.pack (strOption (long "user-agent" <> metavar "UA" <> value "MiniCrawler/1.0 (Haskell)" <> showDefault <> help "Custom User-Agent"))
    <*> option auto (long "timeout" <> metavar "SECONDS" <> value 6 <> showDefault <> help "HTTP request timeout")

main :: IO ()
main = do
  cfgRaw <- execParser opts
  case normalizeConfig cfgRaw of
    Left err -> hPutStrLn stderr err
    Right cfg -> do
      result <- crawl cfg
      maybe (pure ()) (`writeEdges` crEdges result) (cfgEdgesPath cfg)
      if cfgJson cfg
        then LBS.putStrLn (Aeson.encode (renderSummary cfg result))
        else putStrLn $ "Visited " <> show (Set.size (crVisited result)) <> " pages; edges: " <> show (length (crEdges result)) <> "; errors: " <> show (Map.size (crErrors result))
 where
  opts = info (configParser <**> helper) (fullDesc <> progDesc "Breadth-first web crawler")

writeEdges :: FilePath -> [(String, String)] -> IO ()
writeEdges path es = writeFile path (unlines [src <> "\t" <> dst | (src, dst) <- es])
