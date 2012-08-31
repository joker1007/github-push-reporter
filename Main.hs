{-# LANGUAGE OverloadedStrings,FlexibleInstances,QuasiQuotes #-}
module Main where

import Prelude hiding(catch)
import Control.Applicative ((<$>))
import Control.Monad (mzero,liftM)
import Control.Exception
import Data.Monoid (mappend)
import Data.Text
import Data.Time
import System.Locale (defaultTimeLocale)
import Network.HTTP.Conduit
import Data.Maybe
import qualified Data.Vector as DV
import Data.Aeson
import Data.Aeson.Types (Parser)
import qualified Data.Aeson.Types as DAT
import qualified Data.ByteString.Char8 as BC
import Text.Regex
import Text.Pandoc
import Text.Pandoc.Builder
import Text.Hamlet
import Text.Blaze.Html.Renderer.String

data Commit = Commit {
                name :: Text,
                email :: Text,
                comment :: Text
                } deriving (Eq,Show)

data Event = PushEvent { commits :: DV.Vector Commit, createdAt :: UTCTime } deriving (Show,Eq)

instance FromJSON Commit where
  parseJSON (Object o) = do author <- o .: "author"
                            n <- author .: "name"
                            e <- author .: "email"
                            c <- o .: "message"
                            return $ Commit n e c
  parseJSON _ = mzero

instance FromJSON Event where
  parseJSON (Object o) = do eventType <- o .: "type"
                            parseEvent eventType
    where
      parseEvent :: String -> Parser Event
      parseEvent "PushEvent" = do payload <- o .: "payload"
                                  cs <- payload .: "commits"
                                  vcs <- parseJSON cs :: Parser (DV.Vector Commit)
                                  Just t <- liftM (parseTime defaultTimeLocale "%FT%TZ") $ o .: "created_at"
                                  return $ PushEvent vcs t
      parseEvent _ = mzero

  parseJSON _ = mzero

instance FromJSON (DV.Vector Commit) where
  parseJSON (Array a) = DV.mapM parseJSON a
  parseJSON _ = mzero

instance FromJSON (DV.Vector Event) where
  parseJSON (Array a) = DV.mapM parseJSON a
  parseJSON _ = mzero

data BasicAuth = BasicAuth {login :: String, password :: String} deriving Show

main :: IO()
main = do
  repos <- getRepos
  auth <- getAuth
  bss <- mapM (flip fetch auth) repos
  bs <- return $ Prelude.foldl1 (\acc bs -> acc <> bs) bss
  putStrLn $ writeHtmlString defaultWriterOptions {writerStandalone = True, writerTemplate = template} $ doc $ bs
  return ()
  where template = renderHtml [shamlet| $newline always
                              $doctype 5
                              <html>
                                <head>
                                  <link rel="stylesheet" href="http://netdna.bootstrapcdn.com/twitter-bootstrap/2.1.0/css/bootstrap-combined.min.css" />
                                  <title>Github Push Report
                                <body>
                                  <div.container-fluid>
                                    \$body$
                            |]

getRepos :: IO [String]
getRepos = liftM Prelude.lines $ readFile "repositories"

getAuth :: IO (Maybe BasicAuth)
getAuth = do
    flip catch (\e -> do {return (e :: SomeException); return Nothing}) $ do
      (l:p:_) <- splitRegex (mkRegex ":") <$> readFile "auth"
      return $ Just $ BasicAuth l (Prelude.takeWhile (/= '\n') p)

fetch :: String -> Maybe BasicAuth -> IO Blocks
fetch u m = do
  url <- return $ toEventsUrl u
  request <- case m of
               Nothing -> parseUrl url
               Just (BasicAuth l p) -> applyBasicAuth (BC.pack l) (BC.pack p) <$> parseUrl url
  withManager $ \manager -> do
    Response _ _ _ src <- httpLbs request manager
    let objects = (fromJust (decode src :: Maybe Array))
        events = DV.map (DAT.parseMaybe parseJSON) objects :: DV.Vector (Maybe Event)
        bs = DV.map (\e -> toBlock $ fromJust e) $ DV.filter (/= Nothing) events
        bs' = header 1 (str u) <> DV.foldl1 (\acc b -> acc <> b) bs
    return bs'

toBlock :: Event -> Blocks
toBlock (PushEvent cs t) =
  para (str ("PushEvent " ++ show t)) <>
  bulletList commitList
  where
    commitList = flip Prelude.map (DV.toList cs) $ \c ->
      plain (str $ unpack $ name c `mappend` ": " `mappend` comment c `mappend` " ")

toEventsUrl :: String -> String
toEventsUrl s = "https://api.github.com/repos/" ++ s ++ "/events"
