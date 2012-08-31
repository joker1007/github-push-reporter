{-# LANGUAGE OverloadedStrings,FlexibleInstances,QuasiQuotes,TemplateHaskell #-}
module Main where

import Prelude hiding(catch)
import Control.Applicative ((<$>))
import Control.Monad (liftM)
import Control.Monad.IO.Class (liftIO)
import Control.Exception
import Data.Foldable (fold)
import Data.Monoid (mappend)
import Data.Text
import Data.Time
import System.IO
import Network.HTTP.Conduit
import Data.Maybe
import qualified Data.Vector as DV
import Data.Aeson
import qualified Data.Aeson.Types as DAT
import qualified Data.ByteString.Char8 as BC
import Text.Regex
import Text.Pandoc
import Text.Pandoc.Builder
import Text.Hamlet
import Text.Blaze.Html.Renderer.String
import Types

{- Format: <user or orgs>/<repo> -}
getRepos :: IO [String]
getRepos = liftM Prelude.lines $ readFile "repositories"

{- Format: login:pass -}
getAuth :: IO (Maybe BasicAuth)
getAuth = handle (\(SomeException _) -> do {return Nothing}) $ do
    (l:p:_) <- splitRegex (mkRegex ":") <$> readFile "auth"
    return $ Just $ BasicAuth l (Prelude.takeWhile (/= '\n') p)

fetch :: String -> Maybe BasicAuth -> IO Blocks
fetch u m = do
  let url = toEventsUrl u
  request <- case m of
               Nothing -> parseUrl url
               Just (BasicAuth l p) -> applyBasicAuth (BC.pack l) (BC.pack p) <$> parseUrl url
  withManager $ \manager -> do
    Response _ _ _ src <- httpLbs request manager
    let objects = (fromJust (decode src :: Maybe Array))
        events = DV.map (DAT.parseMaybe parseJSON) objects :: DV.Vector (Maybe Event)
        bs = DV.map (\e -> toBlock $ fromJust e) $ DV.filter (/= Nothing) events
        bs' = header 2 (str u) <> DV.foldl1 (\acc b -> acc <> b) bs
    return bs'

toBlock :: Event -> Blocks
toBlock (PushEvent _ r cs t) =
  para (str ("PushEvent to " ++ unpack r ++ " " ++ show (jstTime t))) <>
  bulletList commitList
  where
    commitList = flip Prelude.map (DV.toList cs) $ \c ->
      plain (str (unpack $ name c `mappend` ": " `mappend` comment c `mappend` " ") `mappend` link (commitUrl c) "Go To Commit" "Go To Commit")

jstTime :: UTCTime -> LocalTime
jstTime t = utcToLocalTime (TimeZone (9 * 3600) False "JST") t

{- Format: <user or orgs>/<repo> -}
toEventsUrl :: String -> String
toEventsUrl s = "https://api.github.com/repos/" ++ s ++ "/events"

main :: IO()
main = do
  repos <- getRepos
  auth <- getAuth
  bss <- mapM (flip fetch auth) repos
  bs <- return $ fold bss
  html <- return $ writeHtmlString defaultWriterOptions {writerStandalone = True, writerTemplate = template} $ doc $ bs
  withFile "report.html" WriteMode (\h -> hPutStr h html)
  putStrLn "Output report.html"
    where template = renderHtml $(shamletFile "template/layout.hamlet")

