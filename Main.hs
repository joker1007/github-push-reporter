{-# LANGUAGE OverloadedStrings,FlexibleInstances,QuasiQuotes,TemplateHaskell #-}
module Main where

import Prelude hiding(catch,lookup)
import Control.Applicative ((<$>))
import Control.Monad (liftM)
import Control.Exception
import Data.Foldable (fold)
import Data.Monoid (mappend)
import Data.Text
import Data.Time
import Data.Map (lookup)
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
import System.Console.GetOpt.Simple

{- Format: <user or orgs>/<repo> -}
getRepos :: Maybe FilePath -> IO [String]
getRepos f = liftM Prelude.lines $ readFile $ fromMaybe "repositories" f

getPassword :: IO String
getPassword = do
  putStr "Password: "
  hFlush stdout
  pass <- withEcho False getLine
  putChar '\n'
  return pass

withEcho :: Bool -> IO a -> IO a
withEcho echo action = do
  old <- hGetEcho stdin
  bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action

{- Format: login:pass -}
getAuth :: Maybe LoginName -> IO (Maybe BasicAuth)
getAuth Nothing = return Nothing
getAuth (Just l) = getPassword >>= (\p -> return $ Just $ BasicAuth l p)

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
  para (str ("PushEvent to " ++ unpack r ++ " at " ++ show (jstTime t))) <>
  bulletList commitList
  where
    commitList = flip Prelude.map (DV.toList cs) $ \c ->
      plain (str (unpack $ name c `mappend` ": " `mappend` comment c `mappend` " : ") `mappend` link (commitUrl c) "Go To Commit" (linkStr c))
      where
        linkStr = str . Prelude.take 7 . unpack . sha

jstTime :: UTCTime -> LocalTime
jstTime t = utcToLocalTime (TimeZone (9 * 60) False "JST") t

{- Format: <user or orgs>/<repo> -}
toEventsUrl :: String -> String
toEventsUrl s = "https://api.github.com/repos/" ++ s ++ "/events"

options :: [(FlagMaker, String, Mode, String)]
options = [ (arg, "repositories", Optional, "Target Repositories (separated by comma)"),
            (arg, "login", Optional, "Github login ID"),
            (arg, "conf", Optional, "Repository config"),
            (arg, "format", Default "html", "Output file format")
          ]

type LoginName = String

main :: IO()
main = do
  (opts, _) <- getOptsArgs (makeOptions options) [] []
  let maybeRepositories = lookup "repositories" opts
      maybeLogin = lookup "login" opts
      maybeConf = lookup "conf" opts
      format = lookup "format" opts
  case maybeRepositories of
    Nothing -> do
      repos <- getRepos maybeConf
      process repos maybeLogin $ outputFormat $ fromJust format
    Just r -> do
      repos <- return $ splitRegex (mkRegex ",") r
      process repos maybeLogin $ outputFormat $ fromJust format

process :: [String] -> Maybe LoginName -> OutputFormat -> IO ()
process repos l f = do
  auth <- getAuth l
  bss <- mapM (flip fetch auth) repos
  bs <- return $ fold bss
  html <- return $ write (doc bs) f
  withFile ("report" ++ formatExt f) WriteMode (\h -> hPutStr h html)
  putStrLn $ "Output report" ++ formatExt f

write :: Pandoc -> OutputFormat -> String
write d HtmlFormat = writeHtmlString defaultWriterOptions {writerStandalone = True, writerTemplate = template} $ d
  where template = renderHtml $(shamletFile "template/layout.hamlet")
write d MarkdownFormat = writeMarkdown defaultWriterOptions $ d
write d RSTFormat = writeRST defaultWriterOptions $ d
write d MediaWikiFormat = writeMediaWiki defaultWriterOptions $ d
