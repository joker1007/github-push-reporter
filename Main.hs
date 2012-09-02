{-# LANGUAGE OverloadedStrings,FlexibleInstances,QuasiQuotes,TemplateHaskell #-}
module Main where

import Prelude hiding(catch,lookup)
import Control.Applicative ((<$>))
import Control.Monad (liftM)
import Control.Exception
import Data.Foldable (fold)
import Data.Monoid (mappend,mempty)
import Data.Text
import Data.Time
import Data.Map (lookup)
import System.IO
import System.Locale (defaultTimeLocale)
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

type LoginName = String
type RepositoryName = String

{- Format: <user or orgs>/<repo> -}
getRepos :: Maybe FilePath -> IO [RepositoryName]
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

{- Fetch Repository Events json.
   And parse to pandoc Blocks -}
fetch :: Maybe BasicAuth -> String -> IO Blocks
fetch mauth url = do
  (opts, _) <- getOptsArgs (makeOptions options) [] []
  let filterDate = (lookup "date" opts >>= parseTime defaultTimeLocale "%F") :: Maybe Day
      filterAuthor = liftM pack $ lookup "author" opts
  request <- parseUrlWithAuth (toEventsApiUrl url) mauth
  withManager $ \manager -> do
    Response _ _ _ src <- httpLbs request manager
    let objects = (fromJust (decode src :: Maybe Array))
        events = DV.map (DAT.parseMaybe parseJSON) objects :: DV.Vector (Maybe Event)
        bs = DV.map (\e -> toBlock (fromJust e) filterAuthor) $ DV.filter (\e -> isJust e && e `compareDate` filterDate) events
        bs' = header 2 (str url) <> DV.foldl1 (\acc b -> acc <> b) bs
    return bs'
  where
    compareDate Nothing _ = False
    compareDate (Just event) mDate = (localDay $ utcToJstTime $ createdAt event) `justEql` mDate
    parseUrlWithAuth u Nothing = parseUrl u
    parseUrlWithAuth u (Just (BasicAuth l p)) = applyBasicAuth (BC.pack l) (BC.pack p) <$> parseUrl u

{- compare Only Maybe is not Nothing, otherwise always return True -}
justEql :: (Eq a) => a -> Maybe a -> Bool
justEql _ Nothing = True
justEql a (Just b) = a == b

toBlock :: Event -> Maybe Text -> Blocks
toBlock (PushEvent _ r cs t) author =
  case commitList of
    [] -> mempty
    xs -> para (str ("PushEvent to " ++ unpack r ++ " at " ++ show (utcToJstTime t))) <>
          bulletList xs
  where
    commitList = flip Prelude.map (DV.toList $ DV.filter (\c -> name c `justEql` author) cs) $ \c ->
      plain (str (unpack $ name c `mappend` ": " `mappend` comment c `mappend` " : ") `mappend` link (commitUrl c) "Go To Commit" (linkStr c))
      where
        linkStr = str . Prelude.take 7 . unpack . sha

utcToJstTime :: UTCTime -> LocalTime
utcToJstTime = utcToLocalTime (hoursToTimeZone 9)

{- Format: <user or orgs>/<repo> -}
toEventsApiUrl :: String -> String
toEventsApiUrl s = "https://api.github.com/repos/" ++ s ++ "/events"

options :: [(FlagMaker, String, Mode, String)]
options = [ (arg, "repositories", Optional, "Target Repositories (separated by comma)"),
            (arg, "login", Optional, "Github login ID"),
            (arg, "conf", Optional, "Repository config (per line '<user>/<repository name>'"),
            (arg, "format", Optional, "Output file format"),
            (arg, "date", Optional, "Filter by date"),
            (arg, "author", Optional, "Filter by author")
          ]

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
      process repos maybeLogin $ outputFormat $ fromMaybe "html" format
    Just r -> do
      repos <- return $ splitRegex (mkRegex ",") r
      process repos maybeLogin $ outputFormat $ fromMaybe "html" format

process :: [String] -> Maybe LoginName -> OutputFormat -> IO ()
process repos l f = do
  auth <- getAuth l
  bss <- mapM (fetch auth) repos
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
