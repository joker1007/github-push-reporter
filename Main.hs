{-# LANGUAGE OverloadedStrings,FlexibleInstances #-}
module Main where

import Control.Monad (mzero,liftM)
import Network.HTTP.Conduit
import Control.Monad.IO.Class (liftIO)
import Data.Maybe
import Data.Vector ((!),(!?))
import qualified Data.Vector as DV
import qualified Data.HashMap.Lazy as DH
import Data.Conduit
import Data.Conduit.Binary
import System.IO
import qualified Data.Attoparsec as P
import Data.Aeson
import Data.Aeson.Types (Parser)
import qualified Data.Aeson.Types as DAT
import qualified Data.ByteString.Lazy as L
import Data.Text
import Data.Time
import Data.Time.Format
import System.Locale (defaultTimeLocale)

data Commit = Commit {
                name :: Text,
                email :: Text,
                comment :: Text
                } deriving (Eq,Show)

data Event = PushEvent { commits :: DV.Vector Commit, createdAt :: UTCTime } deriving (Show,Eq)

instance FromJSON Commit where
  parseJSON (Object o) = do author <- o .: "author"
                            name <- author .: "name"
                            email <- author .: "email"
                            comment <- o .: "message"
                            return $ Commit name email comment
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

main :: IO()
main = do
  request <- parseUrl "https://api.github.com/repos/joker1007/pasokara_player3/events"
  withManager $ \manager -> do
    Response _ _ _ src <- httpLbs request manager
    let objects = (fromJust (decode src :: Maybe Array))
        events = DV.map (DAT.parseMaybe parseJSON) objects :: DV.Vector (Maybe Event)
        events' = DV.map (\e -> fromJust e) $ DV.filter (/= Nothing) events
    liftIO $ DV.mapM print events'
    return ()
