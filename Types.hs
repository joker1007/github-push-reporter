{-# LANGUAGE OverloadedStrings,FlexibleInstances #-}
module Types where

import Control.Monad (mzero,liftM)
import Data.Text
import Data.Aeson
import Data.Aeson.Types (Parser)
import qualified Data.Vector as DV
import Data.Time
import System.Locale (defaultTimeLocale)

data Commit = Commit {
                name    :: Text,
                email   :: Text,
                comment :: Text,
                sha     :: Text,
                commitUrl     :: String
              } deriving (Eq,Show)

data Event = PushEvent {
               repositoryName :: Text,
               ref :: Text,
               commits :: DV.Vector Commit,
               createdAt :: UTCTime
             } deriving (Show,Eq)

instance FromJSON Commit where
  parseJSON (Object o) = do author <- o .: "author"
                            n <- author .: "name"
                            e <- author .: "email"
                            c <- o .: "message"
                            s <- o .: "sha"
                            u <- return ""
                            return $ Commit n e c s u
  parseJSON _ = mzero

instance FromJSON Event where
  parseJSON (Object o) = do eventType <- o .: "type"
                            parseEvent eventType
    where
      parseEvent :: String -> Parser Event
      parseEvent "PushEvent" = do repo <- o .: "repo"
                                  n <- repo .: "name"
                                  payload <- o .: "payload"
                                  r <- payload .: "ref"
                                  cs <- payload .: "commits"
                                  vcs <- parseJSON cs :: Parser (DV.Vector Commit)
                                  vcs' <- return $ DV.map (\c -> c {commitUrl = shaToCommitUrl n $ sha c}) vcs
                                  Just t <- liftM (parseTime defaultTimeLocale "%FT%TZ") $ o .: "created_at"
                                  return $ PushEvent n r vcs' t
      parseEvent _ = mzero

  parseJSON _ = mzero

instance FromJSON (DV.Vector Commit) where
  parseJSON (Array a) = DV.mapM parseJSON a
  parseJSON _ = mzero

instance FromJSON (DV.Vector Event) where
  parseJSON (Array a) = DV.mapM parseJSON a
  parseJSON _ = mzero

data BasicAuth = BasicAuth {login :: String, password :: String} deriving Show

shaToCommitUrl :: Text -> Text -> String
shaToCommitUrl n s = "https://github.com/" ++ unpack n ++ "/commit/" ++ unpack s
