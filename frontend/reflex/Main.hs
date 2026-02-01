{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

-- This is a minimal Reflex-DOM frontend skeleton for displaying a single
-- Jira issue fetched from the REST API exposed by the backend.
-- It is intended as a starting point for integrating Reflex into this project.

import           Reflex.Dom
import           Data.Aeson (FromJSON, ToJSON, decode)
import           GHC.Generics (Generic)
import           qualified Data.Text as T
import           qualified Data.ByteString.Lazy as BL
import           Data.Maybe (fromMaybe)
import           Control.Monad (void)
import           Control.Monad.IO.Class (liftIO)
import           Data.Text (Text)
import           Network.HTTP.Client (HttpException)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Network.HTTP.Client (newManager, httpLbs, parseRequest, Response, responseBody)

-- Define a minimal Issue type that mirrors the API response viewed by the backend.
data Assignee = Assignee { name :: Maybe Text, email :: Maybe Text } deriving (Show, Generic)
data Issue = Issue
  { issueid      :: Maybe Text
  , summary      :: Maybe Text
  , description  :: Maybe Text
  , status       :: Maybe Text
  , assignee     :: Maybe Assignee
  , created      :: Maybe Text
  , updated      :: Maybe Text
  , issuetype    :: Maybe Text
  , labels       :: Maybe [Text]
  , priority     :: Maybe Text
  , resolution   :: Maybe Text
  , fixVersions  :: Maybe [Text]
  , created_at   :: Maybe Text
  } deriving (Show, Generic)

instance FromJSON Issue
instance ToJSON Issue

-- Simple helper to fetch an issue by ID from the backend API.
fetchIssue :: String -> IO (Maybe Issue)
fetchIssue issueid = do
  manager <- newManager tlsManagerSettings
  let url = "http://localhost:8000/issues/" ++ issueid
  req <- parseRequest url
  -- We ignore errors for brevity; a production app should handle retry logic.
  res <- httpLbs req manager
  let mIssue = decode (responseBody res) :: Maybe Issue
  return mIssue

main :: IO ()
main = do
    -- This is a placeholder Reflex app wrapper. In a real setup you would
    -- compile with reflex-dom and run in a browser.
    putStrLn "Reflex frontend placeholder. This skeleton demonstrates how to hook up to the API." 
    -- Example: fetch and print a sample issue (console only)
    m <- fetchIssue "YARN-123"
    print m
