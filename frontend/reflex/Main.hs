{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Reflex.Dom
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Data.Maybe (fromMaybe)
import Data.Aeson (FromJSON, ToJSON)

-- Lightweight models mirroring the backend response structure (partial, for UI)
data Assignee = Assignee { name :: Maybe Text, email :: Maybe Text } deriving (Show, Generic)
data Issue = Issue
  { issueid    :: Text
  , summary    :: Text
  , description:: Text
  , status     :: Text
  , assignee   :: Assignee
  , created    :: Text
  , updated    :: Text
  , issuetype  :: Text
  , labels     :: [Text]
  , priority   :: Text
  , resolution :: Text
  , fixVersions:: [Text]
  , created_at :: Text
  } deriving (Show, Generic)

instance FromJSON Assignee
instance FromJSON Issue

-- A small mock issue to demonstrate the UI. Replace with real AJAX fetch later.
mockIssue :: Issue
mockIssue = Issue
  { issueid = "YARN-1"
  , summary = "Sample issue for Reflex frontend"
  , description = "This is a sample issue used to demonstrate the Reflex front-end UI."
  , status = "Done"
  , assignee = Assignee (Just "Alice") (Just "alice@example.com")
  , created = "2025-01-01T12:00:00Z"
  , updated = "2025-01-02T12:00:00Z"
  , issuetype = "Bug"
  , labels = ["reflex", "frontend"]
  , priority = "High"
  , resolution = "Fixed"
  , fixVersions = ["v1.0"]
  , created_at = "2025-01-01T12:00:00Z"
  }

main :: IO ()
main = mainWidget app

app :: MonadWidget t m => m ()
app = do
  el "h1" $ text "Issue Viewer (Reflex Frontend)"
  -- Simple static UI showing mock data; swap in real AJAX calls later
  el "div" $ do
    el "h2" $ text "Details (Mock)"
    el "p" $ text $ T.concat ["ID: ", issueid mockIssue]
    el "p" $ text $ T.concat ["Summary: ", summary mockIssue]
    el "p" $ text $ T.concat ["Description: ", description mockIssue]
    el "p" $ text $ T.concat ["Status: ", status mockIssue]
    el "p" $ text $ T.concat ["Assignee: ", maybeText (name (assignee mockIssue)) , " <", maybeText (email (assignee mockIssue)), ">"]
  el "div" $ do
    el "h3" $ text "Recent Issues (Mock)"
    ul <- el "ul" $ do
      li $ text (issueid mockIssue)
      li $ text (issueid mockIssue)
  where
    maybeText :: Maybe Text -> Text
    maybeText (Just t) = t
    maybeText Nothing  = ""
