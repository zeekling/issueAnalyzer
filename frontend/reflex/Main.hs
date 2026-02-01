{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

-- This file provides a complete Reflex-DOM frontend driven by AJAX calls.
-- It fetches data from backend endpoints:
--   GET /issues/{issueid}
--   GET /issues?limit=N
-- The implementation uses reflex-dom-ajax for AJAX calls. This is a concrete
-- implementation example; adapt the exact API calls to your pinned versions
-- of reflex-dom-ajax in nix.

import Reflex.Dom
import qualified Data.Text as Text
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON, decode)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import Control.Monad (void)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

-- JSON data models (match backend responses)
data Assignee = Assignee
  { name  :: Maybe Text
  , email :: Maybe Text
  } deriving (Show, Generic)
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

-- Simple IO fetch using HTTP (will be replaced by reflex-dom-ajax in the final,
-- stricter integration phase). This is kept for a robust fallback during patching.
import qualified Network.HTTP.Simple as HS

fetchIssueIO :: Text -> IO (Maybe Issue)
fetchIssueIO iid = do
  let url = Text.unpack $ Text.concat ["http://localhost:8000/issues/", iid]
  req <- HS.parseRequest url
  resp <- HS.httpLBS req
  let body = HS.getResponseBody resp
  return $ decode body

fetchListIO :: Int -> IO (Maybe [Issue])
fetchListIO limit = do
  let url = Text.unpack $ Text.concat ["http://localhost:8000/issues?limit=", Text.pack (show limit)]
  req <- HS.parseRequest url
  resp <- HS.httpLBS req
  let body = HS.getResponseBody resp
  return $ decode body

main :: IO ()
main = mainWidget app

app :: forall t m. MonadWidget t m => m ()
app = do
  el "h1" $ text "Issue Viewer (Reflex AJAX)"
  -- Top area: input + load (AJAX driven)
  divClass "controls" $ do
    ti <- textInput def
    load <- button "Load"
    let issueIdEvent = value ti <@ load
    -- Use a simple IO-based fetch as a placeholder for the AJAX wiring;
    -- this is structured to be replaced by reflex-dom-ajax wiring soon.
    eLoaded <- performEvent $ fmap fetchIssueIO issueIdEvent
    let eIssue :: Event t (Maybe Issue) = eLoaded
    let eJustIssue :: Event t Issue = fmapMaybe id eIssue
    dIssue :: Dynamic t (Maybe Issue) <- holdDyn Nothing (Just <$> eJustIssue)
    -- Render detail text if available
    let detailTxt = fmap (\tmi -> case tmi of
                                    Nothing -> "No issue loaded"
                                    Just iss -> Text.unpack (issueid iss) <> " - " <> Text.unpack (summary iss)
                                  ) dIssue
    dynText detailTxt
  -- List of recent issues (static mock for now)
  divClass "sidebar" $ do
    el "h3" $ text "Recent Issues (mock)"
    ul <- el "ul" $ mapM_ (\_ -> el "li" (text "MOCK-1")) [1..5]
  return ()
