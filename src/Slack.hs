{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Slack where

import           Data.Text            (Text)
import           Data.Yaml
import           GHC.Generics

import           Control.Monad.Reader
import qualified Web.Slack            as Slack
import qualified Web.Slack.Api        as Api
import qualified Web.Slack.Chat       as Chat

data Context = Context
  { token   :: Text
  , channel :: Text
  } deriving (Eq, Show, Generic)

instance FromJSON Context

c = Context {token = "token", channel = "channel"}

postMessage c msg = do
  slackConfig <- Slack.mkSlackConfig (token c)
  let req = Chat.mkPostMsgReq (channel c) msg
  flip runReaderT slackConfig (Slack.chatPostMessage req)
