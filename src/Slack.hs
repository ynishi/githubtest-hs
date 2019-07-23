{-# LANGUAGE OverloadedStrings #-}

module Slack where

import qualified Web.Slack as Slack 
import qualified Web.Slack.Api as Api
import qualified Web.Slack.Chat as Chat 
import Control.Monad.Reader

postMessage token = do
    slackConfig <- Slack.mkSlackConfig token
    let req = Chat.mkPostMsgReq "ch" "msg"
    flip runReaderT slackConfig (Slack.chatPostMessage req)
