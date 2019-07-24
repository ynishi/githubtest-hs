module Main where

import           Data.Either        (fromRight)
import           Data.Text          (pack)
import           Data.Yaml
import           System.Environment (getArgs)
import qualified Github as G
import qualified Slack as S

main = do
  args <- getArgs
  githubConfig <-
    decodeFileEither "config_github.yml" :: IO (Either ParseException G.Context)
  let githubBase = fromRight G.c githubConfig
  let c =
        if not (null args)
          then githubBase {G.endpoint = pack . head $ args}
          else githubBase 
  print c
  G.getPRList c
  slackConfig <-
    decodeFileEither "config_slack.yml" :: IO (Either ParseException S.Context)
  let slackBase = fromRight S.c slackConfig 
  print slackBase 
  S.postMessage slackBase . pack $ "msg"
