module Main where

import           Data.Either        (fromRight)
import           Data.Text          (pack, unpack)
import qualified Data.Vector        as V (Vector, empty, filter, foldl,
                                          fromList, head, map, toList)
import           Data.Yaml
import qualified Github             as G
import qualified Slack              as S
import           System.Environment (getArgs)

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
  prs <- G.getPRList c
  slackConfig <-
    decodeFileEither "config_slack.yml" :: IO (Either ParseException S.Context)
  let slackBase = fromRight S.c slackConfig
  print slackBase
  S.postMessage slackBase .
    V.foldl
      (\z x ->
         pack
           ((unpack z) ++
            (unpack
               (V.foldl
                  (\y x' ->
                     pack
                       ((unpack y) ++
                        (unpack
                           (V.foldl
                              (\t x'' -> pack ((unpack t) ++ (unpack x'')))
                              (pack "")
                              x'))))
                  (pack "")
                  x))))
      (pack "") $
    prs
