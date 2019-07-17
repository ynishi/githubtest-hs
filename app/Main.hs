module Main where

import           Data.Either        (fromRight)
import           Data.Text          (pack)
import           Data.Yaml
import           Lib
import           System.Environment (getArgs)

main = do
  args <- getArgs
  config <-
    decodeFileEither "config.yml" :: IO (Either ParseException Lib.Context)
  let base = fromRight Lib.c config
  let c =
        if not (null args)
          then base {endpoint = pack . head $ args}
          else base
  print c
  getPRList c
