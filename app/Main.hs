module Main where

import           Data.Text          (pack)
import           Lib
import           System.Environment (getArgs)

main = do
  args <- getArgs
  let c =
        if not (null args)
          then Lib.c {endpoint = pack . head $ args}
          else Lib.c
  print c
  getPRList c
