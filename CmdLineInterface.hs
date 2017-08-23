module CmdLineInterface where

import XSaiga.SolarmanTriplestore
import System.Environment
import Data.List

main = do
  [query] <- getArgs
  res <- formatParseIO query
  putStrLn $ concat $ intersperse " " res

