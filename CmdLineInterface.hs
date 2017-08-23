module CmdLineInterface where

import XSaiga.SolarmanTriplestore
import System.Environment

main = do
  [query] <- getArgs
  res <- formatParseIO query
  print res

