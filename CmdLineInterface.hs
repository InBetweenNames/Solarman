{-# LANGUAGE OverloadedStrings #-}

module CmdLineInterface where

import XSaiga.SolarmanTriplestore
import System.Environment
import Data.List
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

main = do
  [query] <- getArgs
  res <- formatParseIO $ T.pack query
  TIO.putStrLn $ T.concat $ intersperse " " res

