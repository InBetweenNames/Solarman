import XSaiga.SolarmanTriplestore
import System.Environment

main = do
  [query] <- getArgs
  res <- parse query
  print res

