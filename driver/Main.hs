{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified XSaiga.CGI as C

import qualified XSaiga.SolarmanTriplestore as App

import qualified XSaiga.TypeAg2 as TypeAg

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import System.IO

#ifdef ASTERIUS

import Asterius.Aeson
import Asterius.ByteString
import Asterius.Text
import Asterius.Types

foreign import javascript safe "getText()" getText :: IO JSString

#ifdef INSTORE
dataStore = Local.localData
#else
dataStore = C.remoteData
#endif

main = do
    jsString <- getText
    let input_query = textFromJSString $ jsString
    TIO.putStrLn $ input_query
    C.interpret''' dataStore input_query

#else

main = C.main

#endif

--main = TIO.putStrLn $ T.pack "Hello world"

--main = (C.runQuery $ TypeAg.get_members "moon") >>= (\x -> TIO.putStrLn $ T.intercalate "," (map fst x))

--main = putStrLn $ "test: " ++ (show $ map (TypeAg.getGetts . TypeAg.getQUVAL) $ App.parse "which vacuumous moon orbits a brown planet that was discovered by hall or galileo or a person that discovered a moon in 1877 with a telescope")

--main = putStrLn $ "test: " ++ (show $ map (TypeAg.getGetts . TypeAg.getQUVAL) $ App.parse "what did hall discover")

--main = putStrLn $ show $ TypeAg.getGetts $ TypeAg.get_members "moon"

--main = putStrLn $ show $ TypeAg.flatOptimize $ TypeAg.flattenGetts $ TypeAg.getGetts $ App.make_trans_active App.discover_rel $ (App.intersect_fdbr $ TypeAg.get_members "moon")

--main = do
--    let trees = map (show . snd) $ App.parseTree "which vacuumous moon orbits a brown planet that was discovered by hall or galileo or a person that discovered a moon in 1877 with a telescope"
--    mapM_ putStrLn trees
