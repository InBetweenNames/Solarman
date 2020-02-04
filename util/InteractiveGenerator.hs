{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}

import qualified XSaiga.SolarmanTriplestore as App
import qualified XSaiga.TypeAg2 as TypeAg
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified System.IO as SIO
import Data.Text.IO
import Data.List
import Data.Char
import Data.Maybe

{-Objective: generate the file Interactive.hs with variables corresponding to things in the database,
 - e.g. variable hall = make_proper "hall "-}

filterDict t = map (\(x,y,z) -> (x,y)) $ filter (\(_,y,_) -> y == t) App.dictionary

makeFriendlyName = T.toLower . T.replace "-" "_"

cnounMap = Map.fromList
    [("things", "thing"),
    ("planets", "planet"),
    ("moons", "moon"),
    ("satellite", "moon"),
    ("satellites", "moon"),
    ("exist", "thing"),
    ("exists", "thing"),
    ("spins", "spin"),
    ("people", "person"),
    ("human", "person"),
    ("humans", "person"),
    ("team", "science_team"),
    ("teams", "science_team")]


v_make_pnoun name = makeFriendlyName name `T.append` " = make_pnoun \"" `T.append` name `T.append` "\""
v_make_cnoun name = makeFriendlyName name `T.append` " = get_members \"" `T.append` fromMaybe name (Map.lookup name cnounMap) `T.append` "\""
v_make_adj = v_make_cnoun

v_discoverer_cnoun = "discoverer = get_subjs_of_event_type \"discover_ev\""
v_discoverers_cnoun = "discoverers = get_subjs_of_event_type \"discover_ev\""

v_make_intrans = v_make_cnoun

v_make_transvb name ev = makeFriendlyName name `T.append` " = make_trans_active " `T.append` ev
v_make_transvb_filt name ev = makeFriendlyName name `T.append` "' = make_trans_active' " `T.append` ev
v_make_transvb_tmph_filt name ev = makeFriendlyName name `T.append` "'' = make_trans_active'' " `T.append` ev
v_make_transvb_inverted name ev = makeFriendlyName name `T.append` "_ = make_trans_passive " `T.append` ev
--v_make_transvb_inverted_filt name ev = makeFriendlyName name `T.append` "_' tmph preps = make_trans_passive' \"" `T.append` ev `T.append` "\" $ ([\"object\"],tmph):preps"

typeActionMap = Map.fromList 
    [(TypeAg.Pnoun, v_make_pnoun),
    (TypeAg.Cnoun, v_make_cnoun),
    (TypeAg.Intransvb, v_make_intrans),
    (TypeAg.Adj, v_make_adj)]

removeUnwanted = filter (\(x,y,z) -> notElem x ["discoverer", "discoverers", "telescopes", "places"]) 

genVariables = foldr var [] (removeUnwanted App.dictionary)
    where
    var (name,memoL,_) list | Just f <- Map.lookup memoL typeActionMap = (f name):list
    var (name,memoL,_) list = list


printVars handle = mapM_ (hPutStrLn handle) (nub genVariables)


verbForm file transVbList ev = flip mapM_ transVbList $ \transVb -> do
    hPutStrLn file $ v_make_transvb transVb ev
    hPutStrLn file $ v_make_transvb_filt transVb ev
    hPutStrLn file $ v_make_transvb_tmph_filt transVb ev
    hPutStrLn file $ v_make_transvb_inverted transVb ev
    --hPutStrLn file $ v_make_transvb_inverted_filt transVb ev
    hPutStrLn file ""


main = do
    file <- SIO.openFile "Interactive.hs" SIO.WriteMode
    hPutStrLn file "{-# LANGUAGE NoMonomorphismRestriction #-}"
    hPutStrLn file "{-# LANGUAGE NoImplicitPrelude #-}"
    hPutStrLn file "{-# LANGUAGE OverloadedStrings #-}"
    hPutStrLn file "module Interactive where"
    hPutStrLn file ""
    hPutStrLn file "import XSaiga.SolarmanTriplestore"
    hPutStrLn file "import XSaiga.Getts"
    hPutStrLn file "import XSaiga.TypeAg2"
    hPutStrLn file "import XSaiga.CGI"
    hPutStrLn file "import Data.Functor"
    hPutStrLn file "import Data.Biapplicative"
    hPutStrLn file "import Control.Monad"
    hPutStrLn file ""
    hPutStrLn file "a $ b = a b"
    hPutStrLn file "infixr 0 $"
    hPutStrLn file "a . b = \\x -> a (b x)"
    hPutStrLn file "infixr 9 ."
    hPutStrLn file ""
    verbForm file ["discover", "discovers", "discovered"] "discover_rel"
    verbForm file ["orbit", "orbits", "orbited"] "orbit_rel"
    verbForm file ["use", "uses", "used"] "use_rel"
    hPutStrLn file v_discoverer_cnoun
    hPutStrLn file v_discoverers_cnoun
    hPutStrLn file "anyone = a person"
    hPutStrLn file "anything = a thing"
    hPutStrLn file "anybody = a person"
    hPutStrLn file "someone = a person"
    hPutStrLn file "something = a thing"
    hPutStrLn file "somebody = a person"
    hPutStrLn file "everyone = every person"
    hPutStrLn file "everything = every thing"
    hPutStrLn file "everybody = every person"
    hPutStrLn file "telescopes = telescope"
    hPutStrLn file "places = place"
    printVars file
    SIO.hClose file

