import qualified SolarmanTriplestore as App
import qualified TypeAg2 as TypeAg
import qualified Data.Map.Strict as Map
import System.IO
import Data.String.Utils
import Data.List
import Data.Char
import Data.Maybe

{-Objective: generate the file Interactive.hs with variables corresponding to things in the database,
 - e.g. variable hall = make_proper "hall "-}

filterDict t = map (\(x,y,z) -> (x,y)) $ filter (\(_,y,_) -> y == t) App.dictionary

makeFriendlyName = map toLower . replace "-" "_"

cnounMap = Map.fromList
    [("things", "thing"),
    ("planets", "planet"),
    ("moons", "moon"),
    ("satellite", "moon"),
    ("satellites", "moon"),
    ("exist", "thing"),
    ("exists", "thing"),
    ("spins", "spin")]

v_make_pnoun name = makeFriendlyName name ++ " = make_pnoun \"" ++ name ++ "\""
v_make_cnoun name = makeFriendlyName name ++ " = get_members dataStore \"" ++ fromMaybe name (Map.lookup name cnounMap) ++ "\""
v_make_adj = v_make_cnoun

v_discoverer_cnoun = "discoverer = get_subjs_of_event_type dataStore \"discover_ev\""
v_discoverers_cnoun = "discoverers = get_subjs_of_event_type dataStore \"discover_ev\""

v_make_intrans = v_make_cnoun

v_make_transvb name ev = makeFriendlyName name ++ " = make_relation \"" ++ ev ++ "\"" 
v_make_transvb_filt name ev = makeFriendlyName name ++ "' = make_filtered_relation \"" ++ ev ++" \""
--v_make_transvb_inverted name ev = makeFriendlyName name ++ "_by = make_inverted_filtered_"

typeActionMap = Map.fromList 
    [(TypeAg.Pnoun, v_make_pnoun),
    (TypeAg.Cnoun, v_make_cnoun),
    (TypeAg.Intransvb, v_make_intrans),
    (TypeAg.Adj, v_make_adj)]

removeUnwanted = filter (\(x,y,z) -> x /= "discoverer" && x /= "discoverers") 

genVariables = foldr var [] (removeUnwanted App.dictionary)
    where
    var (name,memoL,_) list | Just f <- Map.lookup memoL typeActionMap = (f name):list
    var (name,memoL,_) list = list


printVars handle = mapM_ (hPutStrLn handle) (nub genVariables)


main = do
    file <- openFile "Interactive.hs" WriteMode
    hPutStrLn file "{-# LANGUAGE NoMonomorphismRestriction #-}"
    hPutStrLn file ""
    hPutStrLn file "import SolarmanTriplestore as App"
    hPutStrLn file "import Getts as Getts"
    hPutStrLn file ""
    hPutStrLn file $ v_make_transvb "discover" "discover_ev"
    hPutStrLn file $ v_make_transvb "discovered" "discover_ev"
    hPutStrLn file $ v_make_transvb "discovers" "discover_ev"
    hPutStrLn file $ v_make_transvb "orbit" "orbit_ev"
    hPutStrLn file $ v_make_transvb "orbits" "orbit_ev"
    hPutStrLn file $ v_make_transvb "orbited" "orbit_ev"
    hPutStrLn file v_discoverer_cnoun
    hPutStrLn file v_discoverers_cnoun
    printVars file
    hClose file

