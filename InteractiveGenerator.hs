import qualified SolarmanTriplestore as App
import qualified TypeAg2 as TypeAg
import qualified Data.Map.Strict as Map
import System.IO
import Data.String.Utils
import Data.List
import Data.Char

{-Objective: generate the file Interactive.hs with variables corresponding to things in the database,
 - e.g. variable hall = make_proper "hall "-}

filterDict t = filter (\(_,y,_) -> y == t) App.dictionary

makeFriendlyName = map toLower . replace "-" "_"

v_make_pnoun name = makeFriendlyName name ++ " = make_pnoun \"" ++ name ++ "\""
v_make_cnoun name = makeFriendlyName name ++ " = get_members dataStore \"" ++ name ++ "\""

v_discoverer_intrans = "discoverer = get_subjs_of_event_type dataStore \"discover_ev\""
v_discoverers_intrans = "discoverers = get_subjs_of_event_type dataStore \"discover_ev\""

typeActionMap = Map.fromList [(TypeAg.Pnoun, v_make_pnoun),(TypeAg.Cnoun, v_make_cnoun)]

removeUnwanted = filter (\(x,y,z) -> x /= "discoverer" && x /= "discoverers") 

genVariables = foldr var [] (removeUnwanted App.dictionary)
    where
    var (name,memoL,_) list | Just f <- Map.lookup memoL typeActionMap = (f name):list
    var (name,memoL,_) list = list


printVars handle = mapM_ (hPutStrLn handle) (v_discoverer_intrans:v_discoverers_intrans:(nub genVariables))


main = do
    file <- openFile "Interactive.hs" WriteMode
    hPutStrLn file "{-# LANGUAGE NoMonomorphismRestriction #-}"
    hPutStrLn file "import SolarmanTriplestore as App"
    hPutStrLn file "import Getts as Getts"
    hPutStrLn file ""
    printVars file
    hClose file

