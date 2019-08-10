{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}

module XSaiga.Getts where
import Data.List as List
import Data.Text as T hiding (concat, map, zip, head)
import Control.Monad
import Debug.Trace

--For slow collect
import qualified Data.Map.Strict as Map

--For endpoint query
import Data.RDF.Types hiding (triple, Triple)
import Database.HSparql.Connection
import Database.HSparql.QueryGenerator

--For caching name lookup
import qualified Network.Socket as Net

import qualified Data.Map as M
import Data.IORef
import System.IO.Unsafe

addUri namespace_uri = (T.append namespace_uri)

type Event = Text
type Triple = (Event, Text, Text)
type FDBR = [(Text,[Event])]

--getts returns all triples in the triple store that match the given parameters
class TripleStore m where
    getts_triples_entevprop_type :: m -> [Text] -> Text -> IO [Triple]
    
    getts_triples_entevprop :: m -> [Text] -> [Event] -> IO [Triple]
   
    getts_triples_members :: m -> Text -> IO [Triple]

sortFirst = sortBy (\x y -> compare (fst x) (fst y))

--TODO: This is DANGEROUS.  Needs refactoring.
make_fdbr_with_prop :: [Triple] -> Text -> FDBR
make_fdbr_with_prop ev_data entity_type 
  = collect $ map (\(x, _, z) -> (z, x)) $ List.filter (\(x, y, z) -> y == entity_type) ev_data

getts_1 ev_data ("?", b, c) = [x | (x,y,z) <- ev_data, b == y, c == z]
getts_2 ev_data (a, "?", c) = [y | (x,y,z) <- ev_data, a == x, c == z]
getts_3 ev_data (a, b, "?") = [z | (x,y,z) <- ev_data, a == x, b == y]

--TODO: needs serious refactoring... divergence big time

--For the "In Program" triple store -- whole typeclass needs refactoring!
instance TripleStore [Triple] where
    getts_triples_entevprop_type ev_data propNames ev_type = do
      let evs_with_type_ev_type = getts_1 ev_data ("?", "type", ev_type)
      getts_triples_entevprop ev_data propNames evs_with_type_ev_type

    getts_triples_entevprop ev_data propNames evs = 
      return $ List.filter (\(ev, prop, _) -> ev `elem` evs && prop `elem` ("type":propNames)) ev_data
    
    getts_triples_members ev_data set = do
      let evs_with_set_as_object = getts_1 ev_data ("?", "object", set)
      let evs_with_type_membership = getts_1 ev_data ("?", "type", "membership")
      let evs = intersect evs_with_set_as_object evs_with_type_membership
      --TODO: abstract triple filtering mechanism for events and ev_types
      let triples = [(x, y, z) | (x, y, z) <- ev_data, x `elem` evs]
      return $ triples

--TODO: note, the pure versions do NOT need to include type information

pure_getts_triples_entevprop_type ev_data propNames ev_type =
  pure_getts_triples_entevprop ev_data propNames evs_with_type_ev_type
  where
    evs_with_type_ev_type = getts_1 ev_data ("?", "type", ev_type)

pure_getts_triples_entevprop ev_data propNames evs
  = List.filter (\(ev, prop, _) -> ev `elem` evs && prop `elem` propNames) ev_data

pure_getts_members ev_data set = collect $ setRel
  where
    evs_with_set_as_object = getts_1 ev_data ("?", "object", set)
    evs_with_type_membership = getts_1 ev_data ("?", "type", "membership")
    evs = intersect evs_with_set_as_object evs_with_type_membership
    setRel = [(z, x) | (x, y, z) <- ev_data, x `elem` evs, "subject" == y]


data SPARQLBackend = SPARQL String Text deriving (Ord, Eq)

endpointTable :: IORef (M.Map String String)
{-# NOINLINE endpointTable #-}
endpointTable = unsafeDupablePerformIO $ newIORef M.empty

lookupEndpoint :: String -> IO String
lookupEndpoint url = do
  m <- readIORef endpointTable
  case M.lookup url m of
    Nothing -> Net.getAddrInfo Nothing (Just $ getServer url) (Just "http") >>=
      \x -> (writeIORef endpointTable (M.insert url (newURL (showAddress x) (getURLPath url)) m) >> return (newURL (showAddress x) (getURLPath url)))
    Just res -> return res
  where
  getServer = List.takeWhile (\x -> '/' /= x) . List.drop 7
  showAddress = show . Net.addrAddress . head
  getURLPath xs = List.drop (7 + (List.length $ getServer xs)) xs
  newURL server path = "http://" ++ server ++ path

--the String in this instance is to be the endpoint that you wish to query
instance TripleStore SPARQLBackend where
    getts_triples_entevprop_type (SPARQL endpoint namespace_uri) propNames ev_type = do
      resolvedEndpoint <- lookupEndpoint endpoint
      m <- selectQuery resolvedEndpoint query
      case m of
        (Just res) -> return $ List.concatMap (\[x, y, z] -> [(removeUri namespace_uri $ deconstruct x, "type", ev_type), (removeUri namespace_uri $ deconstruct x, removeUri namespace_uri $ deconstruct y, removeUri namespace_uri $ deconstruct z)]) res
        Nothing -> return []
      where
        query :: Query SelectQuery
        query = do
          sol <- prefix "sol" (iriRef namespace_uri)
          ev <- var
          prop <- var
          ent <- var
          triple ev prop ent
          triple ev (sol .:. "type") (sol .:. ev_type)
          filterExpr $ List.foldr1 (.||.) $ map ((prop .==.) . (sol .:. )) propNames --type required here as this is not an FDBR
          selectVars [ev, prop, ent]

    getts_triples_entevprop (SPARQL endpoint namespace_uri) propNames evs = do
      resolvedEndpoint <- lookupEndpoint endpoint
      m <- selectQuery resolvedEndpoint query
      case m of
        (Just res) -> return $ map (\[x, y, z] -> (removeUri namespace_uri $ deconstruct x, removeUri namespace_uri $ deconstruct y, removeUri namespace_uri $ deconstruct z)) res
        Nothing -> return []
      where
        query :: Query SelectQuery
        query = do
          sol <- prefix "sol" (iriRef namespace_uri)
          ev <- var
          prop <- var
          ent <- var
          triple ev prop ent
          filterExpr $ List.foldr1 (.||.) $ map ((ev .==.) . (sol .:. )) evs
          filterExpr $ List.foldr1 (.||.) $ map ((prop .==.) . (sol .:. )) propNames
          selectVars [ev, prop, ent]

    getts_triples_members (SPARQL endpoint namespace_uri) set = do
      resolvedEndpoint <- lookupEndpoint endpoint
      m <- selectQuery resolvedEndpoint query
      case m of
        (Just res) -> return $ Prelude.concatMap (\[ev, ent] ->
          [(removeUri namespace_uri $ deconstruct ev, "type", "membership"),
          (removeUri namespace_uri $ deconstruct ev, "subject", removeUri namespace_uri $ deconstruct ent),
          (removeUri namespace_uri $ deconstruct ev, "object", set)]) res
        Nothing -> return []
      where
        query :: Query SelectQuery
        query = do
          sol <- prefix "sol" (iriRef namespace_uri)
          ev <- var
          ent <- var
          triple ev (sol .:. "type") (sol .:. "membership")
          triple ev (sol .:. "subject") ent
          triple ev (sol .:. "object") (sol .:. set)
          selectVars [ev, ent]

    --Used to return FDBR directly
    --Efficient implementation of getts_members for SPARQL backend
    {-getts_members = getts_members'
        where
        getts_members' (SPARQL endpoint namespace_uri) set = do
            resolvedEndpoint <- lookupEndpoint endpoint
            m <- selectQuery resolvedEndpoint query
            case m of
                (Just res) -> return $ condense $ map (\[x, y] -> (removeUri namespace_uri $ deconstruct x, removeUri namespace_uri $ deconstruct y)) res
                Nothing -> return []
            where
                query :: Query SelectQuery
                query = do
                    sol <- prefix "sol" (iriRef namespace_uri)
                    ev <- var
                    subj <- var
                    triple ev (sol .:. "type") (sol .:. "membership")
                    triple ev (sol .:. "subject") subj
                    triple ev (sol .:. "object") (sol .:. set)
                    orderNext subj
                    distinct
                    selectVars [subj, ev]
    -}

removeUri :: Text -> Text -> Text         
removeUri namespace_uri = T.drop $ T.length namespace_uri
                      
preprocess :: Text -> IO [[BindingValue]] -> IO [Text]
preprocess namespace_uri bvals = bvals >>= \x -> return $ map (removeUri namespace_uri . deconstruct) $ concat x


deconstruct :: BindingValue -> Text
deconstruct value = do
    let (Bound node) = value
    case node of
        UNode strURI -> strURI
        LNode (PlainL strLit) -> strLit


{-collect accepts a binary relation as input and computes the image img of each
element x in the projection of the left column of the relation, under the relation,
and returns all of these (x,img) pairs in a list

Note: Since == is an equivalence relation, if multiple elements in the projection
of the left column are in the same equivalence class, only the first member of that
equivalence class that appears in the input list (as the left hand column of a tuple)
will appear in the output (as the left hand column of a tuple).

That is, the first element of that equivalence class that appears in the input list
will be chosen to represent the entire equivalence class in the output.

-}
    
--Faster collect: runs in n lg n time
--collect = condense . sortFirst
collect = Map.toList . Map.fromListWith (++) . map (\(x, y) -> (x, [y]))

--condense computes the image under a sorted relation
--condense runs in O(n) time and is lazy, also is lazy in the list computed in each tuple
--TODO: Use Map.toList/fromList to simplify?  Benchmark.
--In particular: Should unstableSortBy be used?
--condense :: (Eq a, Ord a) => [(a, a)] -> [(a, [a])]
--condense [] = []
--condense ((x,y):t) = (x, y:a):(condense r)
--    where 
--    (a, r) = findall x t
--    findall x [] = ([], [])
--    findall x list@((t,y):ts) | x /= t = ([], list)
--    findall x ((t,y):ts) | x == t = let (a2, t2) = (findall x ts) in (y:a2, t2)

condense = map (\list -> (fst $ head list, map snd list)) . List.groupBy cmp
  where
  cmp x y = (fst x) == (fst y)

--alias for getts_fdbr_entevprop_type
--make_fdbr :: (TripleStore m) => m -> Text -> Text -> IO FDBR
--make_fdbr = getts_fdbr_entevprop_type 
    
    
