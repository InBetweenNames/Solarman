{-# LANGUAGE Trustworthy #-}
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
import Data.RDF hiding (triple, Triple)
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
    getts_1 :: m -> Triple -> IO [Text]
    getts_2 :: m -> Triple -> IO [Text]
    getts_3 :: m -> Triple -> IO [Text]
    --getts_fdbr_entevprop_type computes the function defined by the entity-event relation r
    --where r is the relation between entities of type entity_type
    --and events of type event_type
    --getts_fdbr_entevprop_type :: m -> Text -> Text -> IO FDBR
    --getts_fdbr_entevprop_type ev_data ev_type entity_type = do
    --    evs <- getts_1 ev_data ("?", "type", ev_type)
    --    getts_fdbr_entevprop ev_data entity_type evs
        
    getts_triples_entevprop_type :: m -> [Text] -> Text -> IO [Triple]
    
    --getts_fdbr_entevprop returns the entities of entity_type of the events in the list evs
    getts_fdbr_entevprop :: m -> Text -> [Event] -> IO FDBR
    getts_fdbr_entevprop ev_data entity_type evs = do
        pairs <- liftM concat $ mapM (\ev -> do
            ents <- getts_3 ev_data (ev, entity_type, "?")
            return $ zip ents (repeat ev)) evs
        return $ collect pairs
    
    --getts_members returns
    getts_members :: m -> Text -> IO FDBR
    {-getts_members ev_data set = do
        evs_with_set_as_object <- getts_1 ev_data ("?", "object", set)
        evs_with_type_membership <- getts_1 ev_data ("?", "type", "membership")
        getts_fdbr_entevprop ev_data "subject" $ intersect evs_with_set_as_object evs_with_type_membership-}
            

sortFirst = sortBy (\x y -> compare (fst x) (fst y))

getts_fdbr_entevprop_triples :: [Triple] -> Text -> FDBR
getts_fdbr_entevprop_triples ev_data entity_type 
  = collect $ map (\(x, _, z) -> (z, x)) $ List.filter (\(x, y, z) -> y == entity_type) ev_data

--For the "In Program" triple store
instance TripleStore [Triple] where
    getts_1 ev_data ("?", b, c) = return [x | (x,y,z) <- ev_data, b == y, c == z]
    getts_2 ev_data (a, "?", c) = return [y | (x,y,z) <- ev_data, a == x, c == z]
    getts_3 ev_data (a, b, "?") = return [z | (x,y,z) <- ev_data, a == x, b == y]
    getts_triples_entevprop_type ev_data propNames ev_type = return ev_data --no-op
    getts_members ev_data set = do
      evs_with_set_as_object <- getts_1 ev_data ("?", "object", set)
      evs_with_type_membership <- getts_1 ev_data ("?", "type", "membership")
      let evs = intersect evs_with_set_as_object evs_with_type_membership
      let setRel = [(y, x) | (x, y, _) <- ev_data, x `elem` evs, "subject" == y]
      return $ collect $ setRel

data SPARQLBackend = SPARQL String Text deriving (Ord, Eq)

endpointTable :: IORef (M.Map String String)
{-# NOINLINE endpointTable #-}
endpointTable = unsafeDupablePerformIO $ newIORef M.empty

lookupEndpoint :: String -> IO String
lookupEndpoint url = do
  m <- readIORef endpointTable
  case M.lookup (url) m of
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
    --getts_1 =  getts_1''
    getts_1 = memoIO getts_1''
        where
        getts_1'' (SPARQL endpoint namespace_uri) ("?", b, c) = preprocess namespace_uri $ getts_1'("?", addUri namespace_uri b, addUri namespace_uri c)
            where
            getts_1' :: (Text, Text, Text) -> IO [[BindingValue]]
            getts_1' (a, b, c) = do
                 resolvedEndpoint <- lookupEndpoint endpoint
                 (Just s) <- selectQuery resolvedEndpoint getts_1_query
                 return s
                 where
                   getts_1_query = do 
                      x <- var
                      triple x (iriRef b) (iriRef c)
                      distinct
                      selectVars [x]
    --getts_2 =  getts_2''
    getts_2 = memoIO getts_2''
        where
        getts_2'' (SPARQL endpoint namespace_uri) (a, "?", c) = preprocess namespace_uri $ getts_2'(addUri namespace_uri a,  "?", addUri namespace_uri c)
            where
                getts_2' :: (Text, Text, Text) -> IO [[BindingValue]]
                getts_2' (a, b, c) = do
                     resolvedEndpoint <- lookupEndpoint endpoint
                     (Just s) <- selectQuery resolvedEndpoint getts_2_query  
                     return s
                     where
                       getts_2_query = do 
                          x <- var
                          triple  (iriRef a) x (iriRef c)
                          distinct
                          selectVars [x]
    --getts_3 =  getts_3''
    getts_3 = memoIO getts_3''
        where
        getts_3'' (SPARQL endpoint namespace_uri) (a, b, "?") = preprocess namespace_uri $ getts_3'(addUri namespace_uri a, addUri namespace_uri b, "?")
            where
                getts_3' :: (Text, Text, Text) -> IO [[BindingValue]]
                getts_3' (a, b, c) = do
                     resolvedEndpoint <- lookupEndpoint endpoint
                     (Just s) <- selectQuery resolvedEndpoint getts_3_query  
                     return s
                     where
                       getts_3_query = do 
                          x <- var
                          triple (iriRef a) (iriRef b) x
                          distinct
                          selectVars [x]
    --Efficient implementation of getts_fdbr_entevprop_type for SPARQL backend
    {-getts_fdbr_entevprop_type = memoIO''' getts_fdbr_entevprop_type'''
        where
        getts_fdbr_entevprop_type''' (SPARQL endpoint namespace_uri) ev_type en_type = do
                resolvedEndpoint <- lookupEndpoint endpoint
                m <- selectQuery resolvedEndpoint query
                case m of
                    (Just res) -> return $ condense $ map (\[x, y] -> (removeUri namespace_uri $ deconstruct x, removeUri namespace_uri $ deconstruct y)) res
                    Nothing -> return []
                where
                    query :: Query SelectQuery
                    query = do
                        sol <- prefix ("sol") (iriRef namespace_uri)
                        ev <- var
                        subj <- var
                        triple ev (sol .:. "type") (sol .:. ev_type)
                        triple ev (sol .:. en_type) subj
                        orderNext subj
                        distinct
                        selectVars [subj, ev]-}
    
    
    getts_triples_entevprop_type (SPARQL endpoint namespace_uri) propNames ev_type = do
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
          triple ev (sol .:. "type") (sol .:. ev_type)
          filterExpr $ regex prop $ (T.intercalate "|" (map (`T.append` "$") propNames))
          selectVars [ev, prop, ent]



    --Efficient implementation of getts_fdbr_entevprop for SPARQL backend
    {-getts_fdbr_entevprop = memoIO'' getts_fdbr_entevprop''
        where
        getts_fdbr_entevprop'' (SPARQL endpoint namespace_uri) en_type evs = do
            resolvedEndpoint <- lookupEndpoint endpoint
            m <- selectQuery resolvedEndpoint query
            case m of
                (Just res) -> return $ condense $ map (\[x, y] -> (removeUri namespace_uri $ deconstruct x, removeUri namespace_uri $ deconstruct y)) res
                Nothing -> return []
            where
                query = do
                    sol <- prefix "sol" (iriRef namespace_uri)
                    subj <- var
                    ev <- var
                    triple ev (sol .:. en_type) subj
                    filterExpr $ regex ev $ (T.intercalate "|" (map (`T.append` "$") evs))
                    orderNext subj
                    distinct
                    selectVars [subj, ev]-}
                
    --Efficient implementation of getts_members for SPARQL backend
    
    getts_members = memoIO' getts_members'
    --getts_members = getts_members'
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



memotable :: IORef (M.Map (SPARQLBackend,Triple) [Text])
{-# NOINLINE memotable #-}
memotable = unsafeDupablePerformIO $ newIORef M.empty

memoIO ::(SPARQLBackend -> Triple -> IO [Text]) -> SPARQLBackend -> Triple -> IO [Text]
memoIO f a x = do
    m <- readIORef memotable
    case M.lookup (a,x) m of
        Nothing -> f a x >>= \q -> (writeIORef memotable (M.insert (a,x) q m) >> return q)
        Just r -> return r
        
memotable' :: IORef (M.Map (SPARQLBackend,Text) FDBR)
{-# NOINLINE memotable' #-}
memotable' = unsafeDupablePerformIO $ newIORef M.empty

memoIO' ::(SPARQLBackend -> Text -> IO FDBR) -> SPARQLBackend -> Text -> IO FDBR
memoIO' f a x = do
    m <- readIORef memotable'
    case M.lookup (a,x) m of
        Nothing -> f a x >>= \q -> (writeIORef memotable' (M.insert (a,x) q m) >> return q)
        Just r -> return r
        
memotable'' :: IORef (M.Map (SPARQLBackend,Text,[Text]) FDBR)
{-# NOINLINE memotable'' #-}
memotable'' = unsafeDupablePerformIO $ newIORef M.empty

memoIO'' ::(SPARQLBackend -> Text -> [Text] -> IO FDBR) -> SPARQLBackend -> Text -> [Text] -> IO FDBR
memoIO'' f a x y = do
    m <- readIORef memotable''
    case M.lookup (a,x,y) m of
        Nothing -> f a x y >>= \q -> (writeIORef memotable'' (M.insert (a,x,y) q m) >> return q)
        Just r -> return r
        
memotable''' :: IORef (M.Map (SPARQLBackend,Text,Text) FDBR)
{-# NOINLINE memotable''' #-}
memotable''' = unsafeDupablePerformIO $ newIORef M.empty

memoIO''' ::(SPARQLBackend -> Text -> Text -> IO FDBR) -> SPARQLBackend -> Text -> Text -> IO FDBR
memoIO''' f a x y = do
    m <- readIORef memotable'''
    case M.lookup (a,x,y) m of
        Nothing -> f a x y >>= \q -> (writeIORef memotable''' (M.insert (a,x,y) q m) >> return q)
        Just r -> return r

        
--Get members of named set
get_members :: (TripleStore m) => m -> Text -> IO FDBR
get_members = getts_members
    
--Get all subjects of a given event type
get_subjs_of_event_type :: (TripleStore m) => m -> Text -> IO FDBR
--get_subjs_of_event_type ev_data ev_type = make_fdbr ev_data ev_type "subject"
get_subjs_of_event_type ev_data ev_type = do
  rt <- getts_triples_entevprop_type ev_data ["subject"] ev_type
  return $ getts_fdbr_entevprop_triples rt "subject"

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
    
    
