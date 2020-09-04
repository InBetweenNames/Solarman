{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module XSaiga.Getts where
import Data.List as List
import qualified Data.Text as T
import Control.Monad
import Debug.Trace

--For slow collect
import qualified Data.Map.Strict as Map

--For endpoint query
import Data.RDF.Types hiding (triple, Triple)
import Database.HSparql.Connection
import Database.HSparql.QueryGenerator as HSparql

import qualified Data.Map as M
import Data.IORef

import Debug.Trace
import GHC.Generics
import Data.Aeson

import qualified Data.Text.Read as TR

--If Asterius is being used, override selectQuery with our own implementation
#ifdef ASTERIUS
import Asterius.Aeson
import Asterius.ByteString
import Asterius.Text
import Asterius.Types

import qualified Data.Text.IO as TIO
import qualified System.IO as SIO
#endif

type Cardinality = Int --if we need more than 2 billion entities, we can change this

#ifdef ASTERIUS
foreign import javascript safe "getts_sparql($1,$2)" asterius_getts_sparql :: JSString -> JSString -> IO JSString

foreign import javascript safe "getts_triples_entevprop_type($1,$2,$3)" asterius_getts_triples_entevprop_type :: JSVal -> JSArray -> JSString -> IO JSArray

--Needs to take in endpoint, namespace uri, preferably as JSON object.  Then needs to take in a set name.
--need to de-namespace here, not in the function?
--may be able to use hsparql's direct XML parsing functionality to save a bit of work
--better yet, do the work in Javascript and return the list of triples directly!!!
foreign import javascript safe "getts_triples_members($1,$2)" asterius_getts_triples_members :: JSVal -> JSString -> IO JSArray

foreign import javascript safe "getts_cardinality_allents($1,$2)" asterius_getts_cardinality_allents :: JSVal -> JSArray -> IO JSVal

--TODO: the following is the preferred way to use Asterius, but it does not work yet

{-
gettsSelectQuery :: String -> Query SelectQuery -> IO (Maybe [[BindingValue]])
gettsSelectQuery endpoint query = do
    js_triples_xml <- asterius_getts_sparql (toJSString endpoint) (toJSString $ createSelectQuery query)
    --SIO.putStrLn $ createSelectQuery query --This is crashing!
    return $ structureContent $ fromJSString $ js_triples_xml
-}
#else

gettsSelectQuery = selectQuery
#endif

addUri namespace_uri = (T.append namespace_uri)

type Event = T.Text
type Triple = (Event, T.Text, T.Text) --TODO: use Data.RDF representation?
type FDBR = [(T.Text,[Event])]
type GFDBR = [(T.Text, FDBR)]

--getts returns all triples in the triple store that match the given parameters
class TripleStore m where
    getts_triples_entevprop_type :: m -> [T.Text] -> T.Text -> IO [Triple]

    getts_triples_members :: m -> T.Text -> IO [Triple]

    getts_cardinality_allents :: m -> [T.Text] -> IO Int --gets #entities from triplestore

sortFirst = sortBy (\x y -> compare (fst x) (fst y))

--TODO: This is DANGEROUS.  Needs refactoring.
make_fdbr_with_prop :: [Triple] -> T.Text -> FDBR
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
        where
            getts_triples_entevprop ev_data propNames evs =
                return $ List.filter (\(ev, prop, _) -> ev `elem` evs && prop `elem` ("type":propNames)) ev_data

    --TODO: can getts_triples_members just be implemented in terms of getts_triples_entevprop_type?
    --getts_triples_members ev_data set = getts_triples_entevprop_type ev_data ["subject", "object"] "membership"
    --yes.  Actually, triplestore retrieval really only needs getts_triples_entevprop_type.
    --but note this increases bandwidth requirements as the entire membership relation is needed
    --perhaps could extend _type with a constraint in the future?
    --the pure functions still need all three, I think (maybe not _members...)

    getts_triples_members ev_data set = do
      let evs_with_set_as_object = getts_1 ev_data ("?", "object", set)
      let evs_with_type_membership = getts_1 ev_data ("?", "type", "membership")
      let evs = intersect evs_with_set_as_object evs_with_type_membership
      --TODO: abstract triple filtering mechanism for events and ev_types
      let triples = [(x, y, z) | (x, y, z) <- ev_data, x `elem` evs]
      return $ triples

    getts_cardinality_allents ev_data props = do
        let propEvs = [(x, y, z) | (x, y, z) <- ev_data, y `elem` props]
        return $ List.length $ propEvs

--TODO: note, the pure versions do NOT need to include type information
--TODO: ev_data should come last, not first
pure_getts_triples_entevprop_type ev_data propNames ev_type =
  pure_getts_triples_entevprop ev_data propNames evs_with_type_ev_type
  where
    evs_with_type_ev_type = getts_1 triples ("?", "type", ev_type)
    triples = fst ev_data

pure_getts_triples_entevprop (triples,_) propNames evs
  = List.filter (\(ev, prop, _) -> ev `elem` evs && prop `elem` propNames) triples

pure_getts_members (triples,_) set = collect $ setRel
  where
    evs_with_set_as_object = getts_1 triples ("?", "object", set)
    evs_with_type_membership = getts_1 triples ("?", "type", "membership")
    evs = intersect evs_with_set_as_object evs_with_type_membership
    setRel = [(z, x) | (x, y, z) <- triples, x `elem` evs, "subject" == y]


data SPARQLBackend = SPARQL {sparqlEndpoint :: String, sparqlNamespace :: T.Text} deriving (Generic)
instance ToJSON SPARQLBackend where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON SPARQLBackend

--TODO: can't use hsparql with asterius for unknown reason, likely a compiler error
#ifdef ASTERIUS

instance TripleStore SPARQLBackend where
    --These need to use the asterius_getts_sparql function... or perhaps, we could cook something else up right in Javascript
    --I imagine there are Javascript libraries to do SPARQL queries, after all!
    getts_triples_entevprop_type triplestore@(SPARQL endpoint namespace_uri) propNames ev_type = do
        js_bindings_array <- asterius_getts_triples_entevprop_type (jsonToJSVal triplestore) (toJSArray $ map jsonToJSVal propNames) (textToJSString ev_type)
        let js_bindings = fromJSArray js_bindings_array
        let bindings = map (\x -> jsonFromJSVal' x :: (T.Text, T.Text, T.Text)) js_bindings
        return $ List.concatMap (\(ev,prop,ent) -> [(ev, "type", ev_type), (ev, prop, ent)]) bindings

    getts_triples_members triplestore@(SPARQL endpoint namespace_uri) set = do
        js_bindings_array <- asterius_getts_triples_members (jsonToJSVal triplestore) (textToJSString set)
        let js_bindings = fromJSArray js_bindings_array
        let bindings = map (\x -> jsonFromJSVal' x :: (T.Text, T.Text)) js_bindings
        --now we have the [(ev,ent)] bindings from before, already de-namespaced and deconstructed in javascript
        return $ List.concatMap (\(ev,ent) -> [(ev, "type", "membership"), (ev, "subject", ent), (ev, "object", set)]) bindings

    getts_cardinality_allents triplestore@(SPARQL endpoint namespace_uri) propNames = do
        x <- asterius_getts_cardinality_allents (jsonToJSVal triplestore) (toJSArray $ map jsonToJSVal propNames)
        let count = jsonFromJSVal' x :: Cardinality
        return $ count

    {-getts_triples_members triplestore@(SPARQL endpoint namespace_uri) set = do
        js_triples_xml <- asterius_getts_triples_members triplestore set
        return $ case structureContent $ fromJSString $ js_triples_xml of
            Just res -> Prelude.concatMap (\[ev, ent] ->
          [(removeUri namespace_uri $ deconstruct ev, "type", "membership"),
          (removeUri namespace_uri $ deconstruct ev, "subject", removeUri namespace_uri $ deconstruct ent),
          (removeUri namespace_uri $ deconstruct ev, "object", set)]) res
            Nothing -> []-}

#else
--the String in this instance is to be the endpoint that you wish to query
instance TripleStore SPARQLBackend where
    getts_triples_entevprop_type (SPARQL endpoint namespace_uri) propNames ev_type = do
      m <- gettsSelectQuery endpoint query
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

    getts_triples_members (SPARQL endpoint namespace_uri) set = do
      m <- gettsSelectQuery endpoint query
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

    getts_cardinality_allents (SPARQL endpoint namespace_uri) propNames = do
        m <- gettsSelectQuery endpoint query
        case m of
            Just res -> return $ deconstruct_card $ head $ head res
            Nothing -> error "could not obtain cardinality"
        where
            query :: Query SelectQuery
            query = do
                sol <- prefix "sol" (iriRef namespace_uri)
                ev <- var
                prop <- var
                ent <- var
                triple ev prop ent
                filterExpr $ List.foldr1 (.||.) $ map ((prop .==.) . (sol .:. )) propNames --type required here as this is not an FDBR
                c <- var
                distinct
                HSparql.select [count ent `as` c]
#endif
    --Used to return FDBR directly
    --Efficient implementation of getts_members for SPARQL backend
    {-getts_members = getts_members'
        where
        getts_members' (SPARQL endpoint namespace_uri) set = do
            m <- selectQuery endpoint query
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

removeUri :: T.Text -> T.Text -> T.Text
removeUri namespace_uri = T.drop $ T.length namespace_uri

preprocess :: T.Text -> IO [[BindingValue]] -> IO [T.Text]
preprocess namespace_uri bvals = bvals >>= \x -> return $ map (removeUri namespace_uri . deconstruct) $ concat x


deconstruct :: BindingValue -> T.Text
deconstruct value = do
    let (Bound node) = value
    case node of
        UNode strURI -> strURI
        LNode (PlainL strLit) -> strLit

deconstruct_card :: BindingValue -> Int
deconstruct_card  (Bound (LNode (TypedL strLit "http://www.w3.org/2001/XMLSchema#integer"))) = let (Right (x,_)) = TR.decimal strLit in x
deconstruct_card _ = error "Attempted to deconstruct a non-integer"

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


