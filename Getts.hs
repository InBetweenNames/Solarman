{-# LANGUAGE FlexibleInstances #-}

module Getts where
import Data.List
import Control.Monad
import Debug.Trace

--For endpoint query
import Data.RDF hiding (triple, Triple)
import Database.HSparql.Connection
import Database.HSparql.QueryGenerator
import Data.Text hiding (head, concat, map, zip, drop, length)
import System.IO.Unsafe

addUri fragment = namespace_uri ++ fragment

endpoint_uri = "http://speechweb2.cs.uwindsor.ca/sparql"
namespace_uri = "http://solarman.richard.myweb.cs.uwindsor.ca#"

--getts returns all triples in the triple store that match the given parameters
class TripleStore m where
	getts_1 :: m -> (Event, String, String) -> IO [String]
	getts_2 :: m -> (Event, String, String) -> IO [String]
	getts_3 :: m -> (Event, String, String) -> IO [String]

type Event = String
type Triple = (Event, String, String)

--For the "In Program" triple store
instance TripleStore [Triple] where
	getts_1 ev_data ("?", b, c) = return [x | (x,y,z) <- ev_data, b == y, c == z]
	getts_2 ev_data (a, "?", c) = return [y | (x,y,z) <- ev_data, a == x, c == z]
	getts_3 ev_data (a, b, "?") = return [z | (x,y,z) <- ev_data, a == x, b == y]

data SPARQLBackend = SPARQL String
	
--the String in this instance is to be the endpoint that you wish to query
instance TripleStore SPARQLBackend where
	getts_1 (SPARQL endpoint) ("?", b, c) = return $ removeUri $ preprocess $ getts_1'(pack "?", pack (addUri b), pack (addUri c))
		where
			getts_1' :: (t, Text, Text) -> IO [[BindingValue]]
			getts_1' (a, b, c) = do
				 (Just s) <- selectQuery endpoint getts_1_query
				 return s
				 where
				   getts_1_query = do 
					  x <- var
					  triple x (iriRef b) (iriRef c)
					  return SelectQuery { queryVars = [x] }
					  
	getts_2 (SPARQL endpoint) (a, "?", c) = return $ removeUri $ preprocess  $ getts_2'(pack (addUri a), pack "?", pack (addUri c))
		where
			getts_2' :: (Text, Text, Text) -> IO [[BindingValue]]
			getts_2' (a, b, c) = do
				 (Just s) <- selectQuery endpoint getts_2_query  
				 return s
				 where
				   getts_2_query = do 
					  x <- var
					  triple  (iriRef a) x (iriRef c)
					  return SelectQuery { queryVars = [x] }
					  
	getts_3 (SPARQL endpoint) (a, b, "?") = return $ removeUri $ preprocess $ getts_3'(pack (addUri a), pack (addUri b), pack "?")
		where
			getts_3' :: (Text, Text, Text) -> IO [[BindingValue]]
			getts_3' (a, b, c) = do
				 (Just s) <- selectQuery endpoint getts_3_query  
				 return s
				 where
				   getts_3_query = do 
					  x <- var
					  triple (iriRef a) (iriRef b) x
					  return SelectQuery { queryVars = [x] }

	--addUri endpoint entry = namespace_uri ++ entry --the namespace of the entry
			
removeUri :: [String] -> [String]			
removeUri s = let l = length namespace_uri in
				map (drop l) s
					  
preprocess :: IO [[BindingValue]] -> [String]
preprocess = map deconstruct . concat . dropDups . unsafeDupablePerformIO

deconstruct :: BindingValue -> String
deconstruct value = do
    let (Bound node) = value
    case node of
        UNode strURI -> unpack strURI
        LNode (PlainL strLit) -> unpack strLit

dropDups :: (Eq a) => [a] -> [a]        
dropDups [] = []
dropDups (x:xs) = if elem x xs 
                    then dropDups xs
                    else x : dropDups xs

get_subjs_for_ev :: (TripleStore m) => m -> Event -> IO [String]
get_subjs_for_ev ev_data ev = getts_3 ev_data (ev, "subject", "?")

get_subjs_for_evs :: (TripleStore m) => m -> [Event] -> IO [String]
get_subjs_for_evs ev_data evs = liftM concat $ mapM (get_subjs_for_ev ev_data) evs

--Get members of named set
get_members :: (TripleStore m) => m -> String -> IO [String]
get_members ev_data set = do
	evs_with_set_as_object <- getts_1 ev_data ("?", "object", set)
	evs_with_type_membership <- getts_1 ev_data ("?", "type", "membership")
	get_subjs_for_evs ev_data $ intersect evs_with_set_as_object evs_with_type_membership
	
		
--Get all subjects of a given event type
get_subjs_of_event_type :: (TripleStore m) => m -> String -> IO [String]
get_subjs_of_event_type ev_data ev_type = do
		events <- getts_1 ev_data ("?", "type", ev_type)
		get_subjs_for_evs ev_data events


{-collect accepts a binary relation as input and computes the image of each
element in the projection of the left column of the relation, under the relation.

Note: Since == is an equivalence relation, if multiple elements in the projection
of the left column are in the same equivalence class, only the first member of that
equivalence class that appears in the input list (as the left hand column of a tuple)
will appear in the output (as the left hand column of a tuple).

That is, the first element of that equivalence class that appears in the input list
will be chosen to represent the entire equivalence class in the output.

-}
collect :: [(String, String)] -> [(String, [String])]
collect [] = []
collect ((x,y):t) =
	(x, y:[e2 | (e1, e2) <- t, e1 == x]) : collect [(e1, e2) | (e1, e2) <- t, e1 /= x]
		

--make_image accepts an event type as input, determines all subjects for
--all events of that type, and computes the image under the relation
--for each subject as described in the collect function
--Arguments: triple store, Event type (i.e. join_rel), Entity type (i.e. subject, object)
make_image :: (TripleStore m) => m -> String -> String -> IO [(String,[String])]
make_image ev_data ev_type entity_type = do
	evs <- getts_1 ev_data ("?", "type", ev_type)
	pairs <- liftM concat $ mapM (\ev -> do
		ents <- getts_3 ev_data (ev, entity_type,"?")
		return $ zip ents (repeat ev)) evs
	return $ collect pairs
	
	