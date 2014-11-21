{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Getts where
import Data.List
import Control.Monad
import Debug.Trace

--For endpoint query
import Data.RDF hiding (triple, Triple)
import Database.HSparql.Connection
import Database.HSparql.QueryGenerator
import Data.Text hiding (head, concat, map, zip, drop, length)

import qualified Data.Map as M
import Data.IORef
import System.IO.Unsafe

addUri namespace_uri = (namespace_uri ++)

--getts returns all triples in the triple store that match the given parameters
class TripleStore m where
	getts_1 :: m -> (Event, String, String) -> IO [String]
	getts_2 :: m -> (Event, String, String) -> IO [String]
	getts_3 :: m -> (Event, String, String) -> IO [String]
	--getts_relation computes the relation [(x,e)] such that e is an event of type ev_type
	--and x is a subject of that event.  This is a default implementation that can be overridden
	--in instances.  Output is expected to be sorted.
	getts_relation :: m -> String -> String -> IO [(String, Event)]
	getts_relation ev_data ev_type entity_type = do
		evs <- getts_1 ev_data ("?", "type", ev_type)
		pairs <- liftM concat $ mapM (\ev -> do
			ents <- getts_3 ev_data (ev, entity_type,"?")
			return $ zip ents (repeat ev)) evs
		return $ sortFirst pairs
		
	--getts_inverse returns the entities of entity_type of the events in the list evs
	getts_inverse :: m -> String -> [Event] -> IO [String]
	getts_inverse ev_data en_type evs = liftM concat $ mapM (\ev -> getts_3 ev_data (ev, en_type, "?")) evs
	
	--getts_members returns
	getts_members :: (TripleStore m) => m -> String -> IO [String]
	getts_members ev_data set = do
		evs_with_set_as_object <- getts_1 ev_data ("?", "object", set)
		evs_with_type_membership <- getts_1 ev_data ("?", "type", "membership")
		getts_inverse ev_data "subject" $ intersect evs_with_set_as_object evs_with_type_membership
			

sortFirst = sortBy (\x y -> compare (fst x) (fst y))

type Event = String
type Triple = (Event, String, String)

--For the "In Program" triple store
instance TripleStore [Triple] where
	getts_1 ev_data ("?", b, c) = return [x | (x,y,z) <- ev_data, b == y, c == z]
	getts_2 ev_data (a, "?", c) = return [y | (x,y,z) <- ev_data, a == x, c == z]
	getts_3 ev_data (a, b, "?") = return [z | (x,y,z) <- ev_data, a == x, b == y]

data SPARQLBackend = SPARQL String String deriving (Ord, Eq)
	
--the String in this instance is to be the endpoint that you wish to query
instance TripleStore SPARQLBackend where
	--getts_1 =  getts_1''
	getts_1 = memoIO getts_1''
		where
		getts_1'' (SPARQL endpoint namespace_uri) ("?", b, c) = preprocess namespace_uri $ getts_1'(pack "?", pack (addUri namespace_uri b), pack (addUri namespace_uri c))
			where
			getts_1' :: (t, Text, Text) -> IO [[BindingValue]]
			getts_1' (a, b, c) = do
				 (Just s) <- selectQuery endpoint getts_1_query
				 return s
				 where
				   getts_1_query = do 
					  x <- var
					  triple x (iriRef b) (iriRef c)
					  distinct
					  return SelectQuery { queryVars = [x] }
	--getts_2 =  getts_2''
	getts_2 = memoIO getts_2''
		where
		getts_2'' (SPARQL endpoint namespace_uri) (a, "?", c) = preprocess namespace_uri $ getts_2'(pack (addUri namespace_uri a), pack "?", pack (addUri namespace_uri c))
			where
				getts_2' :: (Text, Text, Text) -> IO [[BindingValue]]
				getts_2' (a, b, c) = do
					 (Just s) <- selectQuery endpoint getts_2_query  
					 return s
					 where
					   getts_2_query = do 
						  x <- var
						  triple  (iriRef a) x (iriRef c)
						  distinct
						  return SelectQuery { queryVars = [x] }
	--getts_3 =  getts_3''
	getts_3 = memoIO getts_3''
		where
		getts_3'' (SPARQL endpoint namespace_uri) (a, b, "?") = preprocess namespace_uri $ getts_3'(pack (addUri namespace_uri a), pack (addUri namespace_uri b), pack "?")
			where
				getts_3' :: (Text, Text, Text) -> IO [[BindingValue]]
				getts_3' (a, b, c) = do
					 (Just s) <- selectQuery endpoint getts_3_query  
					 return s
					 where
					   getts_3_query = do 
						  x <- var
						  triple (iriRef a) (iriRef b) x
						  distinct
						  return SelectQuery { queryVars = [x] }
	--Efficient implementation of getts_relation for SPARQL backend
	getts_relation = memoIO''' getts_relation'''
		where
		getts_relation''' (SPARQL endpoint namespace_uri) ev_type en_type = do
				m <- selectQuery endpoint query
				case m of
					(Just res) -> return $ map (\[x, y] -> (removeUri namespace_uri $ deconstruct x, removeUri namespace_uri $ deconstruct y)) res
					Nothing -> return []
				where
					query :: Query SelectQuery
					query = do
						sol <- prefix (pack "sol") (iriRef (pack namespace_uri))
						ev <- var
						subj <- var
						triple ev (sol .:. (pack "type")) (sol .:. (pack ev_type))
						triple ev (sol .:. (pack en_type)) subj
						orderNext subj
						distinct
						return SelectQuery { queryVars = [subj,ev] }
	
	--Efficient implementation of getts_inverse for SPARQL backend
	getts_inverse = memoIO'' getts_inverse''
		where
		getts_inverse'' (SPARQL endpoint namespace_uri) en_type evs = do
			m <- selectQuery endpoint query
			case m of
				(Just res) -> return $ map (removeUri namespace_uri . deconstruct) $ concat res
				Nothing -> return []
			where
				query = do
					sol <- prefix (pack "sol") (iriRef (pack namespace_uri))
					subj <- var
					ev <- var
					triple ev (sol .:. (pack en_type)) subj
					filterExpr $ regex ev $ (pack $ Data.List.intercalate "|" (map (++ "$") evs))
					--Data.List.foldr1 Database.HSparql.QueryGenerator.union $ map (\ev -> triple (sol .:. pack(ev))  (sol .:. pack("subject")) subj) evs -- UNION nesting problem
					distinct
					return SelectQuery { queryVars = [subj] } 
				
	--Efficient implementation of getts_members for SPARQL backend
	
	getts_members = memoIO' getts_members'
	--getts_members = getts_members'
		where
		getts_members' (SPARQL endpoint namespace_uri) set = do
			m <- selectQuery endpoint query
			case m of
				(Just res) -> return $ map (removeUri namespace_uri . deconstruct) $ concat res
				Nothing -> return []
			where
				query :: Query SelectQuery
				query = do
					sol <- prefix (pack "sol") (iriRef (pack namespace_uri))
					ev <- var
					subj <- var
					triple ev (sol .:. (pack "type")) (sol .:. (pack "membership"))
					triple ev (sol .:. (pack "subject")) subj
					triple ev (sol .:. (pack "object")) (sol .:. (pack set))
					orderNext subj
					distinct
					return SelectQuery { queryVars = [subj] }
	
			
removeUri :: String -> String -> String			
removeUri namespace_uri = drop $ length namespace_uri
					  
preprocess :: String -> IO [[BindingValue]] -> IO [String]
preprocess namespace_uri bvals = bvals >>= \x -> return $ map (removeUri namespace_uri . deconstruct) $ concat x


deconstruct :: BindingValue -> String
deconstruct value = do
    let (Bound node) = value
    case node of
        UNode strURI -> unpack strURI
        LNode (PlainL strLit) -> unpack strLit



memotable :: IORef (M.Map (SPARQLBackend,(String,String,String)) [String])
memotable = unsafePerformIO $ newIORef M.empty

memoIO ::(SPARQLBackend -> (String, String, String) -> IO [String]) -> SPARQLBackend -> (String, String, String) -> IO [String]
memoIO f a x = do
	m <- readIORef memotable
	case M.lookup (a,x) m of
		Nothing -> f a x >>= \q -> (writeIORef memotable (M.insert (a,x) q m) >> return q)
		Just r -> return r
		
memotable' :: IORef (M.Map (SPARQLBackend,String) [String])
memotable' = unsafePerformIO $ newIORef M.empty

memoIO' ::(SPARQLBackend -> (String) -> IO [String]) -> SPARQLBackend -> (String) -> IO [String]
memoIO' f a x = do
	m <- readIORef memotable'
	case M.lookup (a,x) m of
		Nothing -> f a x >>= \q -> (writeIORef memotable' (M.insert (a,x) q m) >> return q)
		Just r -> return r
		
memotable'' :: IORef (M.Map (SPARQLBackend,String,[String]) [String])
memotable'' = unsafePerformIO $ newIORef M.empty

memoIO'' ::(SPARQLBackend -> String -> [String] -> IO [String]) -> SPARQLBackend -> String -> [String] -> IO [String]
memoIO'' f a x y = do
	m <- readIORef memotable''
	case M.lookup (a,x,y) m of
		Nothing -> f a x y >>= \q -> (writeIORef memotable'' (M.insert (a,x,y) q m) >> return q)
		Just r -> return r
		
memotable''' :: IORef (M.Map (SPARQLBackend,String,String) [(String,String)])
memotable''' = unsafePerformIO $ newIORef M.empty

memoIO''' ::(SPARQLBackend -> String -> String -> IO [(String,String)]) -> SPARQLBackend -> String -> String -> IO [(String,String)]
memoIO''' f a x y = do
	m <- readIORef memotable'''
	case M.lookup (a,x,y) m of
		Nothing -> f a x y >>= \q -> (writeIORef memotable''' (M.insert (a,x,y) q m) >> return q)
		Just r -> return r

		
--Get members of named set
get_members :: (TripleStore m) => m -> String -> IO [String]
get_members = getts_members
	
--Get all subjects of a given event type
get_subjs_of_event_type :: (TripleStore m) => m -> String -> IO [String]
get_subjs_of_event_type ev_data ev_type = do
	pairs <- make_image ev_data ev_type "subject"
	return $ map fst pairs


{-collect accepts a binary relation as input and computes the image of each
element in the projection of the left column of the relation, under the relation.

Note: Since == is an equivalence relation, if multiple elements in the projection
of the left column are in the same equivalence class, only the first member of that
equivalence class that appears in the input list (as the left hand column of a tuple)
will appear in the output (as the left hand column of a tuple).

That is, the first element of that equivalence class that appears in the input list
will be chosen to represent the entire equivalence class in the output.

-}
collect :: (Eq a, Ord a) => [(a, a)] -> [(a, [a])]
collect [] = []
collect ((x,y):t) =
	(x, y:[e2 | (e1, e2) <- t, e1 == x]) : collect [(e1, e2) | (e1, e2) <- t, e1 /= x]
	
--Faster collect: runs in n lg n time
collect2 = condense . sortFirst
	
--condense computes the image under a sorted relation
--condense runs in O(n) time and is lazy, also is lazy in the list computed in each tuple
condense :: (Eq a, Ord a) => [(a, a)] -> [(a, [a])]
condense [] = []
condense ((x,y):t) = (x, y:a):(condense r)
	where 
	(a, r) = findall x t
	findall x [] = ([], [])
	findall x list@((t,y):ts) | x /= t = ([], list)
	findall x ((t,y):ts) | x == t = let (a2, t2) = (findall x ts) in (y:a2, t2)


--make_image accepts an event type as input, determines all subjects for
--all events of that type, and computes the image under the relation
--for each subject as described in the collect function
--Arguments: triple store, Event type (i.e. join_rel), Entity type (i.e. subject, object)
make_image :: (TripleStore m) => m -> String -> String -> IO [(String,[String])]
make_image ev_data ev_type entity_type = do
	evSubjs <- getts_relation ev_data ev_type entity_type
	return $ condense $ evSubjs
	
	
