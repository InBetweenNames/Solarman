{-# LANGUAGE FlexibleInstances #-}

module Getts where
import Data.List
import Control.Monad

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