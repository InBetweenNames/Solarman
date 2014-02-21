{-# LANGUAGE FlexibleInstances #-}

module Getts where
import Data.List


--getts returns all triples in the triple store that match the given parameters
class TripleStore m where
	getts_1 :: m -> (Event, String, String) -> [String]
	getts_2 :: m -> (Event, String, String) -> [String]
	getts_3 :: m -> (Event, String, String) -> [String]

type Event = String
type Triple = (Event, String, String)

--For the "In Program" triple store
instance TripleStore [Triple] where
	getts_1 ev_data ("?", b, c) = [x | (x,y,z) <- ev_data, b == y, c == z]
	getts_2 ev_data (a, "?", c) = [y | (x,y,z) <- ev_data, a == x, c == z]
	getts_3 ev_data (a, b, "?") = [z | (x,y,z) <- ev_data, a == x, b == y]


get_subjs_for_ev :: (TripleStore m) => m -> Event -> [String]
get_subjs_for_ev ev_data ev = getts_3 ev_data (ev, "subject", "?")

get_subjs_for_evs :: (TripleStore m) => m -> [Event] -> [String]
get_subjs_for_evs ev_data evs = concatMap (get_subjs_for_ev ev_data) evs


--Get members of named set
get_members :: (TripleStore m) => m -> String -> [String]
get_members ev_data set = get_subjs_for_evs ev_data $
	intersect evs_with_set_as_object evs_with_type_membership
	where
		evs_with_set_as_object = getts_1 ev_data ("?", "object", set)
		evs_with_type_membership = getts_1 ev_data ("?", "type", "membership")
		
--Get all subjects of a given event type
get_subjs_of_event_type :: (TripleStore m) => m -> String -> [String]
get_subjs_of_event_type ev_data ev_type
	= get_subjs_for_evs ev_data (getts_1 ev_data ("?", "type", ev_type))


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
make_image :: (TripleStore m) => m -> String -> String ->[(String,[String])]
make_image ev_data ev_type entity_type =
	collect [(ent, ev) | ev <- evs, ent <- getts_3 ev_data (ev, entity_type,"?") ]
	where evs = getts_1 ev_data ("?", "type", ev_type)
	
