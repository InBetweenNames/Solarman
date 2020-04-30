{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}

module XSaiga.SolarmanTriplestore where

import Prelude hiding ((*>), words, unwords, concat, concatMap, null)
import XSaiga.Getts
import Data.List as List hiding (words, unwords)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import qualified Data.Set as Set
import XSaiga.AGParser2
import XSaiga.TypeAg2
import Control.Monad
import Debug.Trace
import XSaiga.ShowText
import Control.Applicative hiding ((*>), (<|>))
import Data.Biapplicative
import Data.Bifunctor
import qualified Data.Map.Strict as Map
import qualified Data.HashMap.Strict as HashMap
import Control.Monad.State.Strict
import qualified Data.Ord as Ord
import qualified Data.Maybe as Maybe
import Data.Foldable
import qualified Data.Vector as Vector

--copied from gangster_v4: utility functions for making lists unique
subset s t = (s \\ t) == []

--TODO: MERGE IMAGES PROPER

--copied from gangster_v4: combinators
termor' :: SemFunc ((TF FDBR -> TF FDBR) -> (TF FDBR -> TF FDBR) -> TF FDBR -> TF FDBR)
termor' = (liftA2 . liftA2 $ union_fdbr'') >|< liftA2 (GettsUnion GU_NounOr)

termor :: (TFMemo FDBR -> TFMemo FDBR) -> (TFMemo FDBR -> TFMemo FDBR) -> TFMemo FDBR -> TFMemo FDBR
termor = liftA2 $ wrapS2 $ (liftA2 union_fdbr'') >|< (GettsUnion GU_NounOr)
--termor = liftA2 union_fdbr

--see MSc thesis for explanation of why termand is in terms of termor
termand'' :: FDBR -> FDBR -> FDBR
termand'' nph vbph = if not (List.null $ nph) && not (List.null $ vbph) then union_fdbr'' nph vbph else []

--May need to be changed to intersection?  Don't think so:  can't remove anything from nub (t1++t2) because all things are relevant to either t1 or t2
--TODO: MERGE IMAGES PROPER (or do termphrases always preserve ents)
termand' :: SemFunc ((TF FDBR -> TF FDBR) -> (TF FDBR -> TF FDBR) -> TF FDBR -> TF FDBR)
termand' = (liftA2 . liftA2 $ termand'') >|< liftA2 (GettsUnion GU_NounAnd)

termand :: (TFMemo FDBR -> TFMemo FDBR) -> (TFMemo FDBR -> TFMemo FDBR) -> TFMemo FDBR -> TFMemo FDBR
termand = liftA2 $ wrapS2 $ (liftA2 termand'') >|< (GettsUnion GU_NounAnd)

--TODO: FDBRs are sorted.  Use that to improve this.
intersect_fdbr'' _ [] = []
intersect_fdbr'' [] _ = []
intersect_fdbr'' fdbr1@((e1, evs1):eei1) fdbr2@((e2, evs2):eei2)
  = case compare e1 e2 of
      LT -> intersect_fdbr'' eei1 fdbr2
      EQ -> (e2, evs2):(intersect_fdbr'' eei1 eei2)
      GT -> intersect_fdbr'' fdbr1 eei2

{-intersect_fdbr'' eei1 eei2
  = [(subj2, evs2) | (subj1, evs1) <- eei1, (subj2, evs2) <- eei2, subj1 == subj2]-}

intersect_fdbr' :: SemFunc (TF FDBR -> TF FDBR -> TF FDBR)
intersect_fdbr' = liftA2 intersect_fdbr'' >|< GettsIntersect GI_NounAnd

intersect_fdbr = wrapS2 intersect_fdbr'

union_fdbr'' :: FDBR -> FDBR -> FDBR
union_fdbr'' fdbr1 fdbr2 = Map.toList $ Map.fromListWith (++) (fdbr1 ++ fdbr2)

union_fdbr' :: SemFunc (TF FDBR -> TF FDBR -> TF FDBR)
union_fdbr' = liftA2 union_fdbr'' >|< GettsUnion GU_NounOr

union_fdbr = wrapS2 union_fdbr'

nounand = intersect_fdbr

that = nounand

--TODO: MERGE IMAGES PROPER (verify)

nounor = union_fdbr

{-a' nph vbph =
    length (intersect  nph vbph) /= 0-}
a = intersect_fdbr
any' = a
the = a
some = a
an = a

every'' :: FDBR -> FDBR -> FDBR
every'' nph vbph | subset (map fst nph) (map fst vbph) = intersect_fdbr'' nph vbph
                | otherwise = []

every' :: SemFunc (TF FDBR -> TF FDBR -> TF FDBR)
every' = liftA2 every'' >|< GettsIntersect GI_Every

every = wrapS2 every'

most'' :: FDBR -> FDBR -> FDBR
most'' nph vbph = if n_nph /= 0 && (n_nph_v / n_nph) > 0.5 then nph_v else []
  where
    nph_v = intersect_fdbr'' nph vbph
    n_nph = fromIntegral $ length nph
    n_nph_v = fromIntegral $ length nph_v

most' :: SemFunc (TF FDBR -> TF FDBR -> TF FDBR)
most' = liftA2 most'' >|< GettsIntersect GI_Most

most = wrapS2 most'

{- TODO:
no' nph vbph =
    length (intersect nph vbph) == 0
no = liftM2 no'
-}

{- TODO:
none' nph vbph =
    no nph vbph
none = liftM2 none'
-}

one'' :: FDBR -> FDBR -> FDBR
one'' nph vbph   | length res == 1 = res
                | otherwise = []
    where
      res = intersect_fdbr'' nph vbph

one' :: SemFunc (TF FDBR -> TF FDBR -> TF FDBR)
one' = liftA2 one'' >|< GettsIntersect (GI_Number 1)

one = wrapS2 one'

two'' nph vbph   | length res == 2 = res
                | otherwise = []
    where
      res = intersect_fdbr'' nph vbph

two' :: SemFunc (TF FDBR -> TF FDBR -> TF FDBR)
two' = liftA2 two'' >|< GettsIntersect (GI_Number 2)

two = wrapS2 two'

--which nph vbph = if result /= [] then result else "none."
--  where result = unwords $ intersect nph vbph

which'' :: FDBR -> FDBR -> T.Text
which'' nph vbph = if not $ T.null result then result else "none."
  where
  result = T.unwords $ map fst $ intersect_fdbr'' nph vbph

which' :: SemFunc (TF FDBR -> TF FDBR -> TF T.Text)
which' = liftA2 which'' >|< GettsIntersect (GI_Which)

--which = liftS2 which'' (GettsIntersect (GI_Which))
--T.Text is not memoized
--need: unique name, but unmemoized result
--like liftS, but SKIPS memoization
which :: TFMemo FDBR -> TFMemo FDBR -> TFMemo T.Text
which = wrapT2 which'
--which (nph_tf, nph_g) (vbph_tf, vbph_g) = (f, g)
--    where
--        g = GettsIntersect (GI_Which) nph_g vbph_g
--        f = liftA2 (liftM2 which'') nph_tf vbph_tf

how_many'' nph vbph = tshow $ List.length (intersect_fdbr'' nph vbph)
how_many' = liftA2 how_many'' >|< GettsIntersect (GI_HowMany)

--TODO MEMO: would REALLY like to say liftM2 here to skip memoization
--but we can't because then it would not have a name and it would be top-level!
--how_many = liftS2 how_many'' (GettsIntersect (GI_HowMany))
how_many = wrapT2 how_many'
{-how_many (nph, g1) (vbph, g2) = (f, g)
    where
        g = GettsIntersect (GI_HowMany) g1 g2
        f = liftA2 (liftM2 how_many'') nph vbph-}

--who' = which' <<*>> (nounor' <<*>> (get_members "person") <<*>> (get_members "science_team"))

who = which ((get_members "person") `nounor` ((get_members "science_team")))

--New
what'' nph = if not $ T.null result then result else "nothing."
    where result = T.unwords $ map fst nph

what' = fmap what'' >|< id

--what (tf, tf_g) = (f, tf_g)
--    where
--        f = liftA (liftM what'') tf

--is "id" correct here?
what = wrapT1 what'

--TODO: prepositions
make_prep props tmph = (props, Nothing, tmph)

make_prep_nph' props nph = (props, Nothing, intersect_fdbr' <<*>> nph)

make_prep_nph props nph = (props, Nothing, intersect_fdbr nph)

make_prep_superph props (ord, tmph) = (props, Just ord, tmph)

--with :: (TF FDBR -> TF FDBR) -> ([T.Text], TF FDBR -> TF FDBR)
with = make_prep ["with_implement"]

by = make_prep ["subject"]

at = make_prep ["location"]

make_pnoun'' noun image = [(subj, evs) | (subj, evs) <- image, subj == noun]

make_pnoun' :: T.Text -> SemFunc (TF FDBR -> TF FDBR)
make_pnoun' noun = (fmap $ make_pnoun'' noun) >|< GettsIntersect GI_NounAnd (GettsPNoun noun)

make_pnoun noun = wrapS1 $ make_pnoun' noun

--TODO: ugly hack to work around parser problem
make_year' = make_pnoun' . tshow

make_year = make_pnoun . tshow

in_loc' = make_prep ["location"]
in_year' = make_prep ["year"]

--TODO: verify "subject" and identity here.  should not be introducing more info...
--to = (\nph -> (["subject"], intersect_fdbr' nph)) >|< id
to = make_prep_nph ["subject"]

--New for new new semantics

--Strategy: collect all events, get all triples of those evs with prop
--use getts_fdbr_entevprop or similar to...

{-make_prop_termphrase :: (TripleStore m) => m -> T.Text -> TF FDBR -> TF T.Text
make_prop_termphrase ev_data prop nph = do
  list <- nph
  let evs = List.nub $ List.concatMap snd list
  rtriples  <- getts_triples_entevprop ev_data [prop] evs
  let finalList = T.unwords $ List.nub $ map (\(x,y,z) -> z) rtriples
  return $ if not $ T.null finalList then finalList else "nothing."
-}

make_prop_termphrase' :: T.Text -> TF FDBR -> TF T.Text
make_prop_termphrase' prop nph triples = if not $ T.null finalList then finalList else "nothing."
  where
  evs = List.nub $ List.concatMap snd (nph triples)
  rtriples = pure_getts_triples_entevprop triples [prop] evs
  finalList = T.unwords $ List.nub $ map (\(x,y,z) -> z) rtriples

make_prop_termphrase_ :: T.Text -> SemFunc (TF FDBR -> TF T.Text)
make_prop_termphrase_ prop = make_prop_termphrase' prop >|< (GettsPropTmph prop . GettsAttachP prop)

--TODO: how to memoize text? WE DON'T
make_prop_termphrase :: T.Text -> TFMemo FDBR -> TFMemo T.Text
--make_prop_termphrase prop (tf, g) = liftW (make_prop_termphrase'' prop) (GettsPropTmph prop . GettsAttachP prop)
make_prop_termphrase prop = wrapT1 $ make_prop_termphrase_ prop

--NOTE: we could simplify by taking in SemFunc things and lifting directly
--would need to use const to facilitate the memoization
--what is the cost?
--we could bridge FDBR -> FDBR -> ... when possible and TF FDBR -> TF FDBR -> ... everywhere else
--or even go one step further and require a TF FDBR interface

where' = make_prop_termphrase "location"
when' = make_prop_termphrase "year"
how' = make_prop_termphrase "with_implement"

findFirstObj (GettsIntersect _ _ y) = findFirstObj y
findFirstObj (GettsUnion _ _ y) = findFirstObj y
findFirstObj (GettsTP _ (_,_,object) _) = object

--needs special handling due to semantics requiring info from getts
whatobj' :: SemFunc (TF FDBR) -> SemFunc (TF T.Text)
whatobj' (tf, getts) = make_prop_termphrase' prop tf >|< GettsPropTmph prop (GettsAttachP prop getts)
  where
    prop = findFirstObj getts

--top level things should ALWAYS be named
whatobj :: TFMemo FDBR -> TFMemo T.Text
whatobj x = let g = getGetts x in make_prop_termphrase (findFirstObj g) x

--end of copied from gangster_v4

{-
sun     = get_members dataStore ("sun")
planet  = get_members dataStore ("planet")
moon    = get_members dataStore ("moon")
person  = get_members dataStore ("person")
thing   = get_members dataStore ("thing")
telescope = get_members dataStore ("telescope")
science_team = get_members dataStore ("science_team")
spacecraft = get_members dataStore ("spacecraft")
place = get_members dataStore ("place")

atmospheric = get_members dataStore ("atmospheric")
blue = get_members dataStore ("blue")
depressed = get_members dataStore ("depressed")
solid = get_members dataStore ("solid")
brown   = get_members dataStore ("brown")
gaseous = get_members dataStore ("gaseous")
green   = get_members dataStore ("green")
red     = get_members dataStore ("red")
ringed  = get_members dataStore ("ringed")
vacuumous = get_members dataStore ("vacuumous")
exists = thing
spin   = get_members dataStore ("spin")
-}

{-
discover_intrans    = get_subjs_of_event_type dataStore ("discover_ev")
orbit_intrans       = get_subjs_of_event_type dataStore ("orbit_ev")
-}

{-discover = make_trans_active "discover_ev"
discovered = discover

orbit = make_trans_active "orbit_ev"
orbited = orbit-}

--For prepositional phrases
{-discover' = make_trans_active "discover_ev"
discovered' = discover'

orbit' = make_trans_active "orbit_ev"
orbited' = orbit'-}

{-hall = make_pnoun "hall"
phobos = make_pnoun "phobos"
mars = make_pnoun "mars"
refractor_telescope_1 = make_pnoun "refractor_telescope_1"
-}


{-make_inverted_relation :: (TripleStore m) => m -> String -> (IO [String] -> IO Bool) -> IO [String]
make_inverted_relation ev_data rel tmph = do
        images <- make_fdbr ev_data rel "object"
        objPairs <- filterM (\(_, evs) ->
            tmph $ liftM concat $ mapM (\ev -> getts_3 ev_data (ev, "subject", "?")) evs) images
        return $ map fst objPairs-}

--Prepositional filtering

--filter_ev takes a list of ([String], IO [String] -> IO Bool) where the [String] is the list of identifiers corresponding
--to the data the preposition predicate needs to evaluate (e.g., for location predicates the list will contain "location",
{-filter_ev :: (TripleStore m) => m -> [([String], IO [String] -> IO Bool)] -> Event -> IO Bool
filter_ev ev_data [] ev = return True
filter_ev ev_data ((names,pred):list) ev = do
    relevant_list <- mapM (\name -> getts_3 ev_data (ev, name, "?")) names
    res <- pred $ return $ concat relevant_list
    if res then filter_ev ev_data list ev else return False-}

--Modified filter_ev to accommodate predicates like every, one, two, etc...
--The difference is that it concatenates the data the preposition predicate needs to evaluate across all events and
--applies the preposition predicate to that, so that all data is available to the predicate rather than just the subset
--given by a specific event
--TODO: new filter_Ev
{-filter_ev :: (TripleStore m) => m -> [([String], TF FDBR -> TF FDBR)] -> [Event] -> IO Bool
filter_ev _ [] _ = return True
filter_ev ev_data ((names,pred):list) evs = do
    relevant_list <- mapM (\name -> getts_fdbr_entevprop ev_data name evs) names
    res <- pred $ return $ concat $ relevant_list
    if res /= [] then filter_ev ev_data list evs else return False-}

--new filter_ev: Handles prepositional phrases (IN TESTING)
--TODO: handle superlatives =
filter_ev' :: [([T.Text], Maybe Ordering, SemFunc (TF FDBR -> TF FDBR))] -> [Event] -> TF [Event]
filter_ev' [] evs ev_data = evs
filter_ev' ((names,_,pred):list) evs triples
  = if not $ List.null res then filter_ev' list relevant_evs triples else []
  where
  relevant_triples = List.filter (\(x, _, _) -> x `elem` evs) triples -- only get triples with our events
  relevant_list = concatMap (\name -> make_fdbr_with_prop relevant_triples name) names
  res = (fst pred) (pure relevant_list) triples --TODO: prove correct (TODO USE getGetts AS WELL FOR MEMOIZATION!!!)
  --NEW: Merge all events in predicate result for new query.  Result will be a subset of evs.
  relevant_evs = List.nub $ concatMap snd res

--NOTE MEMO: Don't bother memoizing the individual rows, just make sure memoized termphs are used and the actual transvb itself is memoized
--It is incredibly unlikely the same list of evs will occur twice
--Then again, I suppose these could be precomputed... can we do that without messing around here?
--Is this a case for intersect_fdbr :: TFMemo a = ([Triple] -> State (Map.Map GettsTree a), Maybe GettsTree)?
--What's a good name for GettsFilterEv?  How do we deal with the recursion?
--Temporary workaround: don't bother memoizing the list of events
filter_ev :: [([T.Text], Maybe Ordering, (TFMemo FDBR -> TFMemo FDBR))] -> [Event] -> [Triple] -> State (Map.Map GettsTree FDBR) [Event]
filter_ev list evs triples = foldrM filt evs list
    where
        filt _ [] = return []
        filt (names, _, pred) evs = do
            let pred_tf = getSem $ pred (make_pred_arg_memo evs names) --skips memoizing the result
            --Memoization already happened in pred_tf thankfully
            res <- pred_tf triples
            let relevant_evs = List.nub $ concatMap snd res
            return relevant_evs

    --NEW: Merge all events in predicate result for new query.  Result will be a subset of evs.

make_pred_arg_pure :: [Event] -> [T.Text] -> TF FDBR
make_pred_arg_pure evs names triples = let relevant_triples = List.filter (\(x, _, _) -> x `elem` evs) triples -- only get triples with our events
                                        in concatMap (\name -> make_fdbr_with_prop relevant_triples name) names

--Previously, we allowed things to not have names, and unnamed things inhibit memoization in expressions using them
--this was used to kill memoization in prepositional phrases for the prop-FDBRs produced there as it was believed it would hurt performance
--but this turned out not to be the case.
--In that scheme, having a name and being memoized were separate things and 4 cases emerged: unnamed, unmemoized; named, unmemoized...; etc
--it allowed fine grained control over memoization in transitive verbs, but also introduced increased complexity and the possibility of the top-level expression tree
--being unnamed. that would have been a problem for doing the Getts* initial queries as that name is used to produce the flattened getts queries.
--so, asserts were in the code to try to catch this.  as a side effect, it made TFMemo itself a monad transformer where using it would remove any name.
--but that turned out to not be useful as generally you want names for things.
--the new scheme is that everything is memoized, as empirically this was better, and everything is named.
--make_pred_arg_unmemo evs names = wrapU0 (make_pred_arg_pure evs names)
make_pred_arg_memo evs names = wrapS0 (make_pred_arg_pure evs names, GettsPropFDBR names evs)

--TODO: can we memoize this?  could we perhaps have a GFDBR table?
make_gfdbr' :: [T.Text] -> FDBR -> TF GFDBR
make_gfdbr' props fdbr triples = gfdbr
    where
        --fdbr = fdbr_func triples
        relevant_triples triples evs = List.filter (\(x, _, _) -> x `elem` evs) triples -- only get triples with our events TODO OPTIMIZE -- this is done TWICE! (this sure looks like filter_ev)
        expand_evs triples evs = concatMap (\prop -> make_fdbr_with_prop (relevant_triples triples evs) prop) props
        gfdbr = map (\(ent, evs) -> (ent, expand_evs triples evs)) fdbr

--TODO memo

make_gfdbr :: [T.Text] -> FDBR -> [Triple] -> State (Map.Map GettsTree FDBR) GFDBR
make_gfdbr props fdbr triples
        = forM fdbr (\(ent, evs) -> do
                    let prop_g = GettsPropFDBR props evs
                    s <- get
                    case Map.lookup prop_g s of
                        Just fdbr -> return (ent, fdbr)
                        Nothing -> do
                            let relevant_triples = List.filter (\(x, _, _) -> x `elem` evs) triples
                            let prop_fdbr = concatMap (\prop -> make_fdbr_with_prop relevant_triples prop) props
                            modify (\s' -> Map.insert prop_g prop_fdbr s')
                            return (ent, prop_fdbr))


make_partition :: Ordering -> GFDBR -> [GFDBR]
make_partition ord gfdbr = map (map (\(_, ent, fdbr) -> (ent, fdbr))) $ groupBy equal $ sortBy (comparison ord) $ map (\(ent, fdbr) -> (length fdbr, ent, fdbr)) gfdbr
    where
        comparison EQ = error "EQ selected but has no associated meaning"
        comparison LT = Ord.comparing (\(len, _, _) -> len)
        comparison GT = flip (comparison LT)
        equal (len1, _, _) (len2, _, _) = len1 == len2

condense_gfdbr :: GFDBR -> FDBR
condense_gfdbr = map (\(ent, fdbr) -> (ent, concatMap snd fdbr))

filter_super' :: [([T.Text], Maybe Ordering, SemFunc (TF FDBR -> TF FDBR))] -> FDBR -> TF FDBR
filter_super' preps fdbr_start rtriples = foldr filt fdbr_start preps
    where
        filt (_, Nothing, _) fdbr = fdbr --do nothing if no ordering is required (e.g, ``in 1877'')
        filt (props, Just ord, _) fdbr = --here we do the actual ordering requirments for the superlative (termphrase is ignored because it has already been applied previously)
            let gfdbr = make_gfdbr' props fdbr rtriples
                sorted_parts = make_partition ord gfdbr
                maybe_top_gfdbr = Maybe.listToMaybe sorted_parts in
                    case maybe_top_gfdbr of
                        Just top_gfdbr -> condense_gfdbr top_gfdbr
                        Nothing -> []

--TODO MEMO: can't memoize GFDBRs just yet or filter_super, but can memoize the rows
--NOTE MEMO: May be able to memoize using fdbr_start?

filter_super :: [([T.Text], Maybe Ordering, TFMemo FDBR -> TFMemo FDBR)] -> FDBR -> [Triple] -> State (Map.Map GettsTree FDBR) FDBR
filter_super preps fdbr_start rtriples = foldrM filt fdbr_start preps
    where
        filt (_, Nothing, _) fdbr = return fdbr --do nothing if no ordering is required (e.g, ``in 1877'')
        filt (props, Just ord, _) fdbr = --here we do the actual ordering requirments for the superlative (termphrase is ignored because it has already been applied previously)
                do
                    gfdbr <- make_gfdbr props fdbr rtriples
                    let sorted_parts = make_partition ord gfdbr
                    let maybe_top_gfdbr = Maybe.listToMaybe sorted_parts in
                        case maybe_top_gfdbr of
                            Just top_gfdbr -> return $ condense_gfdbr top_gfdbr
                            Nothing -> return []


{-make_trans_active' :: (TripleStore m) => m -> String -> (IO [String] -> IO Bool) -> [([String], IO [String] -> IO Bool)] -> IO [String]
make_trans_active' ev_data rel tmph preps = do
    images <- make_fdbr ev_data rel "subject"
    subPairs <- filterM (\(_, evs) -> do
        filtEvents <- filterM (filter_ev ev_data preps) evs
        tmph $ liftM concat $ mapM (\ev -> getts_3 ev_data (ev, "object", "?")) filtEvents) images
    return $ map fst subPairs-}

{-prepProps :: [([T.Text], a)] -> [T.Text]
prepProps = nub . concatMap fst-}

{-gatherPreps :: [([T.Text], SemFunc (TF FDBR -> TF FDBR))] -> SemFunc [([T.Text], TF FDBR -> TF FDBR)]
gatherPreps preps = peelGetts preps >|< attachProps preps
  where
    peelGetts = map (\(propNames, sf) -> (propNames, getSem sf))
    extractGetts = foldr iunion GettsNone . map (\(propNames, sf) -> getGetts sf)
    attachProps preps = GettsPreps (nub $ concatMap fst preps) `iunion` (extractGetts preps) --Will contain AttachP info [propNames] and whatever else was in SemFunc(->)
-}

--Modified version of make_trans_active' to accomodate new filter_ev
{-make_trans_active' :: (TripleStore m) => m -> T.Text -> [([T.Text], SemFunc (TF FDBR -> TF FDBR))] -> TF FDBR
make_trans_active' ev_data rel preps = do
  triples <- getts_triples_entevprop_type ev_data ("subject":(prepProps preps)) rel
  let images = make_fdbr_with_prop triples "subject"
  fdbrRelevantEvs <- mapM (\(subj, evs) -> filter_ev triples preps evs >>= (\x -> return (subj, x))) images
  filterM (return . not . List.null . snd) fdbrRelevantEvs
-}

data Voice = ActiveVoice | PassiveVoice
getVoiceProps ActiveVoice (_, prop1, prop2) = (prop1, prop2)
getVoiceProps PassiveVoice (_, prop1, prop2) = (prop2, prop1)

gettsTP :: Voice -> Relation -> [GettsTree] -> GettsTree
gettsTP voice rel preps = GettsTP subject rel preps
  where
    (subject,_) = getVoiceProps voice rel

relname (a, _, _) = a

--TODO: need to modify this to actually use the ordering
make_trans''' :: Voice -> Relation -> [([T.Text], Maybe Ordering, SemFunc (TF FDBR -> TF FDBR))] -> TF FDBR
make_trans''' voice rel preps rtriples = ord_fdbr
  where
    (subjectProp,_) = getVoiceProps voice rel
    filtRTriples = pure_getts_triples_entevprop_type rtriples (subjectProp:(nub $ concatMap (\(a,_,_) -> a) $ preps)) (relname rel)
    images = make_fdbr_with_prop filtRTriples subjectProp
    fdbrRelevantEvs = map (\(subj, evs) -> (subj, filter_ev' preps evs rtriples)) images
    fdbr = filter (not . List.null . snd) fdbrRelevantEvs --TODO: this will make it so sets of events with a cardinality of 0 are not counted, leading to wrong "the least" behaviour
    --Now for superlatives.  All termphrases are applied first, and ordering happens after.
    ord_fdbr = filter_super' preps fdbr rtriples

make_trans'' :: Voice -> Relation -> [([T.Text], Maybe Ordering, TFMemo FDBR -> TFMemo FDBR)] -> TFMemo FDBR
make_trans'' voice rel preps = TFMemoT (f, g) --top level things ALWAYS have a name
    where
    g = gettsTP voice rel (gatherPreps preps)
    f rtriples = do
        s <- get
        case Map.lookup g s of
            Just fdbr -> return fdbr
            Nothing -> do
                let (subjectProp,_) = getVoiceProps voice rel
                let filtRTriples = pure_getts_triples_entevprop_type rtriples (subjectProp:(nub $ concatMap (\(a,_,_) -> a) $ preps)) (relname rel)
                let images = make_fdbr_with_prop filtRTriples subjectProp
                fdbrRelevantEvs <- mapM (\(subj, evs) -> filter_ev preps evs rtriples >>= (\x -> return (subj, x))) images
                let fdbr = filter (not . List.null . snd) fdbrRelevantEvs --TODO: this will make it so sets of events with a cardinality of 0 are not counted, leading to wrong "the least" behaviour
                --Now for superlatives.  All termphrases are applied first, and ordering happens after.
                res <- filter_super preps fdbr rtriples
                modify (\s' -> Map.insert g res s')
                return res

--make_trans_active' "discover_ev" <<*>> (gatherPreps [at us_naval_observatory, in' 1877])
--TODO: rtriples is used directly?? is this correct?
--TODO: refactor to take into account active tmph
--TODO: refactor both passive and active into same function (active version may involve more)

--SCHEME
--empty denotes tmph
--' denotes preps
--'' denotes tmph followed by preps

make_trans_active'_ :: Relation -> [([T.Text], Maybe Ordering, SemFunc (TF FDBR -> TF FDBR))] -> SemFunc (TF FDBR)
make_trans_active'_ rel preps =  make_trans''' ActiveVoice rel preps >|< gettsTP ActiveVoice rel (gatherPreps' preps)

make_trans_active' rel preps = make_trans'' ActiveVoice rel preps

--TODO: Bug in solarman3 semantics here with only "subject" in GettsTP
--make_trans_active :: T.Text -> SemFunc ((TF FDBR -> TF FDBR)  -> TF FDBR)
--make_trans_active ev_type = (\tmph_sem -> make_trans_active'' ev_type [(["object"], tmph_sem)]) >|< (\g -> GettsTP ["subject", "object"] ev_type [gettsApply g])

--make_trans_active :: Relation -> SemFunc (TF FDBR -> TF FDBR) -> SemFunc (TF FDBR)
make_trans_active rel tmph = make_trans_active' rel [([object], Nothing, tmph)]
  where
    (_, object) = getVoiceProps ActiveVoice rel

--make_trans_active'' :: Relation -> SemFunc (TF FDBR -> TF FDBR) -> [([T.Text], Maybe Ordering, SemFunc (TF FDBR -> TF FDBR))] -> SemFunc (TF FDBR)
make_trans_active'' rel tmph preps = make_trans_active' rel (([object], Nothing, tmph):preps)
  where
    (_, object) = getVoiceProps ActiveVoice rel

{-make_trans_passive' :: (TripleStore m) => m -> String -> [([String], IO [String] -> IO Bool)] -> IO [String]
make_trans_passive' ev_data rel preps = do
    images <- make_fdbr ev_data rel "object"
    objPairs <- filterM (\(_, evs) -> anyM (filter_ev ev_data preps) evs) images
    return $ map fst objPairs
    where
        anyM :: (Monad m) => (a -> m Bool) -> [a] -> m Bool
        anyM pred lst = foldM (\x y -> pred y >>= \res -> return $ x || res) False lst -}

--Modified version of make_trans_passive' to accomodate new filter_ev
{-make_trans_passive' :: (TripleStore m) => m -> T.Text -> [([T.Text], SemFunc (TF FDBR -> TF FDBR))] -> TF FDBR
make_trans_passive' ev_data rel preps = do
    triples <- getts_triples_entevprop_type ev_data ("object":(prepProps preps)) rel
    let images = make_fdbr_with_prop triples "object"
    fdbrRelevantEvs <- mapM (\(subj, evs) -> filter_ev triples preps evs >>= (\x -> return (subj, x))) images
    filterM (return . not . List.null . snd) fdbrRelevantEvs-}

make_trans_passive'_ :: Relation -> [([T.Text], Maybe Ordering, SemFunc (TF FDBR -> TF FDBR))] -> SemFunc (TF FDBR)
make_trans_passive'_ rel preps = make_trans''' PassiveVoice rel preps >|< gettsTP PassiveVoice rel (gatherPreps' preps)

make_trans_passive rel preps = make_trans'' PassiveVoice rel preps

--Copied from old solarman:
yesno'' x = if x /= [] then "yes." else "no"
yesno' :: SemFunc (TF FDBR -> TF T.Text)
yesno' = fmap yesno'' >|< GettsYesNo

yesno :: TFMemo FDBR -> TFMemo T.Text
yesno = wrapT1 yesno'

_truefalse'' x = if x /= [] then "true." else "false."
_truefalse' = fmap _truefalse'' >|< GettsYesNo

_truefalse :: TFMemo FDBR -> TFMemo T.Text
_truefalse = wrapT1 _truefalse'

does = yesno
did = yesno
do' = yesno
was = yesno
is = yesno
were = yesno
are = yesno

--TODO: is this proper?
{-sand s1 s2 = do
    r1 <- s1
    r2 <- s2
    return $ if r1 /= [] && r2 /= [] then List.nub $ r1 ++ r2 else []-}

--TODO: MERGE IMAGES PROPER (verify new impl)
--This is basically termand
sand'' [] _ = []
sand'' _ [] = []
sand'' fdbr1 fdbr2 = union_fdbr'' fdbr1 fdbr2

sand' = liftA2 sand'' >|< GettsUnion GU_NounAnd

sand = wrapS2 sand'

--TODO: testing
{-
moon = get_members "moon"
phobos = make_pnoun "phobos"
hall = make_pnoun "hall"
discover = make_trans_active "discover_ev"
discover' = make_trans_active' "discover_ev"
discover'' = make_trans_active'' "discover_ev"
discover_ = make_trans_passive "discover_ev"
telescope = get_members "telescope"
person = get_members "person"
planet = get_members "planet"
discoverer = get_subjs_of_event_type "discover_ev"
orbits = make_trans_active "orbit_ev"
orbits' = make_trans_active' "orbit_ev"
spins = get_members "spin"
-}

{-
||-----------------------------------------------------------------------------
||  BASIC INTERPRETERS
||-----------------------------------------------------------------------------
-}

--cnoun = superterminal Cnoun (\x -> [NOUNCLA_VAL $ get_members x])

--TODO: spellchecker?  if invalid word, refuse query
--TODO: do we need a lexer to tag things as pnouns, cnouns, etc?
--TODO: must be careful to not treat words like "who", "what", etc as pnouns!
--pnoun = superterminal Pnoun (\x -> Just [TERMPH_VAL $ make_pnoun x])
--TODO: this is a hack to exclude years and anything in the dictionary that is not a pnoun from being considered one
--Need a more sophisticated approach
pnoun           =  memoize_terminals_from_dictionary Pnoun
{-pnoun = superterminal Pnoun $ \x -> case TR.decimal x of
    Right (y, _) -> Nothing
    Left _ -> if x `elem` excluded then Nothing else Just [TERMPH_VAL $ make_pnoun x]
    where
        excluded = map (\(x, _, _) -> x) $ filter (\(_,type',_) -> type' /= Pnoun) dictionary
-}

--TODO: must take into account plurals, like moon/s, person/persons/people, satellite/satellites/moons etc.  needs synonym mapper.
cnoun           =  memoize_terminals_from_dictionary Cnoun
{-
cnoun = superterminal Cnoun $ \x -> case TR.decimal x of
    Right (y, _) -> Nothing
    Left _ -> if x `elem` excluded then Nothing else case Map.lookup x synonyms of
        --Note: discoverer is treated differently as the members are generated from the transitive verb subject
        --orbiter would be treated the same way
        --in general, these are nounphrases formed from transverbphrases?
        --an intransitive verb is similar (spinner, which comes from treating spin like a transvb)
        --all transitive verbs can be made to be intransitive
        --but not all intransitive verbs are transitive
        --should come up with a generation scheme:
        -- <verb>er/<verb>ers -- are intransitive
        -- <verb>ed/<verb>s -- are transitive, subject to synonym mapping
        -- <cnoun>/<couns>s -- are cnouns, subject to synonym mapping
        -- <pnoun> -- is pnoun
        -- exclude from the above "who/what/when/where/why/how" "a/one/two/..." "most/least" "in/at/by/using/with", "to", indefinite pronouns, "and/or/not", "does/did/do", "many"
        -- those words form the "solid" syntax of the semantics
        Nothing -> if x == "discoverer" || x == "discoverers" then Just $ [NOUNCLA_VAL $ get_subjs_of_event_type "discover_ev"] else Just $ [NOUNCLA_VAL $ get_members x]
        Just syn -> Just $ [NOUNCLA_VAL $ get_members syn]
    where
        excluded = map (\(x, _, _) -> x) $ filter (\(_,type',_) -> type' /= Cnoun) dictionary
        --TODO: make this smarter.  things that end with "s" tend to point to the original
        synonyms = Map.fromList [
            ("things", "thing"),
            ("planets","planet"),
            ("moons", "moon"),
            ("satellite", "moon"),
            ("satellites", "moon"),
            ("human", "person"),
            ("people", "person"),
            ("persons", "person"),
            ("team", "science_team"),
            ("teams", "science_team"),
            ("telescopes", "telescope"),
            ("spacecrafts", "spacecraft"),
            ("places", "place")]
-}
{-
    from the dictionary:
    ("thing",              Cnoun,     [NOUNCLA_VAL $ get_members "thing"]),
    ("things",             Cnoun,     [NOUNCLA_VAL $ get_members "thing"]),
    ("planets",            Cnoun,     [NOUNCLA_VAL $ get_members "planet"]),
    ("planet",             Cnoun,     [NOUNCLA_VAL $ get_members "planet"]),
    ("person",             Cnoun,     [NOUNCLA_VAL $ get_members "person"]),
    ("sun",                Cnoun,     [NOUNCLA_VAL $ get_members "sun"]),
    ("moon",               Cnoun,     [NOUNCLA_VAL $ get_members "moon"]),
    ("moons",              Cnoun,     [NOUNCLA_VAL $ get_members "moon"]),
    ("satellite",          Cnoun,     [NOUNCLA_VAL $ get_members "moon"]),
    ("satellites",         Cnoun,     [NOUNCLA_VAL $ get_members "moon"]),
    ("human",       Cnoun,    meaning_of nouncla "person" Nouncla),
    ("discoverer",  Cnoun,            [NOUNCLA_VAL $ get_subjs_of_event_type "discover_ev"]),
    ("discoverers", Cnoun,            [NOUNCLA_VAL $ get_subjs_of_event_type "discover_ev"]),
    ("humans",      Cnoun,    meaning_of nouncla "person" Nouncla),
    ("people",      Cnoun,    meaning_of nouncla "person" Nouncla),
    ("team",                            Cnoun, [NOUNCLA_VAL $ get_members "science_team"]),
    ("teams",                           Cnoun, [NOUNCLA_VAL $ get_members "science_team"]),
    ("telescope",   Cnoun, [NOUNCLA_VAL $ get_members "telescope"]),
    ("telescopes",  Cnoun, [NOUNCLA_VAL $ get_members "telescope"]),
    ("spacecraft",  Cnoun, [NOUNCLA_VAL $ get_members "spacecraft"]),
    ("spacecrafts", Cnoun, [NOUNCLA_VAL $ get_members "spacecraft"]),
    ("place",   Cnoun, [NOUNCLA_VAL $ get_members "place"]),
    ("places",  Cnoun, [NOUNCLA_VAL $ get_members "place"]),
-}

adj             =  memoize_terminals_from_dictionary Adj
--OK -- so to get sensible parses we end up needing a dictionary anyway, for now
--this seems to slow things down
{-
adj = superterminal Adj $ \x -> case TR.decimal x of
    Right (y, _) -> Nothing
    Left _ -> if x `elem` excluded then Nothing else Just [ADJ_VAL $ get_members x]
    where
        excluded = map (\(x, _, _) -> x) $ filter (\(_,type',_) -> type' /= Adj) dictionary
-}
det             =  memoize_terminals_from_dictionary Det
intransvb       =  memoize_terminals_from_dictionary Intransvb
transvb         =  memoize_terminals_from_dictionary Transvb
linkingvb       =  memoize_terminals_from_dictionary Linkingvb
relpron         =  memoize_terminals_from_dictionary Relpron
termphjoin      =  memoize_terminals_from_dictionary Termphjoin
verbphjoin      =  memoize_terminals_from_dictionary Verbphjoin
nounjoin        =  memoize_terminals_from_dictionary Nounjoin
indefpron       =  memoize_terminals_from_dictionary Indefpron
{-
terminator      =  uninterpreted (SPECIAL_SYMBOL_TERM ".")
                   $orelse
                   uninterpreted (SPECIAL_SYMBOL_TERM "?")
                   $orelse
                   uninterpreted (SPECIAL_SYMBOL_TERM "\n")
-}
sentjoin        =  memoize_terminals_from_dictionary Sentjoin
quest1          =  memoize_terminals_from_dictionary Quest1
quest2          =  memoize_terminals_from_dictionary Quest2
quest3          =  memoize_terminals_from_dictionary Quest3
quest4a         =  memoize_terminals_from_dictionary Quest4a
quest4b         =  memoize_terminals_from_dictionary Quest4b
quest5          =  memoize_terminals_from_dictionary Quest5
quest6          =  memoize_terminals_from_dictionary Quest6

--NEW FOR PREPOSITIONAL PHRASES
prep            =  memoize_terminals_from_dictionary Prepn
prepnph         =  memoize_terminals_from_dictionary Prepnph
prepyear        =  memoize_terminals_from_dictionary Prepyear
super           =  memoize_terminals_from_dictionary Super
superph_start   =  memoize_terminals_from_dictionary SuperphStart
--year            =  memoize_terminals_from_dictionary Year

--years are treated like termphrases.  but should they be?
--want to be able to ask "between 1877 and 1922"
--discovered in 1877 or at an observatory
--discovered by hall or kuiper
--discovered in greenwich or padua
--discovered at greenwich or padua
--discovered in 1877 or padua?  doesn't make sense
--discovered in 1877 at padua
--discovered in 1877 or 1922
--discovered between 1877 and 1922
year = superterminal Year $ \t -> case TR.decimal t of
    Right (y, _) -> Just [YEAR_VAL $ y]
    Left _ -> Nothing

--TODO: space leak here?  May be the foldr1, may be something else.  Stack needs 33KB.  Want to reduce it to 1KB
{-memoize_terminals_from_dictionary key
    = let key_words              = filter (\(_,type',_) -> type' == key) dictionary
          list_of_terms          = map (\(a, _, z) -> terminal a z) key_words
          altTerminals           = foldr1 (<|>) list_of_terms
    in  memoize key altTerminals-}

--Fixed space leak by just having it use a hashmap
memoize_terminals_from_dictionary :: MemoL -> NTType AttValue
memoize_terminals_from_dictionary key
  = let altTerminals           = terminalSet key dictionary'''
        --TODO: do we need to memoize this even?
        --TODO: if we need to memoize it, we should roll it into a higher level of abstraction
    in  memoize key altTerminals
    --in altTerminals

{-
test p = runState (p ((1,[]),input) ([],[])) []

main   = do putStr  $ render80 $ formatAtts Question $ snd $ test (question T0 [])

type Start1   = (Int, InsAttVals)
type Start    = ((Int,InsAttVals), [String])
type End      = (Int, InsAttVals)
type Atts     = [AttValue] -- [(AttType, AttValue)]
type InsAttVals = [(Instance, Atts)]

                 ,[(Start1,(Context,Result))]
type Mtable   = [(MemoL
                 )
                ]
type Result   = [((Start1, End),[Tree MemoL])]
||-----------------------------------------------------------------------------
|| THE ATTRIBUTE GRAMMAR
||-----------------------------------------------------------------------------
-}

-- public <snouncla> = <cnoun> | <adjs> <cnoun>;
snouncla
 = memoize Snouncla
 (parser
  (nt cnoun S3)
  [rule_s NOUNCLA_VAL OF LHS ISEQUALTO copy [synthesized NOUNCLA_VAL OF S3]]
  <|>
  parser (nt adjs S1  *> nt cnoun S2)
  [rule_s NOUNCLA_VAL OF LHS ISEQUALTO intrsct1 [synthesized ADJ_VAL      OF  S1,
                                                 synthesized NOUNCLA_VAL  OF  S2]]

 )

-------------------------------------------------------------------------------
-- public <relnouncla> = <snouncla> <relpron> <joinvbph> | <snouncla>;
relnouncla
 = memoize Relnouncla
   (parser
    (nt snouncla S1  *> nt relpron S2  *> nt joinvbph S3)
    [rule_s NOUNCLA_VAL OF LHS ISEQUALTO apply_middle1[synthesized NOUNCLA_VAL  OF S1,
                                                       synthesized RELPRON_VAL  OF S2,
                                                       synthesized VERBPH_VAL   OF S3]]
    <|>
    parser
    (nt snouncla S4)
    [rule_s NOUNCLA_VAL OF LHS ISEQUALTO copy [synthesized NOUNCLA_VAL OF S4]]
   )
----------------------------------------------------------------------------

-- public <nouncla> = <relnouncla> <nounjoin> <nouncla> | <relnouncla> <relpron> <linkingvb> <nouncla> | <relnouncla>;
nouncla
 = memoize Nouncla
   (parser (nt relnouncla S1 *> nt nounjoin S2 *> nt nouncla S3)
    [rule_s NOUNCLA_VAL OF LHS ISEQUALTO apply_middle2 [synthesized NOUNCLA_VAL  OF S1,
                                                        synthesized NOUNJOIN_VAL OF S2,
                                                        synthesized NOUNCLA_VAL  OF S3]]
    <|>
    parser (nt relnouncla S1 *> nt relpron S2 *> nt linkingvb S3 *> nt nouncla S4) --Does this make sense?  Allows us to ask "which moon that was discovered by hall that is moon orbits mars"
    [rule_s NOUNCLA_VAL  OF LHS ISEQUALTO apply_middle3 [synthesized NOUNCLA_VAL  OF S1,
                                                         synthesized RELPRON_VAL  OF S2,
                                                         synthesized NOUNCLA_VAL  OF S4]]
    <|>
    parser (nt relnouncla S1)
    [rule_s NOUNCLA_VAL  OF LHS ISEQUALTO copy [synthesized NOUNCLA_VAL  OF S1]]
   )

------------------------------------------------------------------------------
-- public <adjs> = <adj> <adjs> | <adj>;
adjs
 = memoize Adjs
   (parser (nt adj S1 *> nt adjs S2)
    [rule_s ADJ_VAL  OF LHS ISEQUALTO intrsct2 [synthesized ADJ_VAL  OF S1,
                                                synthesized ADJ_VAL  OF S2]]
    <|>
    parser (nt adj S3)
    [rule_s ADJ_VAL  OF LHS ISEQUALTO copy [synthesized ADJ_VAL  OF S3]]
   )
------------------------------------------------------------------------------
-- public <detph> = <indefpron> | <det> <nouncla>;
detph
 = memoize Detph
   (parser (nt indefpron S3)
    [rule_s TERMPH_VAL OF LHS ISEQUALTO copy [synthesized TERMPH_VAL OF S3]]
    <|>
    parser (nt det S1 *> nt nouncla S2)
    [rule_s TERMPH_VAL OF LHS ISEQUALTO applydet [synthesized DET_VAL      OF S1,
                                                  synthesized NOUNCLA_VAL  OF S2]]
   )

----------------------------------------------------------------------------------

{-
  public <transvbph> = <transvb>
                        | <transvb> <preps>
                        | <transvb> <jointermph>
                        | <transvb> <jointermph> <preps>
                        | <linkingvb> <jointermph> <transvb>
                        | <linkingvb> <jointermph> <transvb> <preps>;
 -}

transvbph
 = memoize Transvbph
   (parser (nt transvb S1) --"discovered"
    [rule_s VERBPH_VAL OF LHS ISEQUALTO applytransvb_no_tmph [synthesized VERB_VAL OF S1]]
    <|>
    parser (nt transvb S1 *> nt preps S2) --"discovered in..."
    [rule_s VERBPH_VAL OF LHS ISEQUALTO applytransvb_no_tmph [synthesized VERB_VAL OF S1,
                                                              synthesized PREP_VAL OF S2]]
    <|>
    parser (nt transvb S1 *> nt jointermph S2) --"discovered phobos (and blah and blah and blah)"
    [rule_s VERBPH_VAL OF LHS ISEQUALTO applytransvbprep [synthesized VERB_VAL    OF S1,
                                                          synthesized TERMPH_VAL  OF S2]]
    <|>
    parser (nt transvb S1 *> nt jointermph S2 *> nt preps S3) --"discovered phobos in..."
    [rule_s VERBPH_VAL OF LHS ISEQUALTO applytransvbprep [synthesized VERB_VAL OF S1,
                                                          synthesized TERMPH_VAL OF S2,
                                                          synthesized PREP_VAL OF S3]]
    <|> --NEW FOR SUPERLATIVES "discovered the most moons"
    parser (nt transvb S1 *> nt superph S2)
    [rule_s VERBPH_VAL OF LHS ISEQUALTO applytransvbsuper [synthesized VERB_VAL OF S1,
                                                           synthesized SUPERPH_VAL OF S2]]
    <|> --NEW FOR SUPERLATIVES "discovered the most moons in 1877 using the least telescopes..."
    parser (nt transvb S1 *> nt superph S2 *> nt preps S3)
    [rule_s VERBPH_VAL OF LHS ISEQUALTO applytransvbsuper [synthesized VERB_VAL OF S1,
                                                           synthesized SUPERPH_VAL OF S2,
                                                           synthesized PREP_VAL OF S3]]
    <|>
    parser (nt linkingvb S1 *> nt transvb S2) --"was discovered"
    [rule_s VERBPH_VAL  OF LHS ISEQUALTO drop3rdprep [synthesized LINKINGVB_VAL  OF  S1,
                                                      synthesized VERB_VAL       OF  S2]]
    <|>
    parser (nt linkingvb S1 *> nt transvb S2 *> nt preps S3) --"was discovered by hall in..."
    [rule_s VERBPH_VAL  OF LHS ISEQUALTO drop3rdprep [synthesized LINKINGVB_VAL  OF  S1,
                                                  synthesized VERB_VAL       OF  S2,
                                                  synthesized PREP_VAL       OF  S3]]
    <|>
    parser (nt linkingvb S1  *>  nt jointermph S2 *> nt transvb S3) --"was phobos discovered"
    [rule_s VERBPH_VAL  OF LHS ISEQUALTO apply_quest_transvb_passive [synthesized LINKINGVB_VAL OF  S1,
                                                                      synthesized TERMPH_VAL    OF  S2,
                                                                      synthesized VERB_VAL      OF  S3]]
    <|>
    parser (nt linkingvb S1  *>  nt jointermph S2 *> nt transvb S3 *> nt preps S4) --"was phobos discovered in..."
    [rule_s VERBPH_VAL  OF LHS ISEQUALTO apply_quest_transvb_passive [synthesized LINKINGVB_VAL OF  S1,
                                                                      synthesized TERMPH_VAL    OF  S2,
                                                                      synthesized VERB_VAL      OF  S3,
                                                                      synthesized PREP_VAL      OF  S4]]
   )

--NEW FOR SUPERLATIVE PHRASES
-- public <superph> = the <super> <nouncla>
superph
 = memoize Superph
  (parser (nt superph_start S1 *> nt super S2 *> nt nouncla S3)
   [rule_s SUPERPH_VAL OF LHS ISEQUALTO applysuperph [synthesized SUPERPHSTART_VAL OF S1,
                                                      synthesized SUPER_VAL OF S2,
                                                      synthesized NOUNCLA_VAL OF S3]]
  )

----------------------------------------------------------------------------------
--NEW FOR PREPOSITIONAL PHRASES

-- public <preps> = <prepph> | <prepph> <preps>;
preps
 = memoize Preps
    (parser (nt prepph S1)
     [rule_s PREP_VAL OF LHS ISEQUALTO applyprep [synthesized PREPPH_VAL OF S1]]
     <|>
     parser (nt prepph S1 *> nt preps S2)
     [rule_s PREP_VAL OF LHS ISEQUALTO applypreps [synthesized PREPPH_VAL OF S1,
                                                   synthesized PREP_VAL OF S2]]
    )

-- public <joinyear> = <year> | <joinyear> <termphjoin> <joinyear>
joinyear = memoize Joinyear
    (
        parser (nt year S1) [rule_s TERMPH_VAL OF LHS ISEQUALTO applyyear [synthesized YEAR_VAL OF S1]]
        <|>
        parser (nt joinyear S1 *> nt termphjoin S2 *> nt joinyear S3)
            [rule_s TERMPH_VAL  OF LHS ISEQUALTO appjoin1 [synthesized TERMPH_VAL     OF S1,
                                                           synthesized TERMPHJOIN_VAL OF S2,
                                                           synthesized TERMPH_VAL     OF S3]]
    )

-- public <prepph> = <prep> <jointermph>;
prepph
 = memoize Prepph
    (parser (nt prep S1 *> nt jointermph S2)
     [rule_s PREPPH_VAL OF LHS ISEQUALTO applyprepph [synthesized PREPN_VAL OF S1,
                                                     synthesized TERMPH_VAL OF S2]]
     <|>
     parser (nt prepnph S1 *> nt joinvbph S2) -- "to discover phobos"
     [rule_s PREPPH_VAL OF LHS ISEQUALTO applyprepph_nph [synthesized PREPNPH_VAL OF S1,
                                                         synthesized VERBPH_VAL OF S2]]
     <|>
     parser (nt prepyear S1 *> nt joinyear S2) -- "in 1877 or 1822 and 1923"
     [rule_s PREPPH_VAL OF LHS ISEQUALTO applyprepph [synthesized PREPN_VAL OF S1,
                                                      synthesized TERMPH_VAL  OF S2]]
     <|>
     parser (nt prep S1 *> nt superph S2)
     [rule_s PREPPH_VAL OF LHS ISEQUALTO applyprepph_super [synthesized PREPN_VAL OF S1,
                                                            synthesized SUPERPH_VAL OF S2]]
    )

----------------------------------------------------------------------------------

-- public <verbph> = <transvbph> | <intransvb> | <linkingvb> <det> <nouncla>;
verbph
 = memoize Verbph
   (
    parser (nt transvbph S4)
    [rule_s VERBPH_VAL OF LHS ISEQUALTO copy [synthesized VERBPH_VAL OF S4]]
   <|>
    parser (nt intransvb S5)
    [rule_s VERBPH_VAL OF LHS ISEQUALTO copy [synthesized VERBPH_VAL OF S5]]
   <|>
    parser (nt linkingvb S1 *> nt det S2 *> nt nouncla S3)
    [rule_s VERBPH_VAL OF LHS ISEQUALTO applyvbph [synthesized NOUNCLA_VAL OF S3]]
   )
------------------------------------------------------------------------------------
-- public <termph> = <pnoun> | <detph> | <year>;
termph
 = memoize Termph
   (
   parser (nt pnoun S1)
   [rule_s TERMPH_VAL OF LHS ISEQUALTO copy [synthesized TERMPH_VAL OF S1]]
   <|>
   parser (nt detph S2)
   [rule_s TERMPH_VAL OF LHS ISEQUALTO copy [synthesized TERMPH_VAL OF S2]]
   )

--TODO: space leak here?
------------------------------------------------------------------------------------
-- public <jointermph> = <jointermph> <termphjoin> <jointermph> | <termph>;
jointermph
 = memoize Jointermph
   (
{-    parser (nt jointermph S1 *> nt termphjoin S2 *> nt termph S3)
        [rule_s TERMPH_VAL  OF LHS ISEQUALTO appjoin1 [synthesized TERMPH_VAL     OF S1,
                                                       synthesized TERMPHJOIN_VAL OF S2,
                                                       synthesized TERMPH_VAL     OF S3]]
   <|>
-}
    parser (nt jointermph S1 *> nt termphjoin S2 *> nt jointermph S3) --"hall and galileo discovered...?"
    [rule_s TERMPH_VAL  OF LHS ISEQUALTO appjoin1 [synthesized TERMPH_VAL     OF S1,
                                                   synthesized TERMPHJOIN_VAL OF S2,
                                                   synthesized TERMPH_VAL     OF S3]]
   <|>
    parser (nt termph S4)
    [rule_s TERMPH_VAL  OF LHS ISEQUALTO copy [synthesized TERMPH_VAL  OF S4]]
   )
------------------------------------------------------------------------------------
-- public <joinvbph> = <verbph> <verbphjoin> <joinvbph> | <verbph>;
joinvbph
 = memoize Joinvbph
   (
   parser (nt verbph S1  *> nt verbphjoin S2  *> nt joinvbph S3)
   [rule_s VERBPH_VAL  OF LHS ISEQUALTO appjoin2 [synthesized VERBPH_VAL    OF S1,
                                                  synthesized VBPHJOIN_VAL  OF S2,
                                                  synthesized VERBPH_VAL    OF S3]]
    <|>
    parser (nt verbph S4)
    [rule_s VERBPH_VAL  OF LHS ISEQUALTO copy [synthesized VERBPH_VAL  OF S4]]
   )
---------------------------------------------------------------------------
-- public <sent> = <jointermph> <joinvbph>;
sent
 = memoize Sent
   (
    parser (nt jointermph S1  *> nt joinvbph S2)
    [rule_s SENT_VAL OF LHS ISEQUALTO apply_termphrase [synthesized TERMPH_VAL  OF  S1,
                                                        synthesized VERBPH_VAL  OF  S2]]
   )
-- **************************************************************************** --
-- public <two_sent> = <sent> <sentjoin> <sent>;
two_sent
 = memoize Two_sent
   (
    parser (nt sent S1 *> nt sentjoin S2 *> nt sent S3)
    [rule_s SENT_VAL OF LHS ISEQUALTO sent_val_comp [synthesized SENT_VAL      OF  S1,
                                                     synthesized SENTJOIN_VAL  OF  S2,
                                                     synthesized SENT_VAL      OF  S3]]
   )
------------------------------------------------------------------------------------
{-
  public <question> = <quest1> <sent>
                        | <quest6> <quest1> <sent>
                        | <quest5> <quest1> <sent>
                        | <quest2> <joinvbph>
                        | <quest5> <joinvbph>
                        | <quest3> <nouncla>
                        | <quest3> <nouncla> <joinvbph>
                        | <quest4> <nouncla> <joinvbph>
                        | <two_sent>
                        | <sent>;

-}
question
 = memoize Question
   (
    parser (nt quest1 S1  *> nt sent S2 )
    [rule_s QUEST_VAL  OF LHS ISEQUALTO ans1 [synthesized QUEST1_VAL  OF  S1,
                                              synthesized SENT_VAL    OF  S2]]
    <|>
      --(how|when|where|who) DID jointermph discover
    parser (nt quest5 S1  *> nt quest1 S2  *>  nt sent S3 )
    [rule_s QUEST_VAL  OF LHS ISEQUALTO ans5 [synthesized QUEST2_VAL  OF  S1,
                                              synthesized QUEST1_VAL OF S2,
                                              synthesized SENT_VAL    OF  S3]]
    <|> --whatobj case -- special because needs lookup in AST
    parser (nt quest6 S1  *> nt quest1 S2  *>  nt sent S3 )
    [rule_s QUEST_VAL  OF LHS ISEQUALTO ans6 [synthesized QUEST6_VAL  OF  S1,
                                              synthesized QUEST1_VAL OF S2,
                                              synthesized SENT_VAL    OF  S3]]
    <|>
    {-parser (nt quest7 S1  *> nt quest1 S2  *>  nt sent S3 )
    [rule_s QUEST_VAL  OF LHS ISEQUALTO ans5 [synthesized QUEST2_VAL  OF  S1,
                                              synthesized QUEST1_VAL OF S2,
                                              synthesized SENT_VAL    OF  S3]]
    <|>-}
      --(whatsubj|who|how|when|where) [WAS] [tmph] discovered ...
    parser (nt quest2 S1 *> nt joinvbph S2)
    [rule_s QUEST_VAL  OF LHS ISEQUALTO ans2 [synthesized QUEST2_VAL    OF  S1,
                                              synthesized VERBPH_VAL    OF  S2]]
    <|>
    {-parser (nt quest7 S1 *> nt joinvbph S2)
    [rule_s QUEST_VAL  OF LHS ISEQUALTO ans2 [synthesized QUEST2_VAL    OF  S1,
                                              synthesized VERBPH_VAL    OF  S2]]
    <|> -}
    parser (nt quest5 S1 *> nt joinvbph S2)
    [rule_s QUEST_VAL  OF LHS ISEQUALTO ans2 [synthesized QUEST2_VAL    OF  S1,
                                              synthesized VERBPH_VAL    OF  S2]]
    <|>
    parser (nt quest3 S1 *> nt nouncla S2 *> nt joinvbph S3)
    [rule_s QUEST_VAL  OF LHS ISEQUALTO ans3 [synthesized QUEST3_VAL  OF S1,
                                              synthesized NOUNCLA_VAL OF S2,
                                              synthesized VERBPH_VAL  OF  S3]]
    <|>
    parser (nt quest4 S1 *> nt nouncla S2 *> nt joinvbph S3)
    [rule_s QUEST_VAL  OF LHS ISEQUALTO ans3 [synthesized QUEST3_VAL  OF  S1,
                                              synthesized NOUNCLA_VAL OF  S2,
                                              synthesized VERBPH_VAL  OF  S3]]
    <|>
    parser (nt two_sent S1)
    [rule_s QUEST_VAL OF LHS ISEQUALTO truefalse [synthesized SENT_VAL OF  S1]]
    <|>
    parser (nt sent S1)
    [rule_s QUEST_VAL  OF LHS ISEQUALTO truefalse [synthesized SENT_VAL OF  S1]]

   )

-- public <quest4> = <quest4a> <quest4b>;
quest4 = memoize Quest4
   (
   parser (nt quest4a S1 *> nt quest4b S2)
   [rule_s QUEST3_VAL  OF LHS ISEQUALTO copy [synthesized QUEST3_VAL  OF  S1]]
   )
---------------------------------------------------------------------------------

query = memoize Query
        (
        parser (nt question S1) -- *> nt terminator S2)
        [rule_s QUEST_VAL  OF LHS ISEQUALTO copy [synthesized QUEST_VAL  OF  S1]]
        )

{-
|| -----------------------------------------------------------------------------
|| THE SEMANTICS - PART I : The attribute evaluation  functions
||-----------------------------------------------------------------------------
-}

intrsct1 [x, y] = NOUNCLA_VAL (intersect_fdbr (getAtts getAVALS  x) (getAtts getAVALS y))

intrsct2 [x, y] = ADJ_VAL (intersect_fdbr (getAtts getAVALS x) (getAtts getAVALS y))

applydet [x, y] = TERMPH_VAL $ (getAtts getDVAL x) (getAtts getAVALS y)

--make_trans_vb is very similar to make_trans_active.  getBR must mean "get binary relation"
--getTVAL must mean "get predicate" i.e. what would be "phobos" in "discover phobos"
--Changed to get rid of make_trans_vb, since the getBR attribute was changed to not
--be a binary relation but instead a function that make_trans_active would give
--I.e., the kind of function that make_trans_vb would have generated, since they were
--nearly identical

--NEW FOR PREPOSITIONAL PHRASES
applytransvbprep [x,y,z] = VERBPH_VAL $ make_trans_active' reln ((make_prep [object] predicate) : preps)
    where
    reln = getAtts getBR x
    predicate = getAtts getTVAL y
    preps = getAtts getPREPVAL z
    (_, object) = getVoiceProps ActiveVoice reln

applytransvbprep [x,y] = VERBPH_VAL $ make_trans_active' reln [make_prep [object] predicate]
    where
    reln = getAtts getBR x
    predicate = getAtts getTVAL y
    (_, object) = getVoiceProps ActiveVoice reln

applytransvbsuper [x, y] = VERBPH_VAL $ make_trans_active' reln [make_prep_superph [object] superpred]
    where
    reln = getAtts getBR x
    superpred = getAtts getSUPERPHVAL y
    (_, object) = getVoiceProps ActiveVoice reln

applytransvbsuper [x, y, z] = VERBPH_VAL $ make_trans_active' reln ((make_prep_superph [object] superpred) : preps)
    where
    reln = getAtts getBR x
    superpred = getAtts getSUPERPHVAL y
    preps = getAtts getPREPVAL z
    (_, object) = getVoiceProps ActiveVoice reln

applytransvb_no_tmph [x,y] = VERBPH_VAL $ make_trans_active' reln preps
    where
    reln = getAtts getBR x
    preps = getAtts getPREPVAL y

applytransvb_no_tmph [x] = VERBPH_VAL $ make_trans_active' reln []
    where
    reln = getAtts getBR x

--TODO: modify grammar so you can't ask "what was phobos discover", or if you can, make the answer sensible (e.g. hall, not phobos)
apply_quest_transvb_passive (x2:x3:x4:xs) = VERBPH_VAL $ termph (make_trans_passive reln preps)
    where
    linkingvb = getAtts getLINKVAL x2
    termph = getAtts getTVAL x3
    reln = getAtts getBR x4
    preps = case xs of
        [] -> []
        (x5:_) -> getAtts getPREPVAL x5

applyprepph [x, y] = PREPPH_VAL $
        let prep_names = getAtts getPREPNVAL x
            termph = getAtts getTVAL y in
                make_prep prep_names termph

applyprepph_nph [x, y] = PREPPH_VAL $
        let prep_names = getAtts getPREPNPHVAL x
            nph = getAtts getAVALS y in
                make_prep_nph prep_names nph

applyprepph_super [x, y] = PREPPH_VAL $
        let prep_names = getAtts getPREPNVAL x
            superph = getAtts getSUPERPHVAL y in
                make_prep_superph prep_names superph

applyprep [x] = PREP_VAL $ [getAtts getPREPPHVAL x]--[(["with_implement"], a telescope)]

applypreps [x, y] = PREP_VAL $ getAtts getPREPPHVAL x : (getAtts getPREPVAL y)

applysuperph [x, y, z] = SUPERPH_VAL $
        let super_the = getAtts getSUPERPHSTARTVAL x
            super_ordering = getAtts getSUPERVAL y
            nph = getAtts getAVALS z
            --inject :: Ordering -> SemFunc (TF FDBR -> TF FDBR) -> (Ordering, SemFunc (TF FDBR -> TF FDBR))
            inject ord termph = (ord, termph)
            in
                inject super_ordering $ intersect_fdbr nph


applyyear [x] = TERMPH_VAL $ make_pnoun $ tshow $ getAtts getYEARVAL x

--END PREPOSITIONAL PHRASES

applyvbph [z] = VERBPH_VAL (getAtts getAVALS z)

appjoin1 [x, y, z] = TERMPH_VAL $ (getAtts getTJVAL y) (getAtts getTVAL x) (getAtts getTVAL z)

appjoin2 [x, y, z] = VERBPH_VAL ((getAtts getVJVAL y) (getAtts getAVALS x) (getAtts getAVALS z))

apply_middle1 [x, y, z] = NOUNCLA_VAL ((getAtts getRELVAL y) (getAtts getAVALS x) (getAtts getAVALS z))

apply_middle2 [x, y, z] = NOUNCLA_VAL ((getAtts getNJVAL y) (getAtts getAVALS x) (getAtts getAVALS z))

apply_middle3 [x, y, z] =  NOUNCLA_VAL ((getAtts getRELVAL y) (getAtts getAVALS x) (getAtts getAVALS z))

-- Think "a orbited by b" vs "b orbits a"
{-drop3rd          [w, x, y, z]
 = \atts -> VERBPH_VAL $
        let reln = getAtts getBR atts x in
        let predicate = getAtts getTVAL atts z in
        make_inverted_relation dataStore reln predicate-}

--NEW FOR PREPOSITIONAL PHRASES
drop3rdprep (w:x:xs) = VERBPH_VAL $ make_trans_passive reln preps
        where
        reln = getAtts getBR x
        preps = case xs of
                  [] -> []
                  (p:_) -> getAtts getPREPVAL p
--END PREPOSITIONAL PHRASES

apply_termphrase [x, y] = SENT_VAL ((getAtts getTVAL x) (getAtts getAVALS y))

sent_val_comp [s1, f, s2] = SENT_VAL ((getAtts getSJVAL f) (getAtts getSV s1) (getAtts getSV s2))

ans1 [x, y] = QUEST_VAL ((getAtts getQU1VAL x) (getAtts getSV y) )

ans2 [x, y] = QUEST_VAL ((getAtts getQU2VAL x) (getAtts getAVALS y))

ans3 [x, y, z] = QUEST_VAL ((getAtts getQU3VAL x) (getAtts getAVALS y) (getAtts getAVALS z))

ans5 [x, y, z] = QUEST_VAL ((getAtts getQU2VAL x) (getAtts getSV z))

ans6 [x, y, z] = QUEST_VAL ((getAtts getQU6VAL x) (getAtts getSV z))

truefalse [x] = QUEST_VAL $ _truefalse (getAtts getSV x) -- fmap (\fdbr -> if not (List.null fdbr) then "true." else "false.") `first` (getAtts getSV atts x)

{-
||-----------------------------------------------------------------------------
|| THE SEMANTICS - PART II : Functions used to obtain objects denoted by
||   proper nouns, verbs, etc.
||-----------------------------------------------------------------------------

|| FUNCTION USED TO DEFINE OBJECTS ASSOCIATED WITH PROPER NOUNS
-}
--test_wrt e s = e `elem` s

-- FUNCTION USED TO DEFINE MEANINGS OF VERBS IN TERMS OF RELATIONS
--make_trans_vb rel p = [x | (x, image_x) <- collect rel, p image_x] -- Similar to make_trans_active

{- TERMINALS IN JSGF FORM

<Prepn> = with | in | at | by;

<Transvb> = discover | discovers | discovered | orbit | orbited | orbits;

<Indefpron> = anyone | anything | anybody | someone | something | somebody | everyone | everything | everybody;

<Quest5> = where | when | how;

<Quest4b> = many;

<Sentjoin> = and;

<Linkingvb> = is | was | are | were;

<Nounjoin> = and | or;

<Verbphjoin> = and | or;

<Quest3> = which | what;

<Det> = the | a | one | an | some | any | every | all | two;

<Termphjoin> = and | or;

<Quest6> = whatobj;

<Intransvb> = exist | exists | spin | spins;

-}

discover_rel = ("discover_ev","subject","object")
orbit_rel = ("orbit_ev", "subject", "object")
use_rel = ("discover_ev", "subject", "with_implement")

--TODO:
{-
Note the following case:
    ("hall",               Pnoun,     [TERMPH_VAL $ make_pnoun "hall"]),
    ("hall",               Pnoun,     [TERMPH_VAL $ make_pnoun "hall2"]),

!!! Any refactorings need to preserve the semantics that both "hall"s are tried! !!!
This is NOT the same as ("hall", Pnoun, [TERMPH_VAL $ make_pnoun "hall", TERMPH_VAL $ make_pnoun "hall2"]),
which will (seemingly) use the first TERMPH_VAL and ignore the second completely.

NOTE2: Map needs to be lazy in the values or else the parser will loop indefinitely with meaning_of: map from dict, evaluate meaning_of, which needs to use map from dict, which evaluates meaning of

NOTE4: so fromListWith in HashMap is actually *NOT* strict in the values!  That means lazy evaluation is still happening!
https://medium.com/@aleksandrasays/brief-normal-forms-explanation-with-haskell-cd5dfa94a157

A strict HashMap is used because the lists themselves are still lazy.  so evaluating to WHNF is no problem.

-}

--Try to retain old semantics: collect AttValues of multiple definitions together and unpack them later
--Use Lazy hashmap to ensure meaning_of still works!
dictionary''' :: HashMap.HashMap (MemoL,T.Text) [[AttValue]]
dictionary''' = HashMap.fromListWith (flip (++)) $ map (\(t,m,atts) -> ((m,t),[atts])) dictionary

dictionary :: [(T.Text, MemoL, [AttValue])]
dictionary = [
    ("thing",              Cnoun,     [NOUNCLA_VAL $ get_members "thing"]),
    ("things",             Cnoun,     [NOUNCLA_VAL $ get_members "thing"]),
    ("planets",            Cnoun,     [NOUNCLA_VAL $ get_members "planet"]),
    ("planet",             Cnoun,     [NOUNCLA_VAL $ get_members "planet"]),
    ("person",             Cnoun,     [NOUNCLA_VAL $ get_members "person"]),
    ("sun",                Cnoun,     [NOUNCLA_VAL $ get_members "sun"]),
    ("moon",               Cnoun,     [NOUNCLA_VAL $ get_members "moon"]),
    ("moons",              Cnoun,     [NOUNCLA_VAL $ get_members "moon"]),
    ("satellite",          Cnoun,     [NOUNCLA_VAL $ get_members "moon"]),
    ("satellites",         Cnoun,     [NOUNCLA_VAL $ get_members "moon"]),
    ("atmospheric",        Adj,       [ADJ_VAL $ get_members "atmospheric"]),
    ("blue",               Adj,       [ADJ_VAL $ get_members "blue"]),
    ("solid",              Adj,       [ADJ_VAL $ get_members "solid"]),
    ("brown",              Adj,       [ADJ_VAL $ get_members "brown"]),
    ("gaseous",            Adj,       [ADJ_VAL $ get_members "gaseous"]),
    ("green",              Adj,       [ADJ_VAL $ get_members "green"]),
    ("red",                Adj,       [ADJ_VAL $ get_members "red"]),
    ("ringed",             Adj,       [ADJ_VAL $ get_members "ringed"]),
    ("vacuumous",          Adj,       [ADJ_VAL $ get_members "vacuumous"]),
    ("exist",              Intransvb, [VERBPH_VAL $ get_members "thing"]),
    ("exists",             Intransvb, [VERBPH_VAL $ get_members "thing"]),
    ("spin",               Intransvb, [VERBPH_VAL $ get_members "spin"]),
    ("spins",              Intransvb, [VERBPH_VAL $ get_members "spin"]),
    ("the",                Det,       [DET_VAL $ the]),
    ("a",                  Det,       [DET_VAL $ a]),
    ("one",                Det,       [DET_VAL $ one]),
    ("an",                 Det,       [DET_VAL $ a]),
    ("some",               Det,       [DET_VAL $ a]),
    ("any",                Det,       [DET_VAL $ a]),
    --("no",                 Det,       [DET_VAL $ no]),
    ("every",              Det,       [DET_VAL $ every]),
    ("all",                Det,       [DET_VAL $ every]),
    ("two",                Det,       [DET_VAL $ two]),
    ("most",               Det,       [DET_VAL $ most]),
    ("the",                SuperphStart,       [SUPERPHSTART_VAL $ ()]),
    ("most",               Super,     [SUPER_VAL $ GT]),
    --("least",              Super,     [SUPER_VAL $ LT]), --TODO: problem with least is 0-cardinality and negation
    ("bernard",            Pnoun,     [TERMPH_VAL $ make_pnoun "bernard"]),
    ("bond",               Pnoun,     [TERMPH_VAL $ make_pnoun "bond"]),
    ("venus",              Pnoun,     [TERMPH_VAL $ make_pnoun "venus"]),
    ("cassini",            Pnoun,     [TERMPH_VAL $ make_pnoun "cassini"]),
    ("dollfus",            Pnoun,     [TERMPH_VAL $ make_pnoun "dollfus"]),
    ("galileo",            Pnoun,     [TERMPH_VAL $ make_pnoun "galileo"]),
    ("hall",               Pnoun,     [TERMPH_VAL $ make_pnoun "hall"]),
    ("herschel",           Pnoun,     [TERMPH_VAL $ make_pnoun "herschel"]),
    ("huygens",            Pnoun,     [TERMPH_VAL $ make_pnoun "huygens"]),
    ("kowal",              Pnoun,     [TERMPH_VAL $ make_pnoun "kowal"]),
    ("kuiper",             Pnoun,     [TERMPH_VAL $ make_pnoun "kuiper"]),
    ("larsen",             Pnoun,     [TERMPH_VAL $ make_pnoun "larsen"]),
    ("lassell",            Pnoun,     [TERMPH_VAL $ make_pnoun "lassell"]),
    ("melotte",            Pnoun,     [TERMPH_VAL $ make_pnoun "melotte"]),
    ("nicholson",          Pnoun,     [TERMPH_VAL $ make_pnoun "nicholson"]),
    ("perrine",            Pnoun,     [TERMPH_VAL $ make_pnoun "perrine"]),
    ("pickering",          Pnoun,     [TERMPH_VAL $ make_pnoun "pickering"]),
    ("almathea",           Pnoun,     [TERMPH_VAL $ make_pnoun "almathea"]),
    ("ariel",              Pnoun,     [TERMPH_VAL $ make_pnoun "ariel"]),
    ("callisto",           Pnoun,     [TERMPH_VAL $ make_pnoun "callisto"]),
    ("charon",             Pnoun,     [TERMPH_VAL $ make_pnoun "charon"]),
    ("deimos",             Pnoun,     [TERMPH_VAL $ make_pnoun "deimos"]),
    ("dione",              Pnoun,     [TERMPH_VAL $ make_pnoun "dione"]),
    ("earth",              Pnoun,     [TERMPH_VAL $ make_pnoun "earth"]),
    ("enceladus",          Pnoun,     [TERMPH_VAL $ make_pnoun "enceladus"]),
    ("europa",             Pnoun,     [TERMPH_VAL $ make_pnoun "europa"]),
    ("ganymede",           Pnoun,     [TERMPH_VAL $ make_pnoun "ganymede"]),
    ("hyperion",           Pnoun,     [TERMPH_VAL $ make_pnoun "hyperion"]),
    ("iapetus",            Pnoun,     [TERMPH_VAL $ make_pnoun "iapetus"]),
    ("io",                 Pnoun,     [TERMPH_VAL $ make_pnoun "io"]),
    ("janus",              Pnoun,     [TERMPH_VAL $ make_pnoun "janus"]),
    ("jupiter",            Pnoun,     [TERMPH_VAL $ make_pnoun "jupiter"]),
    ("jupitereighth",      Pnoun,     [TERMPH_VAL $ make_pnoun "jupitereighth"]),
    ("jupitereleventh",    Pnoun,     [TERMPH_VAL $ make_pnoun "jupitereleventh"]),
    ("jupiterfourteenth",  Pnoun,     [TERMPH_VAL $ make_pnoun "jupiterfourteenth"]),
    ("jupiterninth",       Pnoun,     [TERMPH_VAL $ make_pnoun "jupiterninth"]),
    ("jupiterseventh",     Pnoun,     [TERMPH_VAL $ make_pnoun "jupiterseventh"]),
    ("jupitersixth",       Pnoun,     [TERMPH_VAL $ make_pnoun "jupitersixth"]),
    ("jupitertenth",       Pnoun,     [TERMPH_VAL $ make_pnoun "jupitertenth"]),
    ("jupiterthirteenth",  Pnoun,     [TERMPH_VAL $ make_pnoun "jupiterthirteenth"]),
    ("jupitertwelfth",     Pnoun,     [TERMPH_VAL $ make_pnoun "jupitertwelfth"]),
    ("luna",               Pnoun,     [TERMPH_VAL $ make_pnoun "luna"]),
    ("mars",               Pnoun,     [TERMPH_VAL $ make_pnoun "mars"]),
    ("mercury",            Pnoun,     [TERMPH_VAL $ make_pnoun "mercury"]),
    ("mimas",              Pnoun,     [TERMPH_VAL $ make_pnoun "mimas"]),
    ("miranda",            Pnoun,     [TERMPH_VAL $ make_pnoun "miranda"]),
    ("neptune",            Pnoun,     [TERMPH_VAL $ make_pnoun "neptune"]),
    ("nereid",             Pnoun,     [TERMPH_VAL $ make_pnoun "nereid"]),
    ("oberon",             Pnoun,     [TERMPH_VAL $ make_pnoun "oberon"]),
    ("phobos",             Pnoun,     [TERMPH_VAL $ make_pnoun "phobos"]),
    ("phoebe",             Pnoun,     [TERMPH_VAL $ make_pnoun "phoebe"]),
    ("pluto",              Pnoun,     [TERMPH_VAL $ make_pnoun "pluto"]),
    ("rhea",               Pnoun,     [TERMPH_VAL $ make_pnoun "rhea"]),
    ("saturn",             Pnoun,     [TERMPH_VAL $ make_pnoun "saturn"]),
    ("saturnfirst",        Pnoun,     [TERMPH_VAL $ make_pnoun "saturnfirst"]),
    ("sol",                Pnoun,     [TERMPH_VAL $ make_pnoun "sol"]),
    ("tethys",             Pnoun,     [TERMPH_VAL $ make_pnoun "tethys"]),
    ("titan",              Pnoun,     [TERMPH_VAL $ make_pnoun "titan"]),
    ("titania",            Pnoun,     [TERMPH_VAL $ make_pnoun "titania"]),
    ("triton",             Pnoun,     [TERMPH_VAL $ make_pnoun "triton"]),
    ("umbriel",            Pnoun,     [TERMPH_VAL $ make_pnoun "umbriel"]),
    ("uranus",             Pnoun,     [TERMPH_VAL $ make_pnoun "uranus"]),
    ("venus",              Pnoun,     [TERMPH_VAL $ make_pnoun "venus"]),
    ("discover",           Transvb,   [VERB_VAL discover_rel]),
    ("discovers",          Transvb,   [VERB_VAL discover_rel]),
    ("discovered",         Transvb,   [VERB_VAL discover_rel]),
    ("orbit",              Transvb,   [VERB_VAL orbit_rel]),
    ("orbited",            Transvb,   [VERB_VAL orbit_rel]),
    ("orbits",             Transvb,   [VERB_VAL orbit_rel]),
    ("use",                Transvb,   [VERB_VAL use_rel]),
    ("used",               Transvb,   [VERB_VAL use_rel]),
    ("uses",               Transvb,   [VERB_VAL use_rel]),
    ("is",                 Linkingvb, [LINKINGVB_VAL $ id]),
    ("was",                Linkingvb, [LINKINGVB_VAL $ id]),
    ("are",                Linkingvb, [LINKINGVB_VAL $ id]),
    ("were",               Linkingvb, [LINKINGVB_VAL $ id]),
    ("that",               Relpron,   [RELPRON_VAL    $ that]),
    ("who",                Relpron,   [RELPRON_VAL    $ that]),
    ("which",              Relpron,   [RELPRON_VAL    $ that]),
    ("and",                Verbphjoin,[VBPHJOIN_VAL   $ nounand]),
    ("or",                 Verbphjoin,[VBPHJOIN_VAL   $ nounor]),
    ("and",                Nounjoin,  [NOUNJOIN_VAL   $ nounand]),
    ("or",                 Nounjoin,  [NOUNJOIN_VAL   $ nounor]),
    ("and",                Termphjoin,[TERMPHJOIN_VAL termand]),
    ("or",                 Termphjoin,[TERMPHJOIN_VAL termor]),
    ("and",                Sentjoin,  [SENTJOIN_VAL   $ sand]),
    ("does",               Quest1,    [QUEST1_VAL     $ yesno]),
    ("did",                Quest1  ,  [QUEST1_VAL     $ yesno]),
    ("do",                 Quest1,    [QUEST1_VAL     $ yesno]),
    --AMBIGUOUS: how was phobos discovered ("is",                 Quest1,    [QUEST1_VAL     $ yesno]),
    --("was",                Quest1,    [QUEST1_VAL     $ yesno]),
    --("are",                Quest1,    [QUEST1_VAL     $ yesno]),
    --("were",               Quest1,    [QUEST1_VAL     $ yesno]),
    ("what",               Quest2,    [QUEST2_VAL     $ what]),
    ("what",               Quest6,    [QUEST6_VAL     $ whatobj]),
    ("who",                Quest5,    [QUEST2_VAL     $ who]),
    ("where",              Quest5,    [QUEST2_VAL     $ where']),
    ("when",               Quest5,    [QUEST2_VAL     $ when']),
    ("how",                Quest5,    [QUEST2_VAL     $ how']),
    ("which",              Quest3,    [QUEST3_VAL     which]),
    ("what",               Quest3,    [QUEST3_VAL     which]),
    ("how",                Quest4a,   [QUEST3_VAL     $ how_many]),
    ("many",               Quest4b,   [QUEST3_VAL     $ how_many]),
    ("human",       Cnoun,    meaning_of nouncla "person" Nouncla),
    ("discoverer",  Cnoun,            [NOUNCLA_VAL $ get_subjs_of_event_type "discover_ev"]),
    ("discoverers", Cnoun,            [NOUNCLA_VAL $ get_subjs_of_event_type "discover_ev"]),
    ("humans",      Cnoun,    meaning_of nouncla "person" Nouncla),
    ("people",      Cnoun,    meaning_of nouncla "person" Nouncla),
    --("orbit",       Intransvb,        [VERBPH_VAL $ get_subjs_of_event_type "orbit_ev"]),
    --("orbits",      Intransvb,        [VERBPH_VAL $ get_subjs_of_event_type "orbit_ev"]),
    ("anyone",      Indefpron,meaning_of detph   "a person" Detph),
    ("anything",    Indefpron,meaning_of detph   "a thing" Detph),
    ("anybody",     Indefpron,meaning_of detph   "a person" Detph),
    ("someone",     Indefpron,meaning_of detph   "a person" Detph),
    ("something",   Indefpron,meaning_of detph   "a thing" Detph),
    ("somebody",    Indefpron,meaning_of detph   "a person" Detph),
    ("everyone",    Indefpron,meaning_of detph   "every person" Detph),
    ("everything",  Indefpron,meaning_of detph   "every thing" Detph),
    ("everybody",   Indefpron,meaning_of detph   "every person" Detph),
    --("nobody",      Indefpron,meaning_of detph   "no person" Detph),
    --("noone",       Indefpron,meaning_of detph   "no person" Detph),
    --Begin prepositional stuff--
    ("with",        Prepn, [PREPN_VAL ["with_implement"]]),
    ("using",       Prepn, [PREPN_VAL ["with_implement"]]),
    ("in",          Prepn, [PREPN_VAL ["location"]]),
    ("at",          Prepn, [PREPN_VAL ["location"]]),
    ("by",          Prepn, [PREPN_VAL ["subject"]]),
    ("to",          Prepnph, [PREPNPH_VAL ["subject"]]),
    ("in",          Prepyear, [PREPN_VAL ["year"]]),
    --Begin telescope stuff--
    ("telescope",   Cnoun, [NOUNCLA_VAL $ get_members "telescope"]),
    ("telescopes",  Cnoun, [NOUNCLA_VAL $ get_members "telescope"]),
    ("cassegrain_telescope",            Pnoun,[TERMPH_VAL $ make_pnoun "cassegrain_telescope"]),
    ("hooker_telescope",                Pnoun,[TERMPH_VAL $ make_pnoun "hooker_telescope"]),
    ("schmidt_telescope",               Pnoun,[TERMPH_VAL $ make_pnoun "schmidt_telescope"]),
    ("subaru_reflector_telescope",      Pnoun,[TERMPH_VAL $ make_pnoun "subaru_reflector_telescope"]),
    ("canada-france-hawaii_telescope",  Pnoun,[TERMPH_VAL $ make_pnoun "canada-france-hawaii_telescope"]),
    ("aerial_telescope_1",              Pnoun,[TERMPH_VAL $ make_pnoun "aerial_telescope_1"]),
    ("bruce_astrograph",                Pnoun,[TERMPH_VAL $ make_pnoun "bruce_astrograph"]),
    ("telescope_1",                     Pnoun,[TERMPH_VAL $ make_pnoun "telescope_1"]),
    ("hale_telescope",                  Pnoun,[TERMPH_VAL $ make_pnoun "hale_telescope"]),
    ("hubble_space_telescope",          Pnoun,[TERMPH_VAL $ make_pnoun "hubble_space_telescope"]),
    ("blanco_telescope",                Pnoun,[TERMPH_VAL $ make_pnoun "blanco_telescope"]),
    ("crossley_reflector_telescope",    Pnoun,[TERMPH_VAL $ make_pnoun "crossley_reflector_telescope"]),
    ("refractor_telescope_1",           Pnoun,[TERMPH_VAL $ make_pnoun "refractor_telescope_1"]),
    ("refractor_telescope_2",           Pnoun,[TERMPH_VAL $ make_pnoun "refractor_telescope_2"]),
    ("refractor_telescope_3",           Pnoun,[TERMPH_VAL $ make_pnoun "refractor_telescope_3"]),
    ("refractor_telescope_4",           Pnoun,[TERMPH_VAL $ make_pnoun "refractor_telescope_4"]),
    ("reflector_telescope_1",           Pnoun,[TERMPH_VAL $ make_pnoun "reflector_telescope_1"]),
    ("reflector_telescope_2",           Pnoun,[TERMPH_VAL $ make_pnoun "reflector_telescope_2"]),
    ("reflector_telescope_3",           Pnoun,[TERMPH_VAL $ make_pnoun "reflector_telescope_3"]),
    ("reflector_telescope_4",           Pnoun,[TERMPH_VAL $ make_pnoun "reflector_telescope_4"]),
    ("ground_based_telescope_1",        Pnoun,[TERMPH_VAL $ make_pnoun "ground_based_telescope_1"]),
    ("ground_based_telescope_2",        Pnoun,[TERMPH_VAL $ make_pnoun "ground_based_telescope_2"]),
    ("ground_based_telescope_3",        Pnoun,[TERMPH_VAL $ make_pnoun "ground_based_telescope_3"]),
    ("galilean_telescope_1",            Pnoun,[TERMPH_VAL $ make_pnoun "galilean_telescope_1"]),
    --Begin science team stuff
    ("team",                            Cnoun, [NOUNCLA_VAL $ get_members "science_team"]),
    ("teams",                           Cnoun, [NOUNCLA_VAL $ get_members "science_team"]),
    ("voyager_science_team",            Pnoun,[TERMPH_VAL $ make_pnoun "voyager_science_team"]),
    ("cassini_imaging_science_team",    Pnoun,[TERMPH_VAL $ make_pnoun "cassini_imaging_science_team"]),
    ("science_team_1",                  Pnoun,[TERMPH_VAL $ make_pnoun "science_team_1"]),
    ("science_team_2",                  Pnoun,[TERMPH_VAL $ make_pnoun "science_team_2"]),
    ("science_team_3",                  Pnoun,[TERMPH_VAL $ make_pnoun "science_team_3"]),
    ("science_team_4",                  Pnoun,[TERMPH_VAL $ make_pnoun "science_team_4"]),
    ("science_team_5",                  Pnoun,[TERMPH_VAL $ make_pnoun "science_team_5"]),
    ("science_team_6",                  Pnoun,[TERMPH_VAL $ make_pnoun "science_team_6"]),
    ("science_team_7",                  Pnoun,[TERMPH_VAL $ make_pnoun "science_team_7"]),
    ("science_team_8",                  Pnoun,[TERMPH_VAL $ make_pnoun "science_team_8"]),
    ("science_team_9",                  Pnoun,[TERMPH_VAL $ make_pnoun "science_team_9"]),
    ("science_team_10",                 Pnoun,[TERMPH_VAL $ make_pnoun "science_team_10"]),
    ("science_team_11",                 Pnoun,[TERMPH_VAL $ make_pnoun "science_team_11"]),
    ("science_team_12",                 Pnoun,[TERMPH_VAL $ make_pnoun "science_team_12"]),
    ("science_team_13",                 Pnoun,[TERMPH_VAL $ make_pnoun "science_team_13"]),
    ("science_team_14",                 Pnoun,[TERMPH_VAL $ make_pnoun "science_team_14"]),
    ("science_team_15",                 Pnoun,[TERMPH_VAL $ make_pnoun "science_team_15"]),
    ("science_team_16",                 Pnoun,[TERMPH_VAL $ make_pnoun "science_team_16"]),
    ("science_team_17",                 Pnoun,[TERMPH_VAL $ make_pnoun "science_team_17"]),
    ("science_team_18",                 Pnoun,[TERMPH_VAL $ make_pnoun "science_team_18"]),
    ("science_team_19",                 Pnoun,[TERMPH_VAL $ make_pnoun "science_team_19"]),
    ("science_team_20",                 Pnoun,[TERMPH_VAL $ make_pnoun "science_team_20"]),
    ("science_team_21",                 Pnoun,[TERMPH_VAL $ make_pnoun "science_team_21"]),
    --Begin new people stuff--
    ("larson", Pnoun, [TERMPH_VAL $ make_pnoun "larson"]),
    ("fountain", Pnoun, [TERMPH_VAL $ make_pnoun "fountain"]),
    ("scotti", Pnoun, [TERMPH_VAL $ make_pnoun "scotti"]),
    ("spahr", Pnoun, [TERMPH_VAL $ make_pnoun "spahr"]),
    ("mcmillan", Pnoun, [TERMPH_VAL $ make_pnoun "mcmillan"]),
    ("montani", Pnoun, [TERMPH_VAL $ make_pnoun "montani"]),
    ("gleason", Pnoun, [TERMPH_VAL $ make_pnoun "gleason"]),
    ("gehrels", Pnoun, [TERMPH_VAL $ make_pnoun "gehrels"]),
    ("roemer", Pnoun, [TERMPH_VAL $ make_pnoun "roemer"]),
    ("sheppard", Pnoun, [TERMPH_VAL $ make_pnoun "sheppard"]),
    ("jewitt", Pnoun, [TERMPH_VAL $ make_pnoun "jewitt"]),
    ("fernandez", Pnoun, [TERMPH_VAL $ make_pnoun "fernandez"]),
    ("magnier", Pnoun, [TERMPH_VAL $ make_pnoun "magnier"]),
    ("kleyna", Pnoun, [TERMPH_VAL $ make_pnoun "kleyna"]),
    ("gladman", Pnoun, [TERMPH_VAL $ make_pnoun "gladman"]),
    ("kavelaars", Pnoun, [TERMPH_VAL $ make_pnoun "kavelaars"]),
    ("petit", Pnoun, [TERMPH_VAL $ make_pnoun "petit"]),
    ("allen", Pnoun, [TERMPH_VAL $ make_pnoun "allen"]),
    ("laques", Pnoun, [TERMPH_VAL $ make_pnoun "laques"]),
    ("lecacheux", Pnoun, [TERMPH_VAL $ make_pnoun "lecacheux"]),
    ("smith", Pnoun, [TERMPH_VAL $ make_pnoun "smith"]),
    ("reitsema", Pnoun, [TERMPH_VAL $ make_pnoun "reitsema"]),
    ("pascu", Pnoun, [TERMPH_VAL $ make_pnoun "pascu"]),
    ("seidelmann", Pnoun, [TERMPH_VAL $ make_pnoun "seidelmann"]),
    ("baum", Pnoun, [TERMPH_VAL $ make_pnoun "baum"]),
    ("currie", Pnoun, [TERMPH_VAL $ make_pnoun "currie"]),
    ("showalter", Pnoun, [TERMPH_VAL $ make_pnoun "showalter"]),
    ("scholl", Pnoun, [TERMPH_VAL $ make_pnoun "scholl"]),
    ("holman", Pnoun, [TERMPH_VAL $ make_pnoun "holman"]),
    ("marsden", Pnoun, [TERMPH_VAL $ make_pnoun "marsden"]),
    ("burns", Pnoun, [TERMPH_VAL $ make_pnoun "burns"]),
    ("milisavljevic", Pnoun, [TERMPH_VAL $ make_pnoun "milisavljevic"]),
    ("grav", Pnoun, [TERMPH_VAL $ make_pnoun "grav"]),
    ("karkoschka", Pnoun, [TERMPH_VAL $ make_pnoun "karkoschka"]),
    ("lissauer", Pnoun, [TERMPH_VAL $ make_pnoun "lissauer"]),
    ("fraser", Pnoun, [TERMPH_VAL $ make_pnoun "fraser"]),
    ("christy", Pnoun, [TERMPH_VAL $ make_pnoun "christy"]),
    ("weaver", Pnoun, [TERMPH_VAL $ make_pnoun "weaver"]),
    ("stern", Pnoun, [TERMPH_VAL $ make_pnoun "stern"]),
    ("mutchler", Pnoun, [TERMPH_VAL $ make_pnoun "mutchler"]),
    ("steffl", Pnoun, [TERMPH_VAL $ make_pnoun "steffl"]),
    ("buie", Pnoun, [TERMPH_VAL $ make_pnoun "buie"]),
    ("merline", Pnoun, [TERMPH_VAL $ make_pnoun "merline"]),
    ("spencer", Pnoun, [TERMPH_VAL $ make_pnoun "spencer"]),
    ("young_e_f", Pnoun, [TERMPH_VAL $ make_pnoun "young_e_f"]),
    ("young_l_a", Pnoun, [TERMPH_VAL $ make_pnoun "young_l_a"]),
    ("hamilton", Pnoun, [TERMPH_VAL $ make_pnoun "hamilton"]),
    ("soummer", Pnoun, [TERMPH_VAL $ make_pnoun "soummer"]),
    ("throop", Pnoun, [TERMPH_VAL $ make_pnoun "throop"]),
    --Begin new spacecraft stuff--
    ("spacecraft",  Cnoun, [NOUNCLA_VAL $ get_members "spacecraft"]),
    ("spacecrafts", Cnoun, [NOUNCLA_VAL $ get_members "spacecraft"]),
    ("voyager_1",   Pnoun, [TERMPH_VAL $ make_pnoun "voyager_1"]),
    ("voyager_2",   Pnoun, [TERMPH_VAL $ make_pnoun "voyager_2"]),
    ("cassini", Pnoun, [TERMPH_VAL $ make_pnoun "voyager_2"]),
    --Begin new places stuff--
    ("place",   Cnoun, [NOUNCLA_VAL $ get_members "place"]),
    ("places",  Cnoun, [NOUNCLA_VAL $ get_members "place"]),
    ("mt_hopkins", Pnoun, [TERMPH_VAL $ make_pnoun "mt_hopkins"]),
    ("fort_davis", Pnoun, [TERMPH_VAL $ make_pnoun "fort_davis"]),
    ("cerro_tololo", Pnoun, [TERMPH_VAL $ make_pnoun "cerro_tololo"]),
    ("us_naval_observatory", Pnoun, [TERMPH_VAL $ make_pnoun "us_naval_observatory"]),
    ("padua", Pnoun, [TERMPH_VAL $ make_pnoun "padua"]),
    ("mt_hamilton", Pnoun, [TERMPH_VAL $ make_pnoun "mt_hamilton"]),
    ("greenwich", Pnoun, [TERMPH_VAL $ make_pnoun "greenwich"]),
    ("mt_wilson", Pnoun, [TERMPH_VAL $ make_pnoun "mt_wilson"]),
    ("mt_palomar", Pnoun, [TERMPH_VAL $ make_pnoun "mt_palomar"]),
    ("kitt_peak", Pnoun, [TERMPH_VAL $ make_pnoun "kitt_peak"]),
    ("mauna_kea", Pnoun, [TERMPH_VAL $ make_pnoun "mauna_kea"]),
    ("slough", Pnoun, [TERMPH_VAL $ make_pnoun "slough"]),
    ("paris", Pnoun, [TERMPH_VAL $ make_pnoun "paris"]),
    ("the_hague", Pnoun, [TERMPH_VAL $ make_pnoun "the_hague"]),
    ("cambridge", Pnoun, [TERMPH_VAL $ make_pnoun "cambridge"]),
    ("liverpool", Pnoun, [TERMPH_VAL $ make_pnoun "liverpool"]),
    ("arequipa", Pnoun, [TERMPH_VAL $ make_pnoun "arequipa"]),
    ("pic_du_midi", Pnoun, [TERMPH_VAL $ make_pnoun "pic_du_midi"]),
    ("flagstaff", Pnoun, [TERMPH_VAL $ make_pnoun "flagstaff"]),
    ("la_silla", Pnoun, [TERMPH_VAL $ make_pnoun "la_silla"]),
    --Begin new moons stuff--
    ("himalia",            Pnoun,     [TERMPH_VAL $ make_pnoun "himalia"]),
    ("elara",              Pnoun,     [TERMPH_VAL $ make_pnoun "elara"]),
    ("pasiphae",           Pnoun,     [TERMPH_VAL $ make_pnoun "pasiphae"]),
    ("sinope",             Pnoun,     [TERMPH_VAL $ make_pnoun "sinope"]),
    ("lysithea",           Pnoun,     [TERMPH_VAL $ make_pnoun "lysithea"]),
    ("carme",              Pnoun,     [TERMPH_VAL $ make_pnoun "carme"]),
    ("ananke",             Pnoun,     [TERMPH_VAL $ make_pnoun "ananke"]),
    ("leda",               Pnoun,     [TERMPH_VAL $ make_pnoun "leda"]),
    ("thebe",              Pnoun,     [TERMPH_VAL $ make_pnoun "thebe"]),
    ("adrastea",           Pnoun,     [TERMPH_VAL $ make_pnoun "adrastea"]),
    ("metis",              Pnoun,     [TERMPH_VAL $ make_pnoun "metis"]),
    ("callirrhoe",         Pnoun,     [TERMPH_VAL $ make_pnoun "callirrhoe"]),
    ("themisto",           Pnoun,     [TERMPH_VAL $ make_pnoun "themisto"]),
    ("megaclite",          Pnoun,     [TERMPH_VAL $ make_pnoun "megaclite"]),
    ("taygete",            Pnoun,     [TERMPH_VAL $ make_pnoun "taygete"]),
    ("chaldene",           Pnoun,     [TERMPH_VAL $ make_pnoun "chaldene"]),
    ("harpalyke",          Pnoun,     [TERMPH_VAL $ make_pnoun "harpalyke"]),
    ("kalyke",             Pnoun,     [TERMPH_VAL $ make_pnoun "kalyke"]),
    ("iocaste",            Pnoun,     [TERMPH_VAL $ make_pnoun "iocaste"]),
    ("erinome",            Pnoun,     [TERMPH_VAL $ make_pnoun "erinome"]),
    ("isonoe",             Pnoun,     [TERMPH_VAL $ make_pnoun "isonoe"]),
    ("praxidike",          Pnoun,     [TERMPH_VAL $ make_pnoun "praxidike"]),
    ("autonoe",            Pnoun,     [TERMPH_VAL $ make_pnoun "autonoe"]),
    ("thyone",             Pnoun,     [TERMPH_VAL $ make_pnoun "thyone"]),
    ("hermippe",           Pnoun,     [TERMPH_VAL $ make_pnoun "hermippe"]),
    ("aitne",              Pnoun,     [TERMPH_VAL $ make_pnoun "aitne"]),
    ("eurydome",           Pnoun,     [TERMPH_VAL $ make_pnoun "eurydome"]),
    ("euanthe",            Pnoun,     [TERMPH_VAL $ make_pnoun "euanthe"]),
    ("euporie",            Pnoun,     [TERMPH_VAL $ make_pnoun "euporie"]),
    ("orthosie",           Pnoun,     [TERMPH_VAL $ make_pnoun "orthosie"]),
    ("sponde",             Pnoun,     [TERMPH_VAL $ make_pnoun "sponde"]),
    ("kale",               Pnoun,     [TERMPH_VAL $ make_pnoun "kale"]),
    ("pasithee",           Pnoun,     [TERMPH_VAL $ make_pnoun "pasithee"]),
    ("hegemone",           Pnoun,     [TERMPH_VAL $ make_pnoun "hegemone"]),
    ("mneme",              Pnoun,     [TERMPH_VAL $ make_pnoun "mneme"]),
    ("aoede",              Pnoun,     [TERMPH_VAL $ make_pnoun "aoede"]),
    ("thelxinoe",          Pnoun,     [TERMPH_VAL $ make_pnoun "thelxinoe"]),
    ("arche",              Pnoun,     [TERMPH_VAL $ make_pnoun "arche"]),
    ("kallichore",         Pnoun,     [TERMPH_VAL $ make_pnoun "kallichore"]),
    ("helike",             Pnoun,     [TERMPH_VAL $ make_pnoun "helike"]),
    ("carpo",              Pnoun,     [TERMPH_VAL $ make_pnoun "carpo"]),
    ("eukelade",           Pnoun,     [TERMPH_VAL $ make_pnoun "eukelade"]),
    ("cyllene",            Pnoun,     [TERMPH_VAL $ make_pnoun "cyllene"]),
    ("kore",               Pnoun,     [TERMPH_VAL $ make_pnoun "kore"]),
    ("herse",              Pnoun,     [TERMPH_VAL $ make_pnoun "herse"]),
    ("epimetheus",         Pnoun,     [TERMPH_VAL $ make_pnoun "epimetheus"]),
    ("helene",             Pnoun,     [TERMPH_VAL $ make_pnoun "helene"]),
    ("telesto",            Pnoun,     [TERMPH_VAL $ make_pnoun "telesto"]),
    ("calypso",            Pnoun,     [TERMPH_VAL $ make_pnoun "calypso"]),
    ("atlas",              Pnoun,     [TERMPH_VAL $ make_pnoun "atlas"]),
    ("prometheus",         Pnoun,     [TERMPH_VAL $ make_pnoun "prometheus"]),
    ("pandora",            Pnoun,     [TERMPH_VAL $ make_pnoun "pandora"]),
    ("pan",                Pnoun,     [TERMPH_VAL $ make_pnoun "pan"]),
    ("ymir",               Pnoun,     [TERMPH_VAL $ make_pnoun "ymir"]),
    ("paaliaq",            Pnoun,     [TERMPH_VAL $ make_pnoun "paaliaq"]),
    ("tarvos",             Pnoun,     [TERMPH_VAL $ make_pnoun "tarvos"]),
    ("ijiraq",             Pnoun,     [TERMPH_VAL $ make_pnoun "ijiraq"]),
    ("suttungr",           Pnoun,     [TERMPH_VAL $ make_pnoun "suttungr"]),
    ("kiviuq",             Pnoun,     [TERMPH_VAL $ make_pnoun "kiviuq"]),
    ("mundilfari",         Pnoun,     [TERMPH_VAL $ make_pnoun "mundilfari"]),
    ("albiorix",           Pnoun,     [TERMPH_VAL $ make_pnoun "albiorix"]),
    ("skathi",             Pnoun,     [TERMPH_VAL $ make_pnoun "skathi"]),
    ("erriapus",           Pnoun,     [TERMPH_VAL $ make_pnoun "erriapus"]),
    ("siarnaq",            Pnoun,     [TERMPH_VAL $ make_pnoun "siarnaq"]),
    ("thrymr",             Pnoun,     [TERMPH_VAL $ make_pnoun "thrymr"]),
    ("narvi",              Pnoun,     [TERMPH_VAL $ make_pnoun "narvi"]),
    ("methone",            Pnoun,     [TERMPH_VAL $ make_pnoun "methone"]),
    ("pallene",            Pnoun,     [TERMPH_VAL $ make_pnoun "pallene"]),
    ("polydeuces",         Pnoun,     [TERMPH_VAL $ make_pnoun "polydeuces"]),
    ("daphnis",            Pnoun,     [TERMPH_VAL $ make_pnoun "daphnis"]),
    ("aegir",              Pnoun,     [TERMPH_VAL $ make_pnoun "aegir"]),
    ("bebhionn",           Pnoun,     [TERMPH_VAL $ make_pnoun "bebhionn"]),
    ("bergelmir",          Pnoun,     [TERMPH_VAL $ make_pnoun "bergelmir"]),
    ("bestla",             Pnoun,     [TERMPH_VAL $ make_pnoun "bestla"]),
    ("farbauti",           Pnoun,     [TERMPH_VAL $ make_pnoun "farbauti"]),
    ("fenrir",             Pnoun,     [TERMPH_VAL $ make_pnoun "fenrir"]),
    ("fornjot",            Pnoun,     [TERMPH_VAL $ make_pnoun "fornjot"]),
    ("hati",               Pnoun,     [TERMPH_VAL $ make_pnoun "hati"]),
    ("hyrrokkin",          Pnoun,     [TERMPH_VAL $ make_pnoun "hyrrokkin"]),
    ("kari",               Pnoun,     [TERMPH_VAL $ make_pnoun "kari"]),
    ("loge",               Pnoun,     [TERMPH_VAL $ make_pnoun "loge"]),
    ("skoll",              Pnoun,     [TERMPH_VAL $ make_pnoun "skoll"]),
    ("surtur",             Pnoun,     [TERMPH_VAL $ make_pnoun "surtur"]),
    ("anthe",              Pnoun,     [TERMPH_VAL $ make_pnoun "anthe"]),
    ("jarnsaxa",           Pnoun,     [TERMPH_VAL $ make_pnoun "jarnsaxa"]),
    ("greip",              Pnoun,     [TERMPH_VAL $ make_pnoun "greip"]),
    ("tarqeq",             Pnoun,     [TERMPH_VAL $ make_pnoun "tarqeq"]),
    ("aegaeon",            Pnoun,     [TERMPH_VAL $ make_pnoun "aegaeon"]),
    ("cordelia",           Pnoun,     [TERMPH_VAL $ make_pnoun "cordelia"]),
    ("ophelia",            Pnoun,     [TERMPH_VAL $ make_pnoun "ophelia"]),
    ("bianca",             Pnoun,     [TERMPH_VAL $ make_pnoun "bianca"]),
    ("cressida",           Pnoun,     [TERMPH_VAL $ make_pnoun "cressida"]),
    ("desdemona",          Pnoun,     [TERMPH_VAL $ make_pnoun "desdemona"]),
    ("juliet",             Pnoun,     [TERMPH_VAL $ make_pnoun "juliet"]),
    ("portia",             Pnoun,     [TERMPH_VAL $ make_pnoun "portia"]),
    ("rosalind",           Pnoun,     [TERMPH_VAL $ make_pnoun "rosalind"]),
    ("belinda",            Pnoun,     [TERMPH_VAL $ make_pnoun "belinda"]),
    ("puck",               Pnoun,     [TERMPH_VAL $ make_pnoun "puck"]),
    ("caliban",            Pnoun,     [TERMPH_VAL $ make_pnoun "caliban"]),
    ("sycorax",            Pnoun,     [TERMPH_VAL $ make_pnoun "sycorax"]),
    ("prospero",           Pnoun,     [TERMPH_VAL $ make_pnoun "prospero"]),
    ("setebos",            Pnoun,     [TERMPH_VAL $ make_pnoun "setebos"]),
    ("stephano",           Pnoun,     [TERMPH_VAL $ make_pnoun "stephano"]),
    ("trinculo",           Pnoun,     [TERMPH_VAL $ make_pnoun "trinculo"]),
    ("francisco",          Pnoun,     [TERMPH_VAL $ make_pnoun "francisco"]),
    ("margaret",           Pnoun,     [TERMPH_VAL $ make_pnoun "margaret"]),
    ("ferdinand",          Pnoun,     [TERMPH_VAL $ make_pnoun "ferdinand"]),
    ("perdita",            Pnoun,     [TERMPH_VAL $ make_pnoun "perdita"]),
    ("mab",                Pnoun,     [TERMPH_VAL $ make_pnoun "mab"]),
    ("cupid",              Pnoun,     [TERMPH_VAL $ make_pnoun "cupid"]),
    ("triton",             Pnoun,     [TERMPH_VAL $ make_pnoun "triton"]),
    ("naiad",              Pnoun,     [TERMPH_VAL $ make_pnoun "naiad"]),
    ("thalassa",           Pnoun,     [TERMPH_VAL $ make_pnoun "thalassa"]),
    ("despina",            Pnoun,     [TERMPH_VAL $ make_pnoun "despina"]),
    ("galatea",            Pnoun,     [TERMPH_VAL $ make_pnoun "galatea"]),
    ("larissa",            Pnoun,     [TERMPH_VAL $ make_pnoun "larissa"]),
    ("proteus",            Pnoun,     [TERMPH_VAL $ make_pnoun "proteus"]),
    ("halimede",           Pnoun,     [TERMPH_VAL $ make_pnoun "halimede"]),
    ("psamathe",           Pnoun,     [TERMPH_VAL $ make_pnoun "psamathe"]),
    ("sao",                Pnoun,     [TERMPH_VAL $ make_pnoun "sao"]),
    ("laomedeia",          Pnoun,     [TERMPH_VAL $ make_pnoun "laomedeia"]),
    ("neso",               Pnoun,     [TERMPH_VAL $ make_pnoun "neso"]),
    ("nix",                Pnoun,     [TERMPH_VAL $ make_pnoun "nix"]),
    ("hydra",              Pnoun,     [TERMPH_VAL $ make_pnoun "hydra"]),
    ("kerberos",           Pnoun,     [TERMPH_VAL $ make_pnoun "kerberos"]),
    ("styx",               Pnoun,     [TERMPH_VAL $ make_pnoun "styx"])

    ]

--todo, pull from dictionary, move to solarman
isPrep "in" = True
isPrep "at" = True
isPrep "by" = True
isPrep "using" = True
isPrep "with" = True
isPrep "to" = True
isPrep _ = False

--the rules:
--brackets are collapsed
--comma has a space after it
--expressions are separated by spaces
--if it is not punctuation, put a space after it unless a comma follows, in which case the space goes after the comma
--"that" is written as "`that`"

shouldSpace x y = isWord x && isWord y || isWord x && isOpeningBracket y || isClosingBracket x && isWord y || isClosingBracket x && isOpeningBracket y
    where
        isBracket x = elem x ["(", ")", "[", "]"]
        isClosingBracket x = elem x [")", "]"]
        isOpeningBracket x = elem x ["(", "["]
        isWord "," = False
        isWord x = not $ isBracket x

intercalateBrackets [] = ""
intercalateBrackets ("that":xs) = "`that` " `T.append` intercalateBrackets xs
intercalateBrackets (x:",":xs) = x `T.append` ", " `T.append` intercalateBrackets xs
intercalateBrackets (x:y:xs) | shouldSpace x y = x `T.append` " " `T.append` intercalateBrackets (y:xs)
intercalateBrackets (x:xs) = x `T.append` intercalateBrackets xs

--note: prepns are scoped as such: (a (b (c d)))
--TODO: This code should be moved to SolarmanTriplestore as it is tied to the syntax of that grammar
--Should keep a version in here which works as it did before
flattenPreps :: SyntaxTree -> [SyntaxTree]
flattenPreps qs@(SyntaxTreeNT ((SyntaxTreeT prep):_)) = [qs]
flattenPreps (SyntaxTreeNT [x@(SyntaxTreeNT _), a]) = x:(flattenPreps a)

syntaxTreeToLinearPreps :: SyntaxTree -> [T.Text]
syntaxTreeToLinearPreps list = intercalate [","] $ map fmt $ flattenPreps list
        where fmt (SyntaxTreeNT t) = concatMap syntaxTreeToLinear t

syntaxTreeToLinear :: SyntaxTree -> [T.Text]
syntaxTreeToLinear (SyntaxTreeT x) = [x]
syntaxTreeToLinear (SyntaxTreeNT ts@((SyntaxTreeT prep):_)) | isPrep prep = ["["] ++ concatMap syntaxTreeToLinear ts ++ ["]"]
syntaxTreeToLinear ts@(SyntaxTreeNT ((SyntaxTreeNT ((SyntaxTreeT prep):_)):_)) | isPrep prep = ["["] ++ syntaxTreeToLinearPreps ts ++ ["]"]
syntaxTreeToLinear (SyntaxTreeNT ts) = ["("] ++ concatMap syntaxTreeToLinear ts ++ [")"]

syntaxTreeToLinear' :: SyntaxTree -> T.Text
syntaxTreeToLinear' (SyntaxTreeT x) = x
syntaxTreeToLinear' (SyntaxTreeNT ts) = intercalateBrackets $ concatMap syntaxTreeToLinear ts

parse i = formatAttsFinalAlt QUEST_VAL Question (Vector.length vWords + 1) $ snd $ test (question T0 empty_insattvals) (vWords)
    where
        vWords = Vector.fromList $ T.words i
parseTree i = findAllParseTreesFormatted syntaxTreeToLinear' QUEST_VAL Question (Vector.length vWords + 1) $ snd $ test (question T0 empty_insattvals) (vWords)
    where
        vWords = Vector.fromList $ T.words i

headParse = getQUVAL . head . parse

input = Vector.fromList $ T.words i1

i1 = "which moons that were discovered by hall orbit mars" -- OK
i2 = "who discovered a moon that orbits mars" -- OK
i3 = "did hall discover every moon that orbits mars" -- OK
i4 = "how many moons were discovered by hall and kuiper" -- OK
i5 = "how many moons were discovered by hall or kuiper" -- OK
i6 = "every moon was discovered by a person" -- OK
i7 = "which planets are orbited by a moon that was discovered by galileo" -- OK
i8 = "which moons were discovered by nobody" -- OK
i9 = "is every planet orbited by a moon" -- Broken in original too
i10 = "which planets are orbited by two moons" -- OK
i11 = "who was the discoverer of phobos" -- Broken in original too
i12 = "hall discovered a moon that orbits mars" -- OK
i13 = "which moons that orbit mars were discovered by hall" -- OK
i14 = "every moon orbits every planet" -- OK
i15 = "every planet is orbited by a moon" -- OK
i16 = "a planet is orbited by a moon" -- OK
i17 = "does phobos orbit mars" -- OK
--
i18 = "did hall discover deimos or phobos and miranda" -- Broken in original too
i19 = "did hall discover deimos or phobos and miranda or deimos and deimos" -- Broken in original too
