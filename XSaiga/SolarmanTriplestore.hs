{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE OverloadedStrings #-}

module XSaiga.SolarmanTriplestore where

import Prelude hiding ((*>), words, unwords, concat, concatMap, null)
import XSaiga.Getts
import Data.List as List hiding (words, unwords)
import qualified Data.Text as T
import qualified Data.Set as Set
import XSaiga.AGParser2
import XSaiga.TypeAg2
import Control.Monad
import Debug.Trace
import XSaiga.ShowText
import Control.Monad.State.Lazy
import Control.Applicative hiding ((*>), (<|>))
import Data.Biapplicative
import Data.Bifunctor
import qualified Data.Map as Map

--copied from gangster_v4: utility functions for making lists unique
subset s t = (s \\ t) == []

--TODO: MERGE IMAGES PROPER
termor' :: (TF FDBR -> TF FDBR) -> (TF FDBR -> TF FDBR) -> TF FDBR -> TF FDBR
termor' tmph1 tmph2 ents = union_fdbr' (tmph1 ents) (tmph2 ents)

--copied from gangster_v4: combinators
termor :: SemFunc ((TF FDBR -> TF FDBR) -> (TF FDBR -> TF FDBR) -> TF FDBR -> TF FDBR)
termor = bipure termor' (\g1 -> \g2 -> \g3 -> gettsIntersect (gettsUnion (gettsApply g1) (gettsApply g2)) g3)

termand' :: (TF FDBR -> TF FDBR) -> (TF FDBR -> TF FDBR) -> TF FDBR -> TF FDBR
termand' tmph1 tmph2 ents r = if not (List.null $ tmph1 ents r) && not (List.null $ tmph2 ents r) then termor' tmph1 tmph2 ents r else []

--May need to be changed to intersection?  Don't think so:  can't remove anything from nub (t1++t2) because all things are relevant to either t1 or t2
--TODO: MERGE IMAGES PROPER (or do termphrases always preserve ents)
termand = bipure termand' (\g1 -> \g2 -> \g3 -> gettsIntersect (gettsUnion (gettsApply g1) (gettsApply g2)) g3)

--TODO: FDBRs are sorted.  Use that to improve this.
intersect_fdbr'' _ [] = []
intersect_fdbr'' [] _ = []
intersect_fdbr'' fdbr1@((e1, evs1):eei1)  fdbr2@((e2, evs2):eei2)
  = case compare e1 e2 of
      LT -> intersect_fdbr'' eei1 fdbr2
      EQ -> (e2, evs2):(intersect_fdbr'' eei1 eei2) 
      GT -> intersect_fdbr'' fdbr1 eei2

{-intersect_fdbr'' eei1 eei2
  = [(subj2, evs2) | (subj1, evs1) <- eei1, (subj2, evs2) <- eei2, subj1 == subj2]-}

intersect_fdbr' :: TF FDBR -> TF FDBR -> TF FDBR
intersect_fdbr' = liftA2 intersect_fdbr'' --intersect_fdbr'' <$> tf1 <*> tf2

intersect_fdbr :: SemFunc (TF FDBR -> TF FDBR -> TF FDBR)
intersect_fdbr = bipure intersect_fdbr' gettsIntersect

union_fdbr'' :: FDBR -> FDBR -> FDBR
union_fdbr'' fdbr1 fdbr2 = Map.toList $ Map.fromListWith (++) (fdbr1 ++ fdbr2)

union_fdbr' :: TF FDBR -> TF FDBR -> TF FDBR
union_fdbr' = liftA2 union_fdbr''

union_fdbr :: SemFunc (TF FDBR -> TF FDBR -> TF FDBR)
union_fdbr = bipure union_fdbr' gettsUnion

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

every' :: TF FDBR -> TF FDBR -> TF FDBR
every' = liftA2 every''

every :: SemFunc (TF FDBR -> TF FDBR -> TF FDBR)
every = bipure every' gettsIntersect

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
one'' nph vbph   | length (res) == 1 = res
                | otherwise = []
    where
      res = intersect_fdbr'' nph vbph

one' :: TF FDBR -> TF FDBR -> TF FDBR
one' = liftA2 one''

one = bipure one' gettsIntersect

two'' nph vbph   | length (res) == 2 = res
                | otherwise = []
    where
      res = intersect_fdbr'' nph vbph

two' = liftA2 two'' 

two = bipure two' gettsIntersect

--which nph vbph = if result /= [] then result else "none."
--  where result = unwords $ intersect nph vbph

which'' :: FDBR -> FDBR -> T.Text
which'' nph vbph = if not $ T.null result then result else "none."
  where
  result = T.unwords $ map fst $ intersect_fdbr'' nph vbph

which :: SemFunc (TF FDBR -> TF FDBR -> TF T.Text)
which = bipure (liftA2 which'') gettsIntersect

how_many'' nph vbph = tshow $ List.length (intersect_fdbr'' nph vbph)
how_many = bipure (liftA2 how_many'') gettsIntersect

who = which <<*>> (nounor <<*>> (get_members "person") <<*>> (get_members "science_team"))

--New
what'' nph = if not $ T.null result then result else "nothing."
    where result = T.unwords $ map fst nph

what = bipure (liftA what'') id

--TODO: prepositions
make_prep props = bipure (\tmph -> (props, tmph)) (gettsApply)

make_prep_nph props = bipure (\nph -> (props, intersect_fdbr' nph)) (id)

--with :: (TF FDBR -> TF FDBR) -> ([T.Text], TF FDBR -> TF FDBR)
with = make_prep ["with_implement"]

by = make_prep ["subject"]

at = make_prep ["location"]

make_pnoun'' noun image = [(subj, evs) | (subj, evs) <- image, subj == noun]

make_pnoun :: T.Text -> SemFunc (TF FDBR -> TF FDBR)
make_pnoun noun = bipure (liftA $ make_pnoun'' noun) id

--TODO: ugly hack to work around parser problem
make_year = make_pnoun . tshow

in' = make_prep ["location", "year"]

--TODO: verify "subject" and identity here.  should not be introducing more info...
--to = bipure (\nph -> (["subject"], intersect_fdbr' nph)) (id)
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

--TODO: attach info!
make_prop_termphrase :: T.Text -> SemFunc (TF FDBR -> TF T.Text)
make_prop_termphrase prop = make_prop_termphrase' prop >|< gettsAttachP prop

where' = make_prop_termphrase "location"
when' = make_prop_termphrase "year"
how' = make_prop_termphrase "with_implement"

--needs special handling due to semantics requiring info from getts
whatobj :: SemFunc (TF FDBR) -> SemFunc (TF T.Text)
whatobj  (fdbr, getts) = make_prop_termphrase' (findFirstObj getts) fdbr >|< gettsAttachPO getts
  where
    findFirstObj (GettsIntersect _ y) = findFirstObj y
    findFirstObj (GettsTP _ (_,_,object) _) = object
    gettsAttachPO (GettsIntersect x y) = GettsIntersect x (gettsAttachPO y)
    gettsAttachPO (GettsTP props (rel, subject, object) sub) = GettsTP (object:props) (rel, subject, object) sub

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
filter_ev :: [([T.Text], TF FDBR -> TF FDBR)] -> [Event] -> TF [Event]
filter_ev [] evs ev_data = evs
filter_ev ((names,pred):list) evs triples
  = if not $ List.null res then filter_ev list relevant_evs triples else []
  where  
  relevant_triples = List.filter (\(x, _, _) -> x `elem` evs) triples -- only get triples with our events
  relevant_list = concatMap (\name -> make_fdbr_with_prop relevant_triples name) names 
  res = pred (pure relevant_list) triples --TODO: prove correct
  --NEW: Merge all events in predicate result for new query.  Result will be a subset of evs.
  relevant_evs = List.nub $ concatMap snd res

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

gettsTP :: Voice -> Relation -> GettsTree -> GettsTree
gettsTP voice rel (GettsPreps props' sub) = GettsTP (subject:props') rel sub
  where
    (subject,_) = getVoiceProps voice rel

relname (a, _, _) = a

make_trans'' :: Voice -> Relation -> [([T.Text], (TF FDBR -> TF FDBR))] -> TF FDBR
make_trans'' voice rel preps rtriples = filter (not . List.null . snd) fdbrRelevantEvs
  where
    (subjectProp,_) = getVoiceProps voice rel
    filtRTriples = pure_getts_triples_entevprop_type rtriples (subjectProp:(nub $ concatMap fst $ preps)) (relname rel)
    images = make_fdbr_with_prop filtRTriples subjectProp
    fdbrRelevantEvs = map (\(subj, evs) -> (subj, filter_ev preps evs rtriples)) images

--make_trans_active' "discover_ev" <<*>> (gatherPreps [at us_naval_observatory, in' 1877])
--TODO: rtriples is used directly?? is this correct?
--TODO: refactor to take into account active tmph
--TODO: refactor both passive and active into same function (active version may involve more)

--SCHEME
--empty denotes tmph
--' denotes preps
--'' denotes tmph followed by preps

make_trans_active' :: Relation -> SemFunc ([([T.Text], (TF FDBR -> TF FDBR))] -> TF FDBR)
make_trans_active' rel =  make_trans'' ActiveVoice rel >|< gettsTP ActiveVoice rel

--TODO: Bug in solarman3 semantics here with only "subject" in GettsTP
--make_trans_active :: T.Text -> SemFunc ((TF FDBR -> TF FDBR)  -> TF FDBR)
--make_trans_active ev_type = (\tmph_sem -> make_trans_active'' ev_type [(["object"], tmph_sem)]) >|< (\g -> GettsTP ["subject", "object"] ev_type [gettsApply g])

make_trans_active :: Relation -> SemFunc ((TF FDBR -> TF FDBR) -> TF FDBR)
make_trans_active rel = (\tmph_sem -> f [([object], tmph_sem)]) >|< (\getts -> g $ GettsPreps [object] [gettsApply getts])
  where
    (_, object) = getVoiceProps ActiveVoice rel
    (f, g) = make_trans_active' rel

make_trans_active'' :: Relation -> SemFunc ((TF FDBR -> TF FDBR) -> [([T.Text], (TF FDBR -> TF FDBR))] -> TF FDBR)
make_trans_active'' rel = (\tmph_sem -> \preps -> f (([object], tmph_sem):preps)) >|< (\getts -> \preps -> g (addPrep object getts preps))
  where
    (subject, object) = getVoiceProps ActiveVoice rel
    (f, g) = make_trans_active' rel
    addPrep :: T.Text -> (GettsTree -> GettsTree) -> GettsTree -> GettsTree
    addPrep prop getts (GettsPreps props subs) = GettsPreps (prop:props) ((gettsApply getts):subs)

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

make_trans_passive :: Relation -> SemFunc ([([T.Text], (TF FDBR -> TF FDBR))] -> TF FDBR)
make_trans_passive rel = make_trans'' PassiveVoice rel >|< gettsTP PassiveVoice rel

--Copied from old solarman:
yesno' x = if x /= [] then "yes." else "no"
yesno = bipure (liftA yesno') id

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
sand'' [] _ = []
sand'' _ [] = []
sand'' fdbr1 fdbr2 = union_fdbr'' fdbr1 fdbr2

sand = bipure (liftA2 sand'') gettsUnion

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

pnoun           =  memoize_terminals_from_dictionary Pnoun
cnoun           =  memoize_terminals_from_dictionary Cnoun
adj             =  memoize_terminals_from_dictionary Adj
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
year            =  memoize_terminals_from_dictionary Year

memoize_terminals_from_dictionary key
  = let key_words              = filter (\(_,type',_) -> type' == key) dictionary
        list_of_terms          = map (\(a, _, z) -> terminal a z) key_words
        altTerminals           = foldr1 (<|>) list_of_terms
    in  memoize key altTerminals

meaning_of p dInp key
 = let dInput     = T.words dInp
       appParser  = runState (p T0 [] ((1,[]), dInput) ([],[])) []
       upperBound = (List.length dInput) + 1
   in  formFinal key upperBound (snd $ appParser)

meaning_of_ p dInp key
 = let dInput     = T.words dInp
       appParser  = runState (p T0 [] ((1,[]), dInput) ([],[])) []
       upperBound = (List.length dInput) + 1
   in  (snd $ appParser)

formAtts key ePoint t
 = List.concat $ List.concat $ List.concat $ List.concat
   [[[[  val1 |(id1,val1)<-synAtts]
           |(((st,inAtt2),(end,synAtts)), ts)<-rs, st == 1 && end == ePoint]
            |((i,inAt1),((cs,ct),rs)) <- sr ]
             |(s,sr) <- t, s == key ]
formFinal key ePoint t
 = List.concat $ List.concat $ List.concat $ List.concat
   [[[[  val1 |(id1,val1)<-synAtts]
           |(((st,inAtt2),(end,synAtts)), ts)<-rs, st == 1 && end == ePoint]
            |((i,inAt1),((cs,ct),rs)) <- sr ]
             |(s,sr) <- t, s == key ]
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

----------------------------------------------------------------------------------
--NEW FOR PREPOSITIONAL PHRASES

-- public <preps> = <prepph> | <preph> <preps>;
preps
 = memoize Preps
    (parser (nt prepph S1)
     [rule_s PREP_VAL OF LHS ISEQUALTO applyprep [synthesized PREPPH_VAL OF S1]]
     <|>
     parser (nt prepph S1 *> nt preps S2)
     [rule_s PREP_VAL OF LHS ISEQUALTO applypreps [synthesized PREPPH_VAL OF S1,
                                                   synthesized PREP_VAL OF S2]]
    )

-- public <prepph> = <prep> <jointermph>;
prepph
 = memoize Prepph
    (parser (nt prep S1 *> nt jointermph S2)
     [rule_s PREPPH_VAL OF LHS ISEQUALTO applyprepph [synthesized PREPN_VAL OF S1,
                                                     synthesized TERMPH_VAL OF S2]]
     <|>
     parser (nt prepnph S1 *> nt verbph S2)
     [rule_s PREPPH_VAL OF LHS ISEQUALTO applyprepph_nph [synthesized PREPNPH_VAL OF S1,
                                                         synthesized VERBPH_VAL OF S2]] 
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
   <|>
   parser (nt year S3)
   [rule_s TERMPH_VAL OF LHS ISEQUALTO applyyear [synthesized YEAR_VAL OF S3]]
   )

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
applyBiOp [e1,op,e2]
                  = \atts -> VAL ((getAtts getB_OP atts op ) (getAtts getAVAL atts e1 ) (getAtts getAVAL atts e2))

-}
-- getAtts f (y,i) x = f (head (x y i))
-- copy      [b]     = \(atts,i) -> head (b atts i)

intrsct1         [x, y]
 = \atts -> NOUNCLA_VAL (intersect_fdbr <<*>> (getAtts getAVALS atts x) <<*>> (getAtts getAVALS atts y))

intrsct2         [x, y]
 = \atts -> ADJ_VAL (intersect_fdbr <<*>> (getAtts getAVALS atts x) <<*>> (getAtts getAVALS atts y))

applydet         [x, y]
 = \atts -> TERMPH_VAL $ (getAtts getDVAL atts x) <<*>> (getAtts getAVALS atts y)

--make_trans_vb is very similar to make_trans_active.  getBR must mean "get binary relation"
--getTVAL must mean "get predicate" i.e. what would be "phobos" in "discover phobos"
--Changed to get rid of make_trans_vb, since the getBR attribute was changed to not
--be a binary relation but instead a function that make_trans_active would give
--I.e., the kind of function that make_trans_vb would have generated, since they were
--nearly identical

--NEW FOR PREPOSITIONAL PHRASES
applytransvbprep [x,y,z] atts = VERBPH_VAL $ make_trans_active' reln <<*>> gatherPreps ((make_prep [object] <<*>> predicate) : preps)
    where
    reln = getAtts getBR atts x
    predicate = getAtts getTVAL atts y
    preps = getAtts getPREPVAL atts z
    (_, object) = getVoiceProps ActiveVoice reln

applytransvbprep [x,y] atts = VERBPH_VAL $ make_trans_active' reln <<*>> gatherPreps [make_prep [object] <<*>> predicate]
    where
    reln = getAtts getBR atts x
    predicate = getAtts getTVAL atts y
    (_, object) = getVoiceProps ActiveVoice reln

applytransvb_no_tmph [x,y] atts = VERBPH_VAL $ make_trans_active' reln <<*>> gatherPreps preps
    where
    reln = getAtts getBR atts x
    preps = getAtts getPREPVAL atts y

applytransvb_no_tmph [x] atts = VERBPH_VAL $ make_trans_active' reln <<*>> gatherPreps []
    where
    reln = getAtts getBR atts x

--TODO: modify grammar so you can't ask "what was phobos discover", or if you can, make the answer sensible (e.g. hall, not phobos)
apply_quest_transvb_passive (x2:x3:x4:xs) atts = VERBPH_VAL $ termph <<*>> (make_trans_passive reln <<*>> gatherPreps preps)
    where
    linkingvb = getAtts getLINKVAL atts x2
    termph = getAtts getTVAL atts x3
    reln = getAtts getBR atts x4
    preps = case xs of
        [] -> []
        (x5:_) -> getAtts getPREPVAL atts x5

applyprepph     [x, y]
 = \atts -> PREPPH_VAL $
        let prep_names = getAtts getPREPNVAL atts x
            termph = getAtts getTVAL atts y in
            make_prep prep_names <<*>> termph

applyprepph_nph [x, y]
 = \atts -> PREPPH_VAL $
        let prep_names = getAtts getPREPNPHVAL atts x
            nph = getAtts getAVALS atts y in
            make_prep_nph prep_names <<*>> nph

applyprep   [x]
  = \atts -> PREP_VAL $ [(getAtts getPREPPHVAL atts x)] --[(["with_implement"], a telescope)]

applypreps      [x, y]
  = \atts -> PREP_VAL $ (getAtts getPREPPHVAL atts x) : (getAtts getPREPVAL atts y)

applyyear [x]
  = \atts -> TERMPH_VAL $ make_pnoun $ tshow $ getAtts getYEARVAL atts x

--END PREPOSITIONAL PHRASES

applyvbph        [z]
 = \atts -> VERBPH_VAL (getAtts getAVALS atts z)

appjoin1         [x, y, z]
 = \atts -> TERMPH_VAL $ (getAtts getTJVAL atts y) <<*>> (getAtts getTVAL atts x) <<*>> (getAtts getTVAL atts z)

appjoin2         [x, y, z]
 = \atts -> VERBPH_VAL ((getAtts getVJVAL atts y) <<*>> (getAtts getAVALS atts x) <<*>> (getAtts getAVALS atts z))

apply_middle1    [x, y, z]
 = \atts -> NOUNCLA_VAL ((getAtts getRELVAL atts y) <<*>> (getAtts getAVALS atts x) <<*>> (getAtts getAVALS atts z))

apply_middle2    [x, y, z]
 = \atts -> NOUNCLA_VAL ((getAtts getNJVAL atts y) <<*>> (getAtts getAVALS atts x) <<*>> (getAtts getAVALS atts z))

apply_middle3    [x, y, z]
 = \atts -> NOUNCLA_VAL ((getAtts getRELVAL atts y) <<*>> (getAtts getAVALS atts x) <<*>> (getAtts getAVALS atts z))

-- Think "a orbited by b" vs "b orbits a"
{-drop3rd          [w, x, y, z]
 = \atts -> VERBPH_VAL $
        let reln = getAtts getBR atts x in
        let predicate = getAtts getTVAL atts z in
        make_inverted_relation dataStore reln predicate-}

--NEW FOR PREPOSITIONAL PHRASES
drop3rdprep (w:x:xs) atts = VERBPH_VAL $ make_trans_passive reln <<*>> gatherPreps preps
        where
        reln = getAtts getBR atts x
        preps = case xs of
                  [] -> []
                  (p:_) -> getAtts getPREPVAL atts p
--END PREPOSITIONAL PHRASES

apply_termphrase [x, y]
 = \atts -> SENT_VAL ((getAtts getTVAL atts x) <<*>> (getAtts getAVALS atts y))

sent_val_comp    [s1, f, s2]
 = \atts -> SENT_VAL ((getAtts getSJVAL atts f) <<*>> (getAtts getSV atts s1) <<*>> (getAtts getSV atts s2))

ans1             [x, y]
 = \atts -> QUEST_VAL ((getAtts getQU1VAL atts x) <<*>> (getAtts getSV atts y) )

ans2             [x, y]
 = \atts -> QUEST_VAL ((getAtts getQU2VAL atts x) <<*>> (getAtts getAVALS atts y))

ans3             [x, y, z]
 = \atts -> QUEST_VAL ((getAtts getQU3VAL atts x) <<*>> (getAtts getAVALS atts y) <<*>> (getAtts getAVALS atts z))

ans5             [x, y, z]
 = \atts -> QUEST_VAL ((getAtts getQU2VAL atts x) <<*>> (getAtts getSV atts z))

ans6             [x, y, z]
 = \atts -> QUEST_VAL ((getAtts getQU6VAL atts x) (getAtts getSV atts z))



truefalse        [x]
  = \atts -> QUEST_VAL $ fmap (\fdbr -> if not (List.null fdbr) then "true." else "false.") `first` (getAtts getSV atts x)

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
    ("bernard",            Pnoun,     [TERMPH_VAL $ make_pnoun "bernard"]),
    ("bond",               Pnoun,     [TERMPH_VAL $ make_pnoun "bond"]),
    ("venus",              Pnoun,     [TERMPH_VAL $ make_pnoun "venus"]),
    ("cassini",            Pnoun,     [TERMPH_VAL $ make_pnoun "cassini"]),
    ("dollfus",            Pnoun,     [TERMPH_VAL $ make_pnoun "dollfus"]),
    --("fouuntain",          Pnoun,     [TERMPH_VAL $ make_pnoun "Fouuntain"]),
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
    ("is",                 Linkingvb, [LINKINGVB_VAL $ bipure (liftA id) id]),
    ("was",                Linkingvb, [LINKINGVB_VAL $ bipure (liftA id) id]),
    ("are",                Linkingvb, [LINKINGVB_VAL $ bipure (liftA id) id]),
    ("were",               Linkingvb, [LINKINGVB_VAL $ bipure (liftA id) id]),
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
    ("in",          Prepn, [PREPN_VAL ["location","year"]]),
    ("at",          Prepn, [PREPN_VAL ["location"]]),
    ("by",          Prepn, [PREPN_VAL ["subject"]]),
    ("to",          Prepnph, [PREPNPH_VAL ["subject"]]),
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

    ] ++ list_of_years

{-Major hack: Since the basic unit that the parser understands is strings (not characters), we have to manually add all years that we can query into the dictionary...
That is, we can't make the parser understand "1984" and "1245" by having it recognize four numbers, instead it must recognize the entire string of numbers at once
as a terminal (i.e., "1984" would be a terminal, not a non-terminal composed of "1", "9", "8", and "4").  Therefore, all possible strings must be added to the dictionary so that the parser can match them.

**this might need to be altered**
-}

list_of_years = map (\n -> (tshow n, Year, [YEAR_VAL n])) $ List.concat [[1000 + x, 2000 + x] | x <- [0..999]]

--test1 p p_ inp = do putStr  $ render80 $ format{-Atts p_-} $ snd $ runState (p T0 [] ((1,[]),words inp) ([],[])) []
test p input = runState (p ((1,[]),input) ([],[])) []

parse i = formatAttsFinalAlt Question  ((List.length $ T.words i)+1) $ snd $ test (question T0 []) (T.words i)

headParse = getQUVAL . head . parse

--formatParseIO = mapM id . map showio . parse

findStart st ((s,ss):rest) | s == st   = [(s,ss)]
                           | otherwise = findStart st rest
findStart st []                        = []

input = T.words i1

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
