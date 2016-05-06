{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE Trustworthy #-}
module SolarmanTriplestore where

import Prelude hiding ((*>))
import Getts
import Data.List as List
import qualified Data.Set as Set
import AGParser2
import TypeAg2
import Control.Monad
import Debug.Trace
import qualified LocalData as Local

--change between remoteData and localData
--dataStore = Local.localData
dataStore = remoteData -- selects database

endpoint_uri = "http://speechweb2.cs.uwindsor.ca/sparql"
namespace_uri = "http://solarman.richard.myweb.cs.uwindsor.ca#"
remoteData = SPARQL endpoint_uri namespace_uri
    
--copied from gangster_v4: utility functions for making lists unique
subset s t = (s \\ t) == []

makeset x = Set.toList $ Set.fromList x
    
--copied from gangster_v4: combinators
termor tmph1 tmph2 ents =
        liftM List.nub $ liftM2 (++) (tmph1 ents) (tmph2 ents) --TODO: MERGE IMAGES PROPER

termand tmph1 tmph2 ents = do
        t1 <- tmph1 ents
        t2 <- tmph2 ents
        if t1 /= [] && t2 /= [] then return $ List.nub $ t1 ++ t2 else return []
        --May need to be changed to intersection?  Don't think so:  can't remove anything from nub (t1++t2) because all things are relevant to either t1 or t2
        --TODO: MERGE IMAGES PROPER (or do termphrases always preserve ents)

intersect_entevimages eei1  eei2 
                = [(subj2, evs2) | (subj1, evs1) <- eei1, (subj2, evs2) <- eei2, subj1 == subj2]
        
that = nounand

nounand = liftM2 intersect_entevimages

nounor' s t = List.nub(s ++ t)
nounor = liftM2 nounor'
       
{-a' nph vbph =
    length (intersect  nph vbph) /= 0-}
a = liftM2 intersect_entevimages    
any' = a
the = a
some = a
an = a

every' nph vbph | subset (map fst nph) (map fst vbph) = intersect_entevimages nph vbph
                | otherwise = []

every = liftM2 every'

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
  
one' nph vbph   | length (res) == 1 = res
                | otherwise = []
    where
    res = intersect_entevimages  nph vbph
    
one = liftM2 one'

two' nph vbph   | length (res) == 2 = res
                | otherwise = []
    where
    res = intersect_entevimages  nph vbph
    
two = liftM2 two'
  

--which nph vbph = if result /= [] then result else "none."
--  where result = unwords $ intersect nph vbph

which nph vbph = do
    nph_ <- nph
    vbph_ <- vbph
    let result = unwords $ map fst $ intersect_entevimages nph_ vbph_
    return $ if result /= [] then result else "none."
    
how_many' nph vbph =
    show $ length (intersect_entevimages nph vbph)
how_many = liftM2 how_many'

who = which ((get_members dataStore "person") `nounor` (get_members dataStore "science_team"))

--New
what' nph = if result /= [] then result else "nothing."
    where result = unwords $ map fst nph
what = liftM what'

with :: (IO Image -> IO Image) -> ([String], IO Image -> IO Image)
with tmph = (["with_implement"], tmph)

by tmph = (["subject"], tmph)

at tmph = (["location"], tmph)

in' tmph = (["location", "year"], make_pnoun $ show tmph)

make_pnoun noun = liftM $ make_pnoun' noun
make_pnoun' noun image = [(subj, evs) | (subj, evs) <- image, subj == noun]

--New for new new semantics

--TODO: Handle case where nothing is in props (return none)
make_prop_termphrase prop nph = do
    list <- nph
    props <- mapM (\(_,y) -> getts_inverse dataStore prop y >>= \loc -> return (map fst loc)) list
    let finalList = unwords $ List.nub $ concat props
    return $ if finalList /= [] then finalList else "nothing."

where' = make_prop_termphrase "location"
when' = make_prop_termphrase "year"
how' = make_prop_termphrase "with_implement"
whatobj = make_prop_termphrase "object"


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

{-discover = make_relation "discover_ev" 
discovered = discover

orbit = make_relation "orbit_ev" 
orbited = orbit-}

--For prepositional phrases
{-discover' = make_relation "discover_ev"
discovered' = discover'

orbit' = make_relation "orbit_ev" 
orbited' = orbit'-}

{-hall = make_pnoun "hall"
phobos = make_pnoun "phobos"
mars = make_pnoun "mars"
refractor_telescope_1 = make_pnoun "refractor_telescope_1"
-}

make_relation ev_type tmph = make_filtered_relation dataStore ev_type [(["object"],tmph)]

                
{-make_inverted_relation :: (TripleStore m) => m -> String -> (IO [String] -> IO Bool) -> IO [String]
make_inverted_relation ev_data rel tmph = do
        images <- make_image ev_data rel "object"
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
{-filter_ev :: (TripleStore m) => m -> [([String], IO Image -> IO Image)] -> [Event] -> IO Bool
filter_ev _ [] _ = return True
filter_ev ev_data ((names,pred):list) evs = do
    relevant_list <- mapM (\name -> getts_inverse ev_data name evs) names
    res <- pred $ return $ concat $ relevant_list
    if res /= [] then filter_ev ev_data list evs else return False-}
    
--new filter_ev: Handles prepositional phrases (IN TESTING)
filter_ev :: (TripleStore m) => m -> [([String], IO Image -> IO Image)] -> [Event] -> IO Bool
filter_ev _ [] _ = return True
filter_ev ev_data ((names,pred):list) evs = do
    relevant_list <- mapM (\name -> getts_inverse ev_data name evs) names
    res <- pred $ return $ concat $ relevant_list
    --NEW: Merge all events in predicate result for new query.  Result will be a subset of evs.
    let relevant_evs = List.nub $ concatMap snd res 
    if res /= [] then filter_ev ev_data list relevant_evs else return False

{-make_filtered_relation :: (TripleStore m) => m -> String -> (IO [String] -> IO Bool) -> [([String], IO [String] -> IO Bool)] -> IO [String]
make_filtered_relation ev_data rel tmph preps = do
    images <- make_image ev_data rel "subject"
    subPairs <- filterM (\(_, evs) -> do
        filtEvents <- filterM (filter_ev ev_data preps) evs
        tmph $ liftM concat $ mapM (\ev -> getts_3 ev_data (ev, "object", "?")) filtEvents) images
    return $ map fst subPairs-}
    
--Modified version of make_filtered_relation to accomodate new filter_ev
make_filtered_relation :: (TripleStore m) => m -> String -> [([String], IO Image -> IO Image)] -> IO Image
make_filtered_relation ev_data rel preps = do
    images <- make_image ev_data rel "subject"
    filterM (\(_, evs) -> filter_ev ev_data preps evs) images
    
{-make_inverted_filtered_relation :: (TripleStore m) => m -> String -> [([String], IO [String] -> IO Bool)] -> IO [String]
make_inverted_filtered_relation ev_data rel preps = do
    images <- make_image ev_data rel "object"
    objPairs <- filterM (\(_, evs) -> anyM (filter_ev ev_data preps) evs) images
    return $ map fst objPairs
    where
        anyM :: (Monad m) => (a -> m Bool) -> [a] -> m Bool
        anyM pred lst = foldM (\x y -> pred y >>= \res -> return $ x || res) False lst -}

--Modified version of make_inverted_filtered_relation to accomodate new filter_ev
make_inverted_filtered_relation :: (TripleStore m) => m -> String -> [([String], IO Image -> IO Image)] -> IO Image
make_inverted_filtered_relation ev_data rel preps = do
    images <- make_image ev_data rel "object"
    filterM (\(_, evs) -> filter_ev ev_data preps evs) images

--Copied from old solarman:
yesno' x = if x /= [] then "yes." else "no"
yesno = liftM yesno'

does = yesno
did = yesno
do' = yesno
was = yesno
is = yesno
were = yesno
are = yesno

--TODO: is this proper? 
sand s1 s2 = do
    r1 <- s1
    r2 <- s2
    return $ if r1 /= [] && r2 /= [] then List.nub $ r1 ++ r2 else []

{-
||-----------------------------------------------------------------------------
||  BASIC INTERPRETERS
||-----------------------------------------------------------------------------
-}


pnoun           =  pre_processed Pnoun
cnoun           =  pre_processed Cnoun
adj             =  pre_processed Adj
det             =  pre_processed Det
intransvb       =  pre_processed Intransvb
transvb         =  pre_processed Transvb
linkingvb       =  pre_processed Linkingvb
relpron         =  pre_processed Relpron
termphjoin      =  pre_processed Termphjoin
verbphjoin      =  pre_processed Verbphjoin
nounjoin        =  pre_processed Nounjoin
indefpron       =  pre_processed Indefpron
{-
terminator      =  uninterpreted (SPECIAL_SYMBOL_TERM ".")
                   $orelse
                   uninterpreted (SPECIAL_SYMBOL_TERM "?")
                   $orelse
                   uninterpreted (SPECIAL_SYMBOL_TERM "\n")
-}
sentjoin        =  pre_processed Sentjoin
quest1          =  pre_processed Quest1
quest2          =  pre_processed Quest2
quest3          =  pre_processed Quest3
quest4a         =  pre_processed Quest4a
quest4b         =  pre_processed Quest4b
quest5          =  pre_processed Quest5
quest6          =  pre_processed Quest6

--NEW FOR PREPOSITIONAL PHRASES
prep            =  pre_processed Prepn
year            =  pre_processed Year


pre_processed key 
 = let formAlts altTerminals  = memoize key (altTerminals) 
       formTerminal [x]       = x
       formTerminal (x:xs)    = x <|>  formTerminal xs
       list_of_ters           = [ terminal (term a) z 
                                | (a,b,z) <- dictionary
                                , b == key]
   in  formAlts (formTerminal list_of_ters)


meaning_of p dInp key
 = let dInput     = words dInp
       appParser  = unState (p T0 [] ((1,[]), dInput) ([],[])) [] 
       upperBound = (length dInput) + 1
   in  formFinal key upperBound (snd $ appParser)   

meaning_of_ p dInp key
 = let dInput     = words dInp
       appParser  = unState (p T0 [] ((1,[]), dInput) ([],[])) [] 
       upperBound = (length dInput) + 1
   in  (snd $ appParser)   

formAtts key ePoint t 
 = concat $ concat $ concat $ concat  
   [[[[  val1 |(id1,val1)<-synAtts]
           |(((st,inAtt2),(end,synAtts)), ts)<-rs, st == 1 && end == ePoint]     
            |((i,inAt1),((cs,ct),rs)) <- sr ]
             |(s,sr) <- t, s == key ]
formFinal key ePoint t 
 = concat $ concat $ concat $ concat  
   [[[[  val1 |(id1,val1)<-synAtts]
           |(((st,inAtt2),(end,synAtts)), ts)<-rs, st == 1 && end == ePoint]     
            |((i,inAt1),((cs,ct),rs)) <- sr ]
             |(s,sr) <- t, s == key ]            
{-
test p = unState (p ((1,[]),input) ([],[])) [] 

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
    parser (nt quest6 S1  *> nt quest1 S2  *>  nt sent S3 )
    [rule_s QUEST_VAL  OF LHS ISEQUALTO ans5 [synthesized QUEST2_VAL  OF  S1,
                                              synthesized QUEST1_VAL OF S2,
                                              synthesized SENT_VAL    OF  S3]]  
    <|>
    parser (nt quest5 S1  *> nt quest1 S2  *>  nt sent S3 )
    [rule_s QUEST_VAL  OF LHS ISEQUALTO ans5 [synthesized QUEST2_VAL  OF  S1,
                                              synthesized QUEST1_VAL OF S2,
                                              synthesized SENT_VAL    OF  S3]]  
    <|>
    parser (nt quest2 S1 *> nt joinvbph S2)
    [rule_s QUEST_VAL  OF LHS ISEQUALTO ans2 [synthesized QUEST2_VAL    OF  S1,
                                              synthesized VERBPH_VAL    OF  S2]] 
    <|>
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
 = \atts -> NOUNCLA_VAL (liftM2 intersect_entevimages (getAtts getAVALS atts x) (getAtts getAVALS atts y))

intrsct2         [x, y]         
 = \atts -> ADJ_VAL (liftM2 intersect_entevimages (getAtts getAVALS atts x) (getAtts getAVALS atts y))

applydet         [x, y]                 
 = \atts -> TERMPH_VAL $ (getAtts getDVAL atts x) (getAtts getAVALS atts y)
 
--make_trans_vb is very similar to make_relation.  getBR must mean "get binary relation"
--getTVAL must mean "get predicate" i.e. what would be "phobos" in "discover phobos"
--Changed to get rid of make_trans_vb, since the getBR attribute was changed to not 
--be a binary relation but instead a function that make_relation would give
--I.e., the kind of function that make_trans_vb would have generated, since they were
--nearly identical

--NEW FOR PREPOSITIONAL PHRASES
applytransvbprep [x,y,z] atts = VERBPH_VAL $ make_filtered_relation dataStore reln ((["object"],predicate):preps)
    where 
    reln = getAtts getBR atts x
    predicate = getAtts getTVAL atts y
    preps = getAtts getPREPVAL atts z
    
applytransvbprep [x,y] atts = VERBPH_VAL $ make_filtered_relation dataStore reln [(["object"],predicate)]
    where
    reln = getAtts getBR atts x
    predicate = getAtts getTVAL atts y

applytransvb_no_tmph [x,y] atts = VERBPH_VAL $ make_filtered_relation dataStore reln preps
    where
    reln = getAtts getBR atts x
    preps = getAtts getPREPVAL atts y

applytransvb_no_tmph [x] atts = VERBPH_VAL $ make_filtered_relation dataStore reln []
    where
    reln = getAtts getBR atts x

--TODO: modify grammar so you can't ask "what was phobos discover", or if you can, make the answer sensible (e.g. hall, not phobos)
apply_quest_transvb_passive (x2:x3:x4:xs) atts = VERBPH_VAL $ termph $ make_inverted_filtered_relation dataStore reln preps
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
        (prep_names, termph)
        
applyprep   [x]
 = \atts -> PREP_VAL $ [(getAtts getPREPPHVAL atts x)] --[(["with_implement"], a telescope)]
 
applypreps      [x, y]
 = \atts -> PREP_VAL $ (getAtts getPREPPHVAL atts x):(getAtts getPREPVAL atts y)
 
applyyear [x]
 = \atts -> TERMPH_VAL $ make_pnoun $ show $ getAtts getYEARVAL atts x
 
--END PREPOSITIONAL PHRASES
        
applyvbph        [z]    
 = \atts -> VERBPH_VAL (getAtts getAVALS atts z)
 
appjoin1         [x, y, z]     
 = \atts -> TERMPH_VAL $ (getAtts getTJVAL atts y) (getAtts getTVAL atts x) (getAtts getTVAL atts z)

appjoin2         [x, y, z]    
 = \atts -> VERBPH_VAL ((getAtts getVJVAL atts y) (getAtts getAVALS atts x) (getAtts getAVALS atts z))

apply_middle1    [x, y, z]    
 = \atts -> NOUNCLA_VAL ((getAtts getRELVAL atts y) (getAtts getAVALS atts x) (getAtts getAVALS atts z))

apply_middle2    [x, y, z]                  
 = \atts -> NOUNCLA_VAL ((getAtts getNJVAL atts y) (getAtts getAVALS atts x) (getAtts getAVALS atts z))

apply_middle3    [x, y, z]    
 = \atts -> NOUNCLA_VAL ((getAtts getRELVAL atts y) (getAtts getAVALS atts x) (getAtts getAVALS atts z))

-- Think "a orbited by b" vs "b orbits a"
{-drop3rd          [w, x, y, z]     
 = \atts -> VERBPH_VAL $
        let reln = getAtts getBR atts x in
        let predicate = getAtts getTVAL atts z in
        make_inverted_relation dataStore reln predicate-}

--NEW FOR PREPOSITIONAL PHRASES
drop3rdprep (w:x:xs) atts = VERBPH_VAL $ make_inverted_filtered_relation dataStore reln preps
        where
        reln = getAtts getBR atts x
        preps = case xs of 
            [] -> []
            (p:ps) -> getAtts getPREPVAL atts p
--END PREPOSITIONAL PHRASES
        
apply_termphrase [x, y]     
 = \atts -> SENT_VAL ((getAtts getTVAL atts x) (getAtts getAVALS atts y))
 
sent_val_comp    [s1, f, s2]      
 = \atts -> SENT_VAL ((getAtts getSJVAL atts f) (getAtts getSV atts s1) (getAtts getSV atts s2))

ans1             [x, y]       
 = \atts -> QUEST_VAL ((getAtts getQU1VAL atts x) (getAtts getSV atts y) )

ans2             [x, y]     
 = \atts -> QUEST_VAL ((getAtts getQU2VAL atts x) (getAtts getAVALS atts y))

ans3             [x, y, z]     
 = \atts -> QUEST_VAL ((getAtts getQU3VAL atts x) (getAtts getAVALS atts y) (getAtts getAVALS atts z))

ans5             [x, y, z]     
 = \atts -> QUEST_VAL ((getAtts getQU2VAL atts x) (getAtts getSV atts z))
 
truefalse        [x]       
 = \atts -> QUEST_VAL $ do
        bool <- (getAtts getSV atts x)
        return $ if bool /= [] then "true." else "false."
        


        
{-
||-----------------------------------------------------------------------------
|| THE SEMANTICS - PART II : Functions used to obtain objects denoted by 
||   proper nouns, verbs, etc.
||-----------------------------------------------------------------------------

|| FUNCTION USED TO DEFINE OBJECTS ASSOCIATED WITH PROPER NOUNS
-}
--test_wrt e s = e `elem` s

-- FUNCTION USED TO DEFINE MEANINGS OF VERBS IN TERMS OF RELATIONS
--make_trans_vb rel p = [x | (x, image_x) <- collect rel, p image_x] -- Similar to make_relation

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

<Quest6> = what;

<Intransvb> = exist | exists | spin | spins;

-}

dictionary = [
    ("thing",              Cnoun,     [NOUNCLA_VAL $ get_members dataStore "thing"]),
    ("things",             Cnoun,     [NOUNCLA_VAL $ get_members dataStore "thing"]),
    ("planets",            Cnoun,     [NOUNCLA_VAL $ get_members dataStore "planet"]),
    ("planet",             Cnoun,     [NOUNCLA_VAL $ get_members dataStore "planet"]),
    ("person",             Cnoun,     [NOUNCLA_VAL $ get_members dataStore "person"]),   
    ("sun",                Cnoun,     [NOUNCLA_VAL $ get_members dataStore "sun"]), 
    ("moon",               Cnoun,     [NOUNCLA_VAL $ get_members dataStore "moon"]), 
    ("moons",              Cnoun,     [NOUNCLA_VAL $ get_members dataStore "moon"]),
    ("satellite",          Cnoun,     [NOUNCLA_VAL $ get_members dataStore "moon"]),
    ("satellites",         Cnoun,     [NOUNCLA_VAL $ get_members dataStore "moon"]),
    ("atmospheric",        Adj,       [ADJ_VAL $ get_members dataStore "atmospheric"]),
    ("blue",               Adj,       [ADJ_VAL $ get_members dataStore "blue"]),
    ("solid",              Adj,       [ADJ_VAL $ get_members dataStore "solid"]),     
    ("brown",              Adj,       [ADJ_VAL $ get_members dataStore "brown"]),   
    ("gaseous",            Adj,       [ADJ_VAL $ get_members dataStore "gaseous"]),  
    ("green",              Adj,       [ADJ_VAL $ get_members dataStore "green"]),  
    ("red",                Adj,       [ADJ_VAL $ get_members dataStore "red"]),  
    ("ringed",             Adj,       [ADJ_VAL $ get_members dataStore "ringed"]),  
    ("vacuumous",          Adj,       [ADJ_VAL $ get_members dataStore "vacuumous"]),
    ("exist",              Intransvb, [VERBPH_VAL $ get_members dataStore "thing"]),
    ("exists",             Intransvb, [VERBPH_VAL $ get_members dataStore "thing"]),
    ("spin",               Intransvb, [VERBPH_VAL $ get_members dataStore "spin"]),
    ("spins",              Intransvb, [VERBPH_VAL $ get_members dataStore "spin"]),
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
    ("discover",           Transvb,   [VERB_VAL ("discover_ev")]),
    ("discovers",          Transvb,   [VERB_VAL ("discover_ev")]),
    ("discovered",         Transvb,   [VERB_VAL ("discover_ev")]),
    ("orbit",              Transvb,   [VERB_VAL ("orbit_ev")]),
    ("orbited",            Transvb,   [VERB_VAL ("orbit_ev")]),
    ("orbits",             Transvb,   [VERB_VAL ("orbit_ev")]),
    ("is",                 Linkingvb, [LINKINGVB_VAL  id]),
    ("was",                Linkingvb, [LINKINGVB_VAL  id]), 
    ("are",                Linkingvb, [LINKINGVB_VAL  id]),
    ("were",               Linkingvb, [LINKINGVB_VAL  id]),
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
    ("is",                 Quest1,    [QUEST1_VAL     $ yesno]),
    ("was",                Quest1,    [QUEST1_VAL     $ yesno]),
    ("are",                Quest1,    [QUEST1_VAL     $ yesno]),  
    ("were",               Quest1,    [QUEST1_VAL     $ yesno]),  
    ("what",               Quest2,    [QUEST2_VAL     $ what]),
    ("who",                Quest2,    [QUEST2_VAL     $ who]),
    ("what",               Quest6,    [QUEST2_VAL     $ whatobj]),
    ("where",              Quest5,    [QUEST2_VAL     $ where']),
    ("when",               Quest5,    [QUEST2_VAL     $ when']),
    ("how",                Quest5,    [QUEST2_VAL     $ how']),
    ("which",              Quest3,    [QUEST3_VAL     which]),
    ("what",               Quest3,    [QUEST3_VAL     which]),
    ("how",                Quest4a,   [QUEST3_VAL     $ how_many]),
    ("many",               Quest4b,   [QUEST3_VAL     $ how_many]),
    ("human",       Cnoun,    meaning_of nouncla "person" Nouncla),
    ("discoverer",  Cnoun,            [NOUNCLA_VAL $ get_subjs_of_event_type dataStore "discover_ev"]),
    ("discoverers", Cnoun,            [NOUNCLA_VAL $ get_subjs_of_event_type dataStore "discover_ev"]), 
    ("humans",      Cnoun,    meaning_of nouncla "person" Nouncla), 
    ("people",      Cnoun,    meaning_of nouncla "person" Nouncla),
    --("orbit",       Intransvb,        [VERBPH_VAL $ get_subjs_of_event_type dataStore "orbit_ev"]), 
    --("orbits",      Intransvb,        [VERBPH_VAL $ get_subjs_of_event_type dataStore "orbit_ev"]),
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
    ("in",          Prepn, [PREPN_VAL ["location","year"]]),
    ("at",          Prepn, [PREPN_VAL ["location"]]),
    ("by",          Prepn, [PREPN_VAL ["subject"]]),
    --Begin telescope stuff--
    ("telescope",   Cnoun, [NOUNCLA_VAL $ get_members dataStore "telescope"]),
    ("telescopes",  Cnoun, [NOUNCLA_VAL $ get_members dataStore "telescope"]),
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
    ("team",                            Cnoun, [NOUNCLA_VAL $ get_members dataStore "science_team"]),
    ("teams",                           Cnoun, [NOUNCLA_VAL $ get_members dataStore "science_team"]),
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
    ("spacecraft",  Cnoun, [NOUNCLA_VAL $ get_members dataStore "spacecraft"]),
    ("spacecrafts", Cnoun, [NOUNCLA_VAL $ get_members dataStore "spacecraft"]),
    ("voyager_1",   Pnoun, [TERMPH_VAL $ make_pnoun "voyager_1"]),
    ("voyager_2",   Pnoun, [TERMPH_VAL $ make_pnoun "voyager_2"]),
    ("cassini", Pnoun, [TERMPH_VAL $ make_pnoun "voyager_2"]),
    --Begin new places stuff--
    ("place",   Cnoun, [NOUNCLA_VAL $ get_members dataStore "place"]),
    ("places",  Cnoun, [NOUNCLA_VAL $ get_members dataStore "place"]),
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

list_of_years = map (\n -> (show n, Year, [YEAR_VAL n])) $ concat [[1000 + x, 2000 + x] | x <- [0..999]]
    
--test1 p p_ inp = do putStr  $ render80 $ format{-Atts p_-} $ snd $ unState (p T0 [] ((1,[]),words inp) ([],[])) [] 
test p input = unState (p ((1,[]),input) ([],[])) [] 



parse i = formatAttsFinalAlt Question  ((length (words i))+1) $ snd $ test (question T0 []) (words i)
    
findStart st ((s,ss):rest) | s == st   = [(s,ss)]
                           | otherwise = findStart st rest
findStart st []                        = []     

input = words i1

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
