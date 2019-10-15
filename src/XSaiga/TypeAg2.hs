{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module XSaiga.TypeAg2 where

import XSaiga.Getts
import Data.Text as T hiding (map)
import XSaiga.ShowText
import Control.Arrow
import Data.Bifunctor
import Data.Biapplicative
import Data.List (nub)
import qualified Data.Map.Strict as Map
import Control.Monad.State.Strict
--import Control.Applicative

data GettsIntersectType = GI_NounAnd | GI_Most | GI_Every | GI_Which | GI_HowMany | GI_Number Int deriving (Eq, Show, Ord)
data GettsUnionType = GU_NounOr | GU_NounAnd deriving (Eq, Show, Ord)

--idea: the tree should be fully applied whenever it is stored
--If made fine grained enough, this may suffice for memoization
type TF a = [Triple] -> a

--newtype TFMemo a = TFMemo (TF (State (Map.Map GettsTree FDBR) a), Maybe GettsTree)

--A SemFunc is really just a tuple of two functions
type SemFunc a = (a, Treeify a)

--convenience functions for TFMemo
{-
getGetts (TFMemo a) = snd a
getSem (TFMemo a) = fst a

instance Functor TFMemo where
    fmap f m = TFMemo (\triples -> fmap f (getSem m triples), Nothing)

instance Applicative TFMemo where
    --(<*>) :: TFMemo (a -> b) -> TFMemo a -> TFMemo b
    f <*> m = TFMemo (f', Nothing)
        where
            f' triples = (getSem f triples) <*> (getSem m triples)
    --f <*> m = f >>= (\f' -> m >>= (\a -> return $ f' a))
    pure a = TFMemo (const $ return a, Nothing)

instance Monad TFMemo where
    m >>= f = TFMemo (f', Nothing)
        where
            f' triples = getSem m triples >>= (\a -> getSem (f a) triples)
    return = pure
-}

--OBSERVATIONS:
{-
 * we didn't even use liftM anywhere for TFMemoT
 * we didn't use TFMemoT as a monad anywhere even
 * we actually lost performance when not memoizing GettsPropFDBR (1.62 vs 1.22 observed on highly ambiguous query)
 * we improved performance slightly using Strict map and Strict state
 * we improved performance slightly by re-using the state between ambiguous parses
 * so, do we need to overcomplicate with Nothing?

 * significant slowdown in ambiguous queries from fact that SPARQL queries are not cached between parses
 * it may be ideal to collect ALL parses at once and then make a reduced triplestore from that
 * above IMPLEMENTED to GREAT effect!
 * and also fix our representation of how reduced triplestores are stored to alleviate the slowdown
 
  NEED TO TEST:
  * liftS vs wrapV -- can we just get SemFunc to do our work for us

-}

newtype TFMemoT t g m a = TFMemoT (t -> m a, Maybe g)
getGetts (TFMemoT a) = snd a
getSem (TFMemoT a) = fst a

instance (Functor m) => Functor (TFMemoT t g m) where
    fmap f m = TFMemoT (\triples -> fmap f (getSem m triples), Nothing)

instance (Applicative m) => Applicative (TFMemoT t g m) where
    f <*> m = TFMemoT (f', Nothing)
        where
            f' triples = (getSem f triples) <*> (getSem m triples)
    --f <*> m = f >>= (\f' -> m >>= (\a -> return $ f' a))
    pure a = TFMemoT (const $ pure a, Nothing)

instance (Monad m) => Monad (TFMemoT t g m) where
    m >>= f = TFMemoT (f', Nothing)
        where
            f' triples = getSem m triples >>= (\a -> getSem (f a) triples)
    return = pure

type TFMemo = TFMemoT [Triple] GettsTree (State (Map.Map GettsTree FDBR))

--moon >>= (\moon_fdbr -> spins >>= (\spin_fdbr -> a'' moon_fdbr spins_fdbr))

--the idea: bind erases the name but keeps the memoized computation
--so liftM* works as expected
--liftS* can be used to keep names for things
--does this uphold the monad laws?

data GettsTree =
      GettsNone -- TODO MEMO: NOT to be used for memoization (replace with Maybe GettsTree in termphrases/superphrases?)
    | GettsPNoun Text
    | GettsMembers Text
    | GettsTP Text Relation [GettsTree]
    | GettsPrep [Text] (Maybe Ordering) GettsTree --TODO memoized superlatives
    | GettsIntersect GettsIntersectType GettsTree GettsTree --representing result of intersect_fdbr (only evs from 2nd tree are kept)
    | GettsUnion GettsUnionType GettsTree GettsTree --representing result of "union" of fdbrs.  Keeps everything.
    | GettsYesNo GettsTree
    | GettsPropTmph Text GettsTree
    | GettsAttachP Text GettsTree
    | GettsPropFDBR [Text] [Text] --props and events, used for memoizing filter_evs
    deriving (Eq, Show, Ord)

data GettsFlatTypes =
    FGettsMembers Text
  | FGettsT [Text] Text

type FGettsMembers = Text
type FGettsT = ([Text], Text)

type GettsFlat = ([FGettsMembers], [FGettsT])

--Convert a function signature to a Getts style function
--Keeps same kind, same arity
type family Treeify x where
  --Treeify ((TF a -> b) -> c) = Treeify (TF a -> b) -> Treeify c
  --Treeify (TF a -> b) = GettsTree -> Treeify b
  Treeify (TF a) = GettsTree
  Treeify (a -> b) = Treeify a -> Treeify b
  Treeify a = GettsTree
  --Treeify _ = GettsTree<Paste>

--TODO: Abstract tree into separate datastructure as Foldable and Traversable
--It seems to me there are five layers:
{-

User layer: GettsT, GettsPrep, GettsAttachP ... these are like semantic "words"
Unparsed Tree layer: GettsSubQuery, GettsList, GettsNode ... this is like the parse tree of those "words"
Semantic (condensed) layer: Group SubQuery, List, Node, etc... into GettsTP, keeping tree
Flattened layer: Only a list of GettsMembers and a list of GettsTP (flattened tree)
Optimization layer: Knowledge to reduce queries (subsets, GettsTP of same type)

Semantic tree layer
Flattened layer

-}

--wrap memoizes and names
wrap :: (TF FDBR, GettsTree) -> TFMemo FDBR
wrap (f,g) = TFMemoT (f', Just g)
    where
        f' triples = do
            s <- get
            case Map.lookup g s of
                Just fdbr -> return fdbr
                Nothing -> do
                    let res = f triples
                    modify (\s' -> Map.insert g res s')
                    return res

--ALTERNATIVE APPROACH: use existing SemFunc infrastructure and const to produce memoized definitions
--wrapV* functions do not memoize results but do provide names
--provides more uniformity!  could leverage all existing definitions and automatically have them work
--but does const imply a performance penalty?
wrapU0 :: TF a -> TFMemo a
wrapU0 f = TFMemoT (f', Nothing)
                    where
                        f' triples = return $ f triples

wrapV0 :: (TF a, GettsTree) -> TFMemo a
wrapV0 (f,g) = TFMemoT (f', Just g)
                    where
                        f' triples = return $ f triples

wrapV1 :: (TF a -> TF b, GettsTree -> GettsTree) -> TFMemo a -> TFMemo b
wrapV1 (f,g) a = TFMemoT (f', g')
                    where
                        g' = liftM g (getGetts a)
                        f' triples = (getSem a triples) >>= (\x -> return $ f (const x) triples)

wrapV2 :: (TF a -> TF b -> TF c, GettsTree -> GettsTree -> GettsTree) -> TFMemo a -> TFMemo b -> TFMemo c
wrapV2 (f,g) a b = TFMemoT (f', g')
                    where
                        g' = liftM2 g (getGetts a) (getGetts b)
                        f' triples = do
                            x1 <- (getSem a triples)
                            x2 <- (getSem b triples)
                            return $ f (const x1) (const x2) triples

--versions of wrapV* could be provided that do memoize results, PROVIDED they are FDBRs

--liftT* is like liftM, but it still maintains a unique name even though it does not memoize anything
liftT :: (a -> b) -> (GettsTree -> GettsTree) -> TFMemo a -> TFMemo b
liftT f g a = TFMemoT (f', g')
                where
                    g' = liftM g (getGetts a)
                    f' triples = liftM f $ getSem a triples

liftT2 :: (a -> b -> c) -> (GettsTree -> GettsTree -> GettsTree) -> TFMemo a -> TFMemo b -> TFMemo c
liftT2 f g a b = TFMemoT (f', g')
                where
                    g' = liftM2 g (getGetts a) (getGetts b)
                    f' triples = liftM2 f (getSem a triples) (getSem b triples)

liftS :: (FDBR -> FDBR) -> (GettsTree -> GettsTree) -> TFMemo FDBR -> TFMemo FDBR
liftS f g a = TFMemoT (f', g')
                where
                    g' = liftM g (getGetts a)
                    f' triples = case g' of
                        Nothing -> liftM f $ getSem a triples
                        Just name -> do
                            s <- get
                            case Map.lookup name s of
                                Just fdbr -> return fdbr
                                Nothing -> do
                                    res <- liftM f $ getSem a triples
                                    modify (\s' -> Map.insert name res s')
                                    return res

liftS2 :: (FDBR -> FDBR -> FDBR) -> (GettsTree -> GettsTree -> GettsTree) -> TFMemo FDBR -> TFMemo FDBR -> TFMemo FDBR
liftS2 f g a1 a2 = TFMemoT (f', g')
                where
                    g' = liftM2 g (getGetts a1) (getGetts a2)
                    f' triples = case g' of
                        Nothing -> liftM2 f (getSem a1 triples) (getSem a2 triples)
                        Just name -> do
                            s <- get
                            case Map.lookup name s of
                                Just fdbr -> return fdbr
                                Nothing -> do
                                    res <- liftM2 f (getSem a1 triples) (getSem a2 triples)
                                    modify (\s' -> Map.insert name res s')
                                    return res

--NEW: convert from biapplicative form to regular function application
--this is a one way conversion, unless you have the property that the tuple elements are "invariant" with respect to each other

--A type-level function to "tie" two functions together into a series of functions each taking a pair argument
type family BiappFunc x y where
    BiappFunc (a1 -> a2) (b1 -> b2) = (a1, b1) -> BiappFunc a2 b2
    BiappFunc x y = (x, y)

--Implementation of the "tie" function above
class BiappFuncImpl x y where
    tieFunc :: x -> y -> BiappFunc x y

instance {-# OVERLAPPING #-} (BiappFuncImpl a2 b2) => BiappFuncImpl (a1 -> a2) (b1 -> b2) where
    tieFunc f g = (\(a, b) -> tieFunc (f a) (g b))

instance (BiappFunc a b ~ (a, b)) => BiappFuncImpl a b where
    tieFunc a b = (a, b)

--Implementation to convert `SemFunc a` to function application form
fromBiapp (a, b) = tieFunc a b
--toBiapp :: (a, b) -> (a2, b2) -> ... -> (a -> a2 -> ..., b -> b2 ->...)
--TODO: make more general -- support generalized biapplicative?
--fromBiapp :: Biapplicative p (a1 -> a2) (b1 -> b2) -> p a1 b1 -> p a2 b2
--fromBiapp b x = (b <<*>> x) <<*> ...
--TODO: support higher order functions as well using `extract` function

--So, you get the original behaviour by doing convBiapp $ termor, convBiapp $ a, etc...
--This allows biapplicative form to be used for easy extraction, and function application form for convenience
--It would be nice to get rid of the overlapping instances though!

--TODO: restrict arity of b (same kind as a)
semFunc :: a -> Treeify a -> SemFunc a
semFunc = bipure

--Just for presentation purposes
instance Show (TF FDBR) where
  show x = "TF FDBR"

instance (Show a) => Show (TF (State (Map.Map GettsTree FDBR) a)) where
  show x = "TF State"

instance Show (TF GFDBR) where
  show x = "TF GFDBR"

instance Show (TF Text) where
  show x = "TF Text"



--Unclean -- Foldable would help
--TODO: filter duplicates
--TODO: make [SemFunc a] not used with gatherPreps, apply directly to verb?
--Would avoid GettsPreps situation

--apply over the intersect_entevimages, remember evs of im2 are preserved and im1 are discarded
gettsAttachP :: T.Text -> GettsTree -> GettsTree
gettsAttachP prop (GettsIntersect t x y) = GettsIntersect t x (gettsAttachP prop y)
gettsAttachP prop (GettsTP p rel sub) = GettsTP p rel ((GettsPrep [prop] Nothing GettsNone):sub) --is this correct?  should i go back to the old way?  does cardinality have to match?
--TODO MEMO -- altering tree may affect identity for memoization
--IDEA!!  create a GettsAttachP node instead and only during flattening will it have any effect -- prevents altering trees

flattenGetts :: GettsTree -> GettsFlat
flattenGetts GettsNone = ([],[])
flattenGetts (GettsPNoun _) = ([],[])
flattenGetts (GettsMembers set) = ([set], [])
flattenGetts (GettsYesNo fdbr) = flattenGetts fdbr
flattenGetts (GettsIntersect t i1 i2) = merge (flattenGetts i1) (flattenGetts i2)
flattenGetts (GettsUnion t i1 i2) = merge (flattenGetts i1) (flattenGetts i2)

flattenGetts (GettsPrep props _ sub) = error "Cannot flatten prep directly" --TODO
flattenGetts (GettsPropTmph t sub) = flattenGetts sub --TODO MEMO!

flattenGetts (GettsAttachP prop sub) = flattenGetts $ gettsAttachP prop sub

flattenGetts (GettsTP prop (rel, _, _) preps) = let props = nub $ prop:(Prelude.concatMap (\(GettsPrep p _ _) -> p) preps)
        in Prelude.foldr merge ([],[(props, rel)]) $ map (\(GettsPrep _ _ sub) -> flattenGetts sub) preps

merge :: GettsFlat -> GettsFlat -> GettsFlat
merge (x1, y1) (x2, y2) = (x1 ++ x2, y1 ++ y2)

(>|<) :: a -> Treeify a -> SemFunc a
f >|< g = bipure f g

flatOptimize :: GettsFlat -> GettsFlat
flatOptimize (members, gettsTPs) = (nub members, dedupTP)
  where
    dedupTP = Prelude.foldr (\(props, rel) -> \optlist -> if Prelude.any (\(_, r) -> r == rel) optlist
                                                             then map (\(p, r) -> if r == rel then (nub $ props ++ p, rel) else (p, r)) optlist
                                                             else (props,rel):optlist) [] gettsTPs

getReducedTriplestore :: (TripleStore m) => m -> GettsFlat -> IO [Triple]
getReducedTriplestore ev_data (sets, trans) = do
  t1 <- mapM (\set -> getts_triples_members ev_data set) sets
  t2 <- mapM (\(props, rel) -> getts_triples_entevprop_type ev_data props rel) trans
  return $ Prelude.concat $ t1 ++ t2

unionerr x y = error $ "attempt to union: " ++ show x ++ " with " ++ show y --TODO: debugging

--TODO OLD
{-gettsIdentity f GettsNone y = y
gettsIdentity f x GettsNone = x
gettsIdentity f x y = f x y

gettsIntersect = gettsIdentity GettsIntersect

--TODO: make this a proper union
gettsUnion = gettsIdentity GettsUnion
-}

gettsApply g = g GettsNone

--NOTE MEMO: only use gettsApply here for unique naming, not for memoizing individual semantic functions
--NOTE MEMO: what do we do if we have unmemoized stuff?
--NOTE MEMO: assume all top level things are named, because otherwise we can't query the database
--cannot use sequenceA because it would look like
--with (a (telescope (by (a (person))))) instead of (with ( a telescope )), (by (a person))
--TODO: augment with superlative
gatherPreps' :: [([T.Text], Maybe Ordering, SemFunc (TF a -> TF b))] -> [GettsTree]
gatherPreps' = map (\(props, ord, g) -> GettsPrep props ord $ gettsApply $ snd g)

gatherPreps :: [([T.Text], Maybe Ordering, TFMemo a -> TFMemo b)] -> [GettsTree]
gatherPreps = map (\(props, ord, g) -> GettsPrep props ord $ applyGettsPrep g) --test with undefined to prove

--used ONLY to get a unique identifier for prepositions
applyGettsPrep :: (TFMemo a -> TFMemo b) -> GettsTree
applyGettsPrep g = case getGetts $ g (TFMemoT (error "Should not eval!", Just GettsNone)) of
                    Just x -> x
                    Nothing -> error "Top level semantic functions need GettsTree names"

--OBSERVATION: top level things need names for sure.  the only time we don't want to name things is sometimes inside other computations.
--a top level expression without a name will cause the entire expression to not have a name
--but that also means we can't retrieve anything from the triplestore because we will have Nothing to query it with
--it would be nice to garrant that top level things will always have a name, and provide an escape hatch for those inside computations that don't contribute to the name of other things
--in practice, it's only inside transitive verbs we need more control over this
--in particular, the predicates in here need that control
--if we pass Nothing in, we forbid them from memoizing
--since they may appear at the top level, the same denotation must be used inside and outside
--therefore, that denotation needs to account for this
--if we make GettsIntersect have a Maybe GettsTree as the last argument, we can remove GettsNone
--but this complicates liftS* a bit: (FDBR -> FDBR -> FDBR) -> (GettsTree -> Maybe GettsTree -> Maybe GettsTree) -> ...?

--a <<*>> moon <<*>> spins... 
--PUBLIC INTERFACE (TODO)
--TODO: make_fdbr_with_prop is DANGEROUS
--Get members of named set
--get_members :: (TripleStore m) => m -> Text -> IO FDBR
--get_members = getts_members
get_members' :: Text -> SemFunc (TF FDBR)
get_members' set = (\r -> pure_getts_members r set) >|< GettsMembers set

get_members = wrap . get_members'

--TODO: revise this for new transitive verb definition.  should not assume these fields in general.
get_subjs_of_event_type' :: Text -> SemFunc (TF FDBR)
get_subjs_of_event_type' ev_type = (\r -> make_fdbr_with_prop (pure_getts_triples_entevprop_type r ["subject"] ev_type) "subject") >|< GettsTP "subject" (ev_type, "subject", "object") []

get_subjs_of_event_type = wrap . get_subjs_of_event_type'

data AttValue = VAL             {getAVAL    ::   Int}
              | MaxVal          {getAVAL    ::   Int}
              | SubVal          {getAVAL    ::   Int}
              | RepVal          {getAVAL    ::   Int}
              | Res             {getRVAL    ::   DisplayTree}
              | B_OP            {getB_OP    ::   (Int -> Int -> Int)}
              | U_OP            {getU_OP    ::   (Int -> Int)}
              | SENT_VAL        {getSV      ::   TFMemo FDBR }
              | ErrorVal        {getEVAL    ::   Text}
              | NOUNCLA_VAL     {getAVALS   ::   TFMemo FDBR }
              | VERBPH_VAL      {getAVALS   ::   TFMemo FDBR }
              | ADJ_VAL         {getAVALS   ::   TFMemo FDBR }
              | TERMPH_VAL      {getTVAL    ::   TFMemo FDBR -> TFMemo FDBR}
              | DET_VAL         {getDVAL    ::   TFMemo FDBR -> TFMemo FDBR -> TFMemo FDBR}
              | VERB_VAL        {getBR      ::   Relation } --stores relation, "subject" and "object". TODO: need to expand on later for "used"      
          | RELPRON_VAL     {getRELVAL  ::   TFMemo FDBR -> TFMemo FDBR -> TFMemo FDBR}
          | NOUNJOIN_VAL    {getNJVAL   ::   TFMemo FDBR -> TFMemo FDBR -> TFMemo FDBR}
          | VBPHJOIN_VAL    {getVJVAL   ::   TFMemo FDBR -> TFMemo FDBR -> TFMemo FDBR}
          | TERMPHJOIN_VAL  {getTJVAL   ::   (TFMemo FDBR -> TFMemo FDBR) -> (TFMemo FDBR -> TFMemo FDBR) -> TFMemo FDBR -> TFMemo FDBR }
          | SUPERPHSTART_VAL  {getSUPERPHSTARTVAL :: ()}
          | SUPER_VAL       {getSUPERVAL ::  Ordering}
          | SUPERPH_VAL     {getSUPERPHVAL :: (Ordering, TFMemo FDBR -> TFMemo FDBR)}
          | PREP_VAL       {getPREPVAL ::   [([Text], Maybe Ordering, TFMemo FDBR -> TFMemo FDBR)]} -- used in "hall discovered phobos with a telescope" as "with".  
          | PREPN_VAL       {getPREPNVAL :: [Text]} --used for mapping between prepositions and their corresponding identifiers in the database.  I.e., "in" -> ["location", "year"]
          | PREPNPH_VAL     {getPREPNPHVAL :: [Text]}
          | PREPPH_VAL      {getPREPPHVAL :: ([Text], Maybe Ordering, TFMemo FDBR -> TFMemo FDBR)}
          | LINKINGVB_VAL   {getLINKVAL ::   TFMemo FDBR -> TFMemo FDBR}
          | SENTJOIN_VAL    {getSJVAL   ::   TFMemo FDBR -> TFMemo FDBR -> TFMemo FDBR}
          | DOT_VAL         {getDOTVAL  ::   TFMemo Text}
          | QM_VAL          {getQMVAL   ::   TFMemo Text}
          | QUEST_VAL       {getQUVAL   ::   TFMemo Text}
          | QUEST1_VAL      {getQU1VAL  ::   TFMemo FDBR -> TFMemo Text}
          | QUEST2_VAL      {getQU2VAL  ::   TFMemo FDBR -> TFMemo Text}
          | QUEST6_VAL      {getQU6VAL  ::   TFMemo FDBR -> TFMemo Text}
          | QUEST3_VAL      {getQU3VAL  ::   TFMemo FDBR -> TFMemo FDBR -> TFMemo Text}
          | YEAR_VAL        {getYEARVAL ::   Int}

--            | RESULT [sys_message]
--Also called a "NodeName"
data MemoL    = Start | Tree | Num | Emp | ALeaf Text | Expr | Op  | ET
              | Pnoun|Cnoun|Adj|Det|Intransvb|Transvb|Linkingvb|Relpron|Termphjoin|Verbphjoin|Nounjoin|Preps|Prepph|Super|Superph|SuperphStart|Prepn|Prepnph|Indefpron|Sentjoin|Quest1|Quest2|Quest3|Quest4a|Quest4b
              | Snouncla|Relnouncla|Nouncla|Adjs|Detph|Transvbph|Verbph|Termph|Jointermph|Joinvbph|Sent|Two_sent|Question|Quest4|Query|Year|Quest5|Quest6
                deriving (Eq,Ord,Show)

data Id       = O0|S0 |S1|S2|S3|S4|S5|S6|S7|S8|S9|T0|T1 | T2 | T3 |T4 | N1| N2 | N3 | E0 |E1 |E2 | O1| LHS  deriving (Eq,Ord,Show, Enum)
                -- basic IDs for identifying each NT  

{-

attFunc 
 = [
    (VAL,getAVAL), (MaxVal,getAVAL), (SubVal,getAVAL), (RepVal,getAVAL), (Res,getRVAL), (B_OP,getB_OP),(U_OP,getU_OP),(SENT_VAL,getSV),(ErrorVal,getEVAL),
    (NOUNCLA_VAL,getAVALS),(VERBPH_VAL,getAVALS),(ADJ_VAL,getAVALS),(TERMPH_VAL,getTVAL),(DET_VAL,getDVAL),(VERB_VAL,getBR),(RELPRON_VAL,getRELVAL),(NOUNJOIN_VAL,getNJVAL),
    (VBPHJOIN_VAL,getVJVAL),(TERMPHJOIN_VAL,getTJVAL),(PREP_VAL,getPREPVAL),(LINKINGVB_VAL,getLINKVAL),(SENTJOIN_VAL,getSJVAL),(DOT_VAL,getDOTVAL),(QM_VAL,getQMVAL),(QUEST1_VAL,getQU1VAL),
    (QUEST2_VAL,getQU2VAL),(QUEST3_VAL,getQU3VAL)   
   ]
-}
type Entity         =  Text
--type Bin_Rel        =  [(Entity,Entity)] -- [(Int, Int)]
--type Relation     = (ES -> Bool) -> FDBR
type Relation = (Text, Text, Text)

data DisplayTree = B [DisplayTree]
                 | N Int
                   deriving (Show, Eq)

{-instance Show AttValue where
    show (VAL  j)     = "VAL "    ++ show j
    show (MaxVal j)   = "MaxVal " ++ show j
    show (SubVal j)   = "SubVal " ++ show j
    show (RepVal j)   = "RepVal " ++ show j
    show (Res j)      = "Tree: "  ++ show j
    show (B_OP j)     = "B_OP"
    show (U_OP j)     = "U_OP"
    show (SENT_VAL j) = show j
    show (ErrorVal j) =  j
    show (NOUNCLA_VAL j) = "NOUNCLA_VAL " ++ unwords $ map fst j
    show (VERBPH_VAL j)  = "VERBPH_VAL " ++ unwords $ map fst j
    show (ADJ_VAL    j)  = "ADJ_VLA " ++ unwords $ map fst j
    show (TERMPH_VAL j)  = "TERMPH_VAL "      
    show (DET_VAL j)     = "DET_VAL " 
    show (VERB_VAL j)    =  "VERB_VAL " ++ j
    show (RELPRON_VAL j)  = "RELPRON_VAL "
    show (NOUNJOIN_VAL j)  = "NOUNJOIN_VAL "
    show (VBPHJOIN_VAL j)  = "VBPHJOIN_VAL "
    show (TERMPHJOIN_VAL j)  = "TERMPHJOIN_VAL "
    show (PREP_VAL j)  = "PREP_VAL "
    show (LINKINGVB_VAL j)  = "LINKINGVB_VAL "
    show (SENTJOIN_VAL j)  = "SENTJOIN_VAL "
    show (DOT_VAL j) = j 
    show (QM_VAL j) = j 
    show (QUEST_VAL j) = j 
    show (QUEST1_VAL j)  = "QUEST1_VAL"
    show (QUEST2_VAL j)  = "QUEST2_VAL"
    show (QUEST3_VAL j)  = "SENTJOIN_VAL"-}

instance Eq AttValue where
    (VAL  j)           == (VAL  j')     = True
    (MaxVal j)         == (MaxVal j')   = True
    (SubVal j)         == (SubVal j')   = True
    (RepVal j)         == (RepVal j')   = True
    (Res j)            == (Res j')      = True
    (B_OP j)           == (B_OP j')     = True
    (U_OP j)           == (U_OP j')     = True
    (SENT_VAL j)       == (SENT_VAL j') = True
    (NOUNCLA_VAL j)    == (NOUNCLA_VAL j') = True
    (VERBPH_VAL j)     == (VERBPH_VAL j') = True
    (ADJ_VAL j)        == (ADJ_VAL j') = True
    (TERMPH_VAL j)     == (TERMPH_VAL j') = True
    (DET_VAL j)        == (DET_VAL j') = True
    (VERB_VAL j)       == (VERB_VAL j') = True
    (RELPRON_VAL j)    == (RELPRON_VAL j') = True
    (NOUNJOIN_VAL j)   == (NOUNJOIN_VAL j') = True
    (VBPHJOIN_VAL j)   == (VBPHJOIN_VAL j') = True
    (TERMPHJOIN_VAL j) == (TERMPHJOIN_VAL j') = True
    (LINKINGVB_VAL j)  == (LINKINGVB_VAL j') = True
    (SENTJOIN_VAL j)   == (SENTJOIN_VAL j') = True
    (DOT_VAL j)        == (DOT_VAL j') = True
    (QM_VAL j)         == (QM_VAL j') = True
    (QUEST_VAL j)      == (QUEST_VAL j') = True
    (QUEST1_VAL j)     == (QUEST1_VAL j') = True
    (QUEST2_VAL j)     == (QUEST2_VAL j') = True
    (QUEST3_VAL j)     == (QUEST3_VAL j') = True
    (QUEST6_VAL j)     == (QUEST6_VAL j') = True
    (PREP_VAL s1)      == (PREP_VAL s) = True
    (PREPN_VAL s1)     == (PREPN_VAL s) = True
    (PREPNPH_VAL s1)   == (PREPNPH_VAL s) = True
    (PREPPH_VAL s1)    == (PREPPH_VAL s) = True
    (YEAR_VAL s1)      == (YEAR_VAL s) = True
    (SUPERPHSTART_VAL s1) == (SUPERPHSTART_VAL s) = True
    (SUPER_VAL s1)     == (SUPER_VAL s) = True
    (SUPERPH_VAL s1)   == (SUPERPH_VAL s) = True
    _                  == _              = False






--------- *********************** --------------
-- needs to be simplified --
setAtt (MaxVal s1)   (MaxVal s)       = [MaxVal s]
setAtt (MaxVal s1)   (SubVal s)       = [MaxVal s]
setAtt (MaxVal s1)   (RepVal s)       = [MaxVal s]
setAtt (MaxVal s1)   (VAL s)          = [MaxVal s]
setAtt (MaxVal s1)   (ErrorVal s)     = [ErrorVal s]

setAtt (SubVal s1)   (MaxVal s)       = [SubVal s]
setAtt (SubVal s1)   (SubVal s)       = [SubVal s]
setAtt (SubVal s1)   (RepVal s)       = [SubVal s]
setAtt (SubVal s1)   (VAL s)          = [SubVal s]
setAtt (SubVal s1)   (ErrorVal s)     = [ErrorVal s]

setAtt (RepVal s1)   (MaxVal s)       = [RepVal s]
setAtt (RepVal s1)   (SubVal s)       = [RepVal s]
setAtt (RepVal s1)   (RepVal s)       = [RepVal s]
setAtt (RepVal s1)   (VAL s)          = [RepVal s]
setAtt (RepVal s1)   (ErrorVal s)     = [ErrorVal s]

setAtt (VAL s1)   (VAL s)          = [VAL s]
setAtt (VAL s1)   (MaxVal s)       = [VAL s]
setAtt (VAL s1)   (SubVal s)       = [VAL s]
setAtt (VAL s1)   (RepVal s)       = [VAL s]
setAtt (VAL s1)   (ErrorVal s)     = [ErrorVal s]


setAtt (Res s1)      (Res s)        = [Res s]
setAtt (Res s1)      (ErrorVal s)   = [ErrorVal s]
setAtt (ErrorVal s1) (ErrorVal s)   = [ErrorVal s]

setAtt (NOUNCLA_VAL s1)  (NOUNCLA_VAL s)    = [NOUNCLA_VAL s]
setAtt (ADJ_VAL s1)      (ADJ_VAL s)        = [ADJ_VAL s]
setAtt (TERMPH_VAL s1)   (TERMPH_VAL s)     = [TERMPH_VAL s]
setAtt (VERBPH_VAL s1)   (VERBPH_VAL s)     = [VERBPH_VAL s]
setAtt (SENT_VAL   s1)   (SENT_VAL   s)     = [SENT_VAL s]
setAtt (QUEST_VAL  s1)   (QUEST_VAL  s)     = [QUEST_VAL s]
setAtt (QUEST1_VAL  s1)   (QUEST1_VAL  s)     = [QUEST1_VAL s]
setAtt (QUEST2_VAL  s1)   (QUEST2_VAL  s)     = [QUEST2_VAL s]
setAtt (QUEST3_VAL  s1)   (QUEST3_VAL  s)     = [QUEST3_VAL s]
setAtt (QUEST6_VAL  s1)   (QUEST6_VAL  s)     = [QUEST6_VAL s]

setAtt (PREP_VAL s1) (PREP_VAL s) = [PREP_VAL s]
setAtt (PREPN_VAL s1) (PREPN_VAL s) = [PREPN_VAL s]
setAtt (PREPNPH_VAL s1) (PREPNPH_VAL s) = [PREPNPH_VAL s]
setAtt (PREPPH_VAL s1) (PREPPH_VAL s) = [PREPPH_VAL s]
setAtt (YEAR_VAL s1) (YEAR_VAL s) = [YEAR_VAL s]

setAtt (SUPERPHSTART_VAL s1) (SUPERPHSTART_VAL s) = [SUPERPHSTART_VAL s]
setAtt (SUPER_VAL s1) (SUPER_VAL s) = [SUPER_VAL s]
setAtt (SUPERPH_VAL s1) (SUPERPH_VAL s) = [SUPERPH_VAL s]
--------- *********************** --------------







