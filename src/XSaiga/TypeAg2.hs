{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module XSaiga.TypeAg2 where

import XSaiga.Getts
import Data.Text as T hiding (map)
import XSaiga.ShowText
import Control.Arrow
import Data.List (nub)
--import Control.Monad.State.Lazy
import qualified Data.Map.Strict as Map

type GettsCache = Map.Map GettsTree FDBR

--NOTE: much of this stuff should be moved to Getts.hs
--NOTE: *2.hs should be renamed such that there is no 2
--NOTE: use the state monad to memoize everything
--type TF a = [Triple] -> State GettsCache a
type TF a = [Triple] -> a

--NOTE: this is more like a syntax tree, it should be renamed
--NOTE: FGetts* is essentially the actual Getts information
--NOTE: GettsNone is more like a placeholder rather than just "get nothing" -- it is treated like an identity value for now
data GettsTree =
      GettsNone
    | GettsMembers Text
    | GettsTP [Text] Relation [GettsTree]
    | GettsPreps [Text] [GettsTree]
    | GettsIntersect GettsTree GettsTree --representing result of intersect_fdbr (only evs from 2nd tree are kept)
    | GettsUnion GettsTree GettsTree --representing result of "union" of fdbrs.  Keeps everything.
    deriving (Eq, Show, Ord)

data GettsFlatTypes =
    FGettsMembers Text
  | FGettsT [Text] Text

type FGettsMembers = Text
type FGettsT = ([Text], Text)

type GettsFlat = ([FGettsMembers], [FGettsT])

--NOTE: it seems possible to do without a biapplicative bifunctor if you allow overlapping instances (or find a way to work around those)
--Also, you need AllowAmbiguousTypes for things like (+) >|< (-) (but things like plus >|< minus should be fine)

type family Treeify x y where
    Treeify (x -> a2) (y -> b2) = (x, y) -> Treeify a2 b2
    Treeify x y = (x, y)

--Convert a function of the form a -> b -> c to the form GettsTree -> GettsTree -> ... -> GettsTree
--Keep the same arity
type family GettsTreeify x where
    GettsTreeify (TF a) = GettsTree -- Needed to halt recursion when a TF is found
    GettsTreeify (a -> b) = GettsTreeify a -> GettsTreeify b
    GettsTreeify a = GettsTree -- For everything else

class TreeifyImpl x y where
    (>|<) :: x -> y -> Treeify x y

instance {-# OVERLAPPING #-} (TreeifyImpl a2 b2) => TreeifyImpl (a1 -> a2) (b1 -> b2) where
    f >|< g = (\(a, b) ->  (f a) >|< (g b))

instance (Treeify a b ~ (a, b)) => TreeifyImpl a b where
    a >|< b = (a, b)

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

--A SemFunc is really just a tuple of two functions
--Use GettsTreeify to keep the same arity
type SemFunc a = Treeify a (GettsTreeify a)

--Just for presentation purposes
instance Show (TF FDBR) where
  show x = "TF FDBR"

instance Show (TF GFDBR) where
  show x = "TF GFDBR"

instance Show (TF Text) where
  show x = "TF Text"

--This is also bothering me!
{-
sem_memo :: ([Triple] -> State GettsCache FDBR) -> GettsTree ->  [Triple] -> State GettsCache FDBR --TF FDBR
sem_memo f key rtriples = state $ \cache ->
    case Map.lookup key cache of
        Just fdbr -> (fdbr, cache)
        Nothing -> runState cache $ do
                                 fdbr <- f rtriples
                                 cache' <- get
                                 let cache'' = Map.insert key fdbr cache'
                                 return (fdbr, cache'')

f >>>|<<< key = sem_memo f key
-}

--Unclean -- Foldable would help
--TODO: filter duplicates
--TODO: make [SemFunc a] not used with gatherPreps, apply directly to verb?
--Would avoid GettsPreps situation

flattenGetts :: GettsTree -> GettsFlat
flattenGetts GettsNone = ([],[])
flattenGetts (GettsMembers set) = ([set], [])
flattenGetts (GettsTP props (rel,_,_) sub) = merge ([], [(props, rel)]) (Prelude.concat *** Prelude.concat $ unzip $ map flattenGetts sub)
flattenGetts (GettsPreps props sub) = error "Cannot flatten preps"
flattenGetts (GettsIntersect i1 i2) = merge (flattenGetts i1) (flattenGetts i2)
flattenGetts (GettsUnion i1 i2) = merge (flattenGetts i1) (flattenGetts i2)

merge :: GettsFlat -> GettsFlat -> GettsFlat
merge (x1, y1) (x2, y2) = (x1 ++ x2, y1 ++ y2)

{-
gettsMemoize :: GettsTree -> ([Triple] -> FDBR) -> [Triple] -> State GettsCache FDBR
gettsMemoize key f rtriples = state $ \cache ->
    case Map.lookup key cache of
        Just fdbr -> (fdbr, cache)
        Nothing -> let fdbr = f rtriples in (fdbr, Map.insert key fdbr cache)

--Easy memoization operator (may only be used with fully applied trees)
f >>|<< key = gettsMemoize key f >|< key
-}

--Reduce GettsTree down to minimum number of queries that spans original
--it is okay if they end up pulling in more data than needed
gettsOptimize :: GettsTree -> GettsTree
gettsOptimize u = u

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

gettsIdentity f GettsNone y = y
gettsIdentity f x GettsNone = x
gettsIdentity f x y = f x y

gettsIntersect = gettsIdentity GettsIntersect

gettsApply g = g GettsNone

--TODO: make this a proper union
gettsUnion = gettsIdentity GettsUnion

--apply over the intersect_entevimages, remember evs of im2 are preserved and im1 are discarded
gettsAttachP :: T.Text -> GettsTree -> GettsTree
gettsAttachP prop (GettsIntersect x y) = GettsIntersect x (gettsAttachP prop y)
gettsAttachP prop (GettsTP props rel sub) = GettsTP (prop:props) rel sub

--cannot use sequenceA because it would look like
--with (a (telescope (by (a (person))))) instead of (with ( a telescope )), (by (a person))
gatherPreps :: [SemFunc ([T.Text], Maybe Ordering, TF FDBR -> TF FDBR)] -> SemFunc [([T.Text], Maybe Ordering, TF FDBR -> TF FDBR)]
gatherPreps preps = preps_tf >|< GettsPreps prepNames (map snd preps)
  where
    prepNames = nub $ Prelude.concatMap (\(a, _, _) -> a) preps_tf
    preps_tf = map fst preps

--a <<*>> moon <<*>> spins... 
--PUBLIC INTERFACE (TODO)
--TODO: make_fdbr_with_prop is DANGEROUS
--Get members of named set
--get_members :: (TripleStore m) => m -> Text -> IO FDBR
--get_members = getts_members

get_members :: Text -> SemFunc (TF FDBR)
get_members set = (\rtriples -> pure_getts_members rtriples set) >|< GettsMembers set

--TODO: revise this for new transitive verb definition.  should not assume these fields in general.
--Used for intransitive verbs
--The above revisions means "discoverer" works because "subject" is used, but consider the others that use "object", "implement", etc
get_subjs_of_event_type :: Text -> SemFunc (TF FDBR)
get_subjs_of_event_type ev_type =
    (\rtriples -> make_fdbr_with_prop (pure_getts_triples_entevprop_type rtriples ["subject"] ev_type) "subject")
    >|<
    GettsTP ["subject"] (ev_type, "subject", "object") []

--NOTE: anytime you add to the following ADT, you must extend setAtt to cover it

data AttValue = VAL             {getAVAL    ::   Int}
              | MaxVal          {getAVAL    ::   Int}
              | SubVal          {getAVAL    ::   Int}
              | RepVal          {getAVAL    ::   Int}
              | Res             {getRVAL    ::   DisplayTree}
              | B_OP            {getB_OP    ::   (Int -> Int -> Int)}
              | U_OP            {getU_OP    ::   (Int -> Int)}
              | SENT_VAL        {getSV      ::   SemFunc (TF FDBR) }
              | ErrorVal        {getEVAL    ::   Text}
              | NOUNCLA_VAL     {getAVALS   ::   SemFunc (TF FDBR) }
              | VERBPH_VAL      {getAVALS   ::   SemFunc (TF FDBR) }
              | ADJ_VAL         {getAVALS   ::   SemFunc (TF FDBR) }
              | TERMPH_VAL      {getTVAL    ::   SemFunc (TF FDBR -> TF FDBR)}
              | DET_VAL         {getDVAL    ::   SemFunc (TF FDBR -> TF FDBR -> TF FDBR)}
              | VERB_VAL        {getBR      ::   Relation } --stores relation, "subject" and "object". TODO: need to expand on later for "used"      
          | RELPRON_VAL     {getRELVAL  ::   SemFunc (TF FDBR -> TF FDBR -> TF FDBR)}
          | NOUNJOIN_VAL    {getNJVAL   ::   SemFunc (TF FDBR -> TF FDBR -> TF FDBR)}
          | VBPHJOIN_VAL    {getVJVAL   ::   SemFunc (TF FDBR -> TF FDBR -> TF FDBR)}
          | TERMPHJOIN_VAL  {getTJVAL   ::   SemFunc (TF FDBR -> TF FDBR) -> SemFunc (TF FDBR -> TF FDBR) -> SemFunc (TF FDBR -> TF FDBR) }
          | SUPERPHSTART_VAL  {getSUPERPHSTARTVAL :: ()}
          | SUPER_VAL       {getSUPERVAL ::  Ordering}
          | SUPERPH_VAL     {getSUPERPHVAL :: (Ordering, SemFunc (TF FDBR -> TF FDBR))}
          | PREP_VAL       {getPREPVAL ::   [SemFunc ([Text], Maybe Ordering, (TF FDBR -> TF FDBR))]} -- used in "hall discovered phobos with a telescope" as "with".  
          | PREPN_VAL       {getPREPNVAL :: [Text]} --used for mapping between prepositions and their corresponding identifiers in the database.  I.e., "in" -> ["location", "year"]
          | PREPNPH_VAL     {getPREPNPHVAL :: [Text]}
          | PREPPH_VAL      {getPREPPHVAL :: SemFunc ([Text], Maybe Ordering, (TF FDBR -> TF FDBR))}
          | LINKINGVB_VAL   {getLINKVAL ::   SemFunc (TF FDBR -> TF FDBR)}
          | SENTJOIN_VAL    {getSJVAL   ::   SemFunc (TF FDBR -> TF FDBR -> TF FDBR)}
          | DOT_VAL         {getDOTVAL  ::   SemFunc (TF Text)}
          | QM_VAL          {getQMVAL   ::   SemFunc (TF Text)}
          | QUEST_VAL       {getQUVAL   ::   SemFunc (TF Text)}
          | QUEST1_VAL      {getQU1VAL  ::   SemFunc (TF FDBR -> TF Text)}
          | QUEST2_VAL      {getQU2VAL  ::   SemFunc (TF FDBR -> TF Text)}
          | QUEST6_VAL      {getQU6VAL  ::   SemFunc (TF FDBR) -> SemFunc(TF Text)}
          | QUEST3_VAL      {getQU3VAL  ::   SemFunc (TF FDBR -> TF FDBR -> TF Text)}
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







