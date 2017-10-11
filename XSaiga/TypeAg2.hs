{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE OverloadedStrings #-}
module XSaiga.TypeAg2 where

import XSaiga.Getts
import Data.Text as T hiding (map)
import XSaiga.ShowText
import Control.Arrow
import Control.Applicative

type TF a = [Triple] -> a
data GettsTree =
      GettsBranch [GettsTree]
    | GettsPreps [Text] GettsTree
    | GettsTP [Text] Text GettsTree
    --"User" functions
    | GettsNone
    | GettsT [Text] Text
    | GettsMembers Text
    | GettsPrep [Text]
    | GettsAttachP Text
      deriving (Eq, Show)

--TODO: Abstract tree into separate datastructure as Foldable and Traversable
--It seems to me there are five layers:
{-

User layer: GettsT, GettsPrep, GettsAttachP ... these are like semantic "words"
Unparsed Tree layer: GettsSubQuery, GettsList, GettsNode ... this is like the parse tree of those "words"
Semantic layer: Group SubQuery, List, Node, etc... into GettsTP, keeping tree
Flattened layer: Only a list of GettsMembers and a list of GettsTP
Optimization layer: Knowledge to reduce queries (subsets, GettsTP of same type)

-}
{-data GTree a = GPlain a | GList [a] | GSub a [a] | GNone

superApply :: Getts -> Getts -> GTree Getts
superApply (GettsPrep ts) x = GSub (GettsPrep ts) x

sortofComp :: (GTree b -> GTree c) -> (GTree a -> GTree b) -> GTree a -> GTree c
sortofComp f g | GPlain x <- f GNone, GPlain y <- g GNone = f . g
sortofComp f g | GPlain x <- f GNone, GList ys GNone <- g = 

sortofComp f g = f . g
-}
data SemFunc a = SemFunc { getSem :: a, getGetts :: GettsTree -> GettsTree }

type GettsFlat = ([GettsTree], [GettsTree])

applyGetts x = getGetts x GettsNone

--Unclean -- Foldable would help
flattenGetts :: SemFunc a -> GettsFlat
flattenGetts = flatten . applyGetts
  where
    concatFlat = Prelude.concat *** Prelude.concat
    flatten (GettsBranch xs) = concatFlat $ unzip $ map flatten xs
    flatten (GettsTP props rel xs) = let (ts, ms) = flatten xs in ((GettsT props rel):ts, ms)
    flatten GettsNone = ([], [])
    flatten (GettsMembers set) = ([], [GettsMembers set])

(>|<) :: a -> GettsTree -> SemFunc a
f >|< g = SemFunc f (attach g)

--Reduce GettsTree down to minimum number of queries that spans original
--it is okay if they end up pulling in more data than needed
gettsOptimize :: GettsTree -> GettsTree
gettsOptimize u = u

unionerr x y = error $ "attempt to union: " ++ show x ++ " with " ++ show y --TODO: debugging

--cannot use sequenceA because it would look like
--with (a (telescope (by (a (person))))) instead of (with ( a telescope )), (by (a person))
gatherPreps :: [SemFunc a] -> SemFunc [a]
gatherPreps sems = Prelude.foldr (\x -> \y ->  ((getSem x):(getSem y)) >|< (getGetts x GettsNone `prepAttach` getGetts y GettsNone)) (pure []) sems

--To be used for prepositional phrases ONLY
--Regular attach can't be used due to function composition
prepAttach :: GettsTree -> GettsTree -> GettsTree
prepAttach GettsNone x = x
prepAttach x GettsNone = x
prepAttach (GettsPrep props) (GettsPrep props') = GettsPrep (props ++ props')
prepAttach (GettsPrep props) (GettsPreps props' sub) = GettsPreps (props ++ props') sub
prepAttach (GettsPreps props sub) (GettsPrep props') = GettsPreps (props ++ props') sub
prepAttach (GettsPreps props sub) (GettsPreps props' sub') = GettsPreps (props ++ props') (sub `attach` sub') --TODO: optimize
prepAttach (GettsPreps props sub) x = unionerr (GettsPreps props sub) x 

attach :: GettsTree -> GettsTree -> GettsTree
attach GettsNone x = x
attach x GettsNone = x

attach x@(GettsPrep _) y@(GettsPrep _) = unionerr x y
attach x@(GettsPrep _) y@(GettsPreps _ _) = unionerr x y
attach (GettsPrep props) x = GettsPreps props x

attach x@(GettsPreps _ _) y@(GettsPrep _) = unionerr x y
attach x@(GettsPreps _ _) y@(GettsPreps _ _) = unionerr x y

attach (GettsT props rel) (GettsPrep props') = GettsT (props ++ props') rel
attach (GettsT props rel) (GettsPreps props' sub) = GettsTP (props ++ props') rel sub
attach (GettsT props rel) x = GettsTP props rel x --needed to support active trans without preps directly

attach (GettsAttachP prop) (GettsT props' rel) = GettsT (prop:props') rel
attach (GettsAttachP prop) (GettsTP props rel sub) = GettsTP (prop:props) rel sub

--this is where optimization can take place TODO
attach (GettsBranch x) (GettsBranch y) = GettsBranch (x ++ y)
attach x (GettsBranch y) = GettsBranch (x:y)
attach (GettsBranch x) y = GettsBranch (x ++ [y])
attach x y = GettsBranch [x, y]

--a <*> moon <*> spins... 

instance Functor SemFunc where
  fmap f (SemFunc sem iu) = SemFunc (f sem) iu

instance Applicative SemFunc where
  (SemFunc f iu1) <*> (SemFunc sem iu2) = SemFunc (f sem) (iu1 . iu2)
  pure x = SemFunc x id

--PUBLIC INTERFACE (TODO)
--Get members of named set
--get_members :: (TripleStore m) => m -> Text -> IO FDBR
--get_members = getts_members
get_members :: Text -> SemFunc (TF FDBR)
get_members set = (\r -> pure_getts_members r set) >|< GettsMembers set

get_subjs_of_event_type :: Text -> SemFunc (TF FDBR)
get_subjs_of_event_type ev_type = (\r -> make_fdbr_with_prop (pure_getts_triples_entevprop_type r ["subject"] ev_type) "subject") >|< GettsT ["subject"] ev_type

data AttValue = VAL             {getAVAL    ::   Int} 
              | MaxVal          {getAVAL    ::   Int} 
              | SubVal          {getAVAL    ::   Int}
              | RepVal          {getAVAL    ::   Int}
              | Res             {getRVAL    ::   DisplayTree}
              | B_OP            {getB_OP    ::   (Int -> Int -> Int)} 
              | U_OP            {getU_OP    ::   (Int -> Int)} 
              | SENT_VAL        {getSV      ::   SemFunc (TF FDBR)}
              | ErrorVal        {getEVAL    ::   Text}    
              | NOUNCLA_VAL     {getAVALS   ::   SemFunc (TF FDBR)} 
              | VERBPH_VAL      {getAVALS   ::   SemFunc (TF FDBR)}  
              | ADJ_VAL         {getAVALS   ::   SemFunc (TF FDBR)} 
              | TERMPH_VAL      {getTVAL    ::   SemFunc (TF FDBR -> TF FDBR)}     
              | DET_VAL         {getDVAL    ::   SemFunc (TF FDBR -> TF FDBR -> TF FDBR)} 
          | VERB_VAL        {getBR      ::   Relation}      
          | RELPRON_VAL     {getRELVAL  ::   SemFunc (TF FDBR -> TF FDBR -> TF FDBR)}    
          | NOUNJOIN_VAL    {getNJVAL   ::   SemFunc (TF FDBR -> TF FDBR -> TF FDBR)}
          | VBPHJOIN_VAL    {getVJVAL   ::   SemFunc (TF FDBR -> TF FDBR -> TF FDBR)}    
          | TERMPHJOIN_VAL  {getTJVAL   ::   SemFunc ((TF FDBR -> TF FDBR) -> (TF FDBR -> TF FDBR) -> TF FDBR -> TF FDBR) }
          | PREP_VAL        {getPREPVAL ::   [SemFunc ([Text], (TF FDBR -> TF FDBR))]} -- used in "hall discovered phobos with a telescope" as "with".  
          | PREPN_VAL       {getPREPNVAL :: [Text]} --used for mapping between prepositions and their corresponding identifiers in the database.  I.e., "in" -> ["location", "year"]
          | PREPPH_VAL      {getPREPPHVAL :: SemFunc ([Text], (TF FDBR -> TF FDBR))}
          | LINKINGVB_VAL   {getLINKVAL ::   SemFunc (TF FDBR -> TF FDBR)}
          | SENTJOIN_VAL    {getSJVAL   ::   SemFunc (TF FDBR -> TF FDBR -> TF FDBR)}
          | DOT_VAL         {getDOTVAL  ::   SemFunc (TF Text)}
          | QM_VAL          {getQMVAL   ::   SemFunc (TF Text)}
          | QUEST_VAL       {getQUVAL   ::   SemFunc (TF Text)}
          | QUEST1_VAL      {getQU1VAL  ::   SemFunc (TF FDBR -> TF Text)}
          | QUEST2_VAL      {getQU2VAL  ::   SemFunc (TF FDBR -> TF Text)}
          | QUEST3_VAL      {getQU3VAL  ::   SemFunc (TF FDBR -> TF FDBR -> TF Text)}
          | YEAR_VAL        {getYEARVAL ::   Int}

--            | RESULT [sys_message]
--Also called a "NodeName"
data MemoL    = Start | Tree | Num | Emp | ALeaf Text | Expr | Op  | ET
              | Pnoun|Cnoun|Adj|Det|Intransvb|Transvb|Linkingvb|Relpron|Termphjoin|Verbphjoin|Nounjoin|Preps|Prepph|Prepn|Indefpron|Sentjoin|Quest1|Quest2|Quest3|Quest4a|Quest4b
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
type Relation = Text

data DisplayTree = B [DisplayTree]
                 | N Int
                   deriving (Show, Eq)

{-showio :: AttValue -> IO Text
showio (VAL  j)     = return $ "VAL " `T.append` tshow j
showio (MaxVal j)   = return $ "MaxVal " `T.append` tshow j
showio (SubVal j)   = return $ "SubVal " `T.append` tshow j
showio (RepVal j)   = return $ "RepVal " `T.append` tshow j
showio (Res j)      = return $ "Tree: "  `T.append` tshow j
showio (B_OP j)     = return $ "B_OP"
showio (U_OP j)     = return $ "U_OP"
showio (SENT_VAL j) = j >>= return . tshow 
showio (ErrorVal j) = return j
showio (NOUNCLA_VAL j) = j >>= return . T.append ( "NOUNCLA_VAL ") . T.unwords . map fst
showio (VERBPH_VAL j)  = j >>= return . T.append ( "VERBPH_VAL ") . T.unwords . map fst
showio (ADJ_VAL    j)  = j >>= return . T.append ( "ADJ_VAL ") . T.unwords . map fst
showio (TERMPH_VAL j)  = return $ "TERMPH_VAL "      
showio (DET_VAL j)     = return $ "DET_VAL " 
showio (VERB_VAL j)    = return $ T.append "VERB_VAL"  j
showio (RELPRON_VAL j)  = return $ "RELPRON_VAL "
showio (NOUNJOIN_VAL j)  = return $ "NOUNJOIN_VAL "
showio (VBPHJOIN_VAL j)  = return $ "VBPHJOIN_VAL "
showio (TERMPHJOIN_VAL j)  = return $ "TERMPHJOIN_VAL "
showio (PREP_VAL j)  = return $ "PREP_VAL "
showio (LINKINGVB_VAL j)  = return $ "LINKINGVB_VAL "
showio (SENTJOIN_VAL j)  = return $ "SENTJOIN_VAL "
showio (DOT_VAL j) = j 
showio (QM_VAL j) = j 
showio (QUEST_VAL j) = j 
showio (QUEST1_VAL j)  = return $ "QUEST1_VAL"
showio (QUEST2_VAL j)  = return $ "QUEST2_VAL"
showio (QUEST3_VAL j)  = return $ "SENTJOIN_VAL"-}


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
    (PREP_VAL s1)      == (PREP_VAL s) = True
    (PREPN_VAL s1)     == (PREPN_VAL s) = True
    (PREPPH_VAL s1)    == (PREPPH_VAL s) = True
    (YEAR_VAL s1)      == (YEAR_VAL s) = True
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

setAtt (PREP_VAL s1) (PREP_VAL s) = [PREP_VAL s]
setAtt (PREPN_VAL s1) (PREPN_VAL s) = [PREPN_VAL s]
setAtt (PREPPH_VAL s1) (PREPPH_VAL s) = [PREPPH_VAL s]
setAtt (YEAR_VAL s1) (YEAR_VAL s) = [YEAR_VAL s]


--------- *********************** --------------







