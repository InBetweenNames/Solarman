module TypeAg2 where

import System.IO.Unsafe
import Getts

data AttValue = VAL             {getAVAL    ::   Int} 
              | MaxVal          {getAVAL    ::   Int} 
              | SubVal          {getAVAL    ::   Int}
              | RepVal          {getAVAL    ::   Int}
              | Res             {getRVAL    ::   DisplayTree}
              | B_OP            {getB_OP    ::   (Int -> Int -> Int)} 
              | U_OP            {getU_OP    ::   (Int -> Int)} 
              | SENT_VAL        {getSV      ::   IO ES}
              | ErrorVal        {getEVAL    ::   String}    
	      | NOUNCLA_VAL     {getAVALS   ::   IO ES} 
	      | VERBPH_VAL      {getAVALS   ::   IO ES}  
	      | ADJ_VAL         {getAVALS   ::   IO ES} 
	      | TERMPH_VAL      {getTVAL    ::   (IO ES -> IO ES)}     
	      | DET_VAL         {getDVAL    ::   (IO ES -> IO ES -> IO ES)} 
	      | VERB_VAL        {getBR      ::   Relation}      
	      | RELPRON_VAL     {getRELVAL  ::   (IO ES -> IO ES -> IO ES)}    
	      | NOUNJOIN_VAL    {getNJVAL   ::   (IO ES -> IO ES -> IO ES)}
	      | VBPHJOIN_VAL    {getVJVAL   ::   (IO ES -> IO ES -> IO ES)}    
	      | TERMPHJOIN_VAL  {getTJVAL   ::   ((IO ES -> IO ES) -> (IO ES -> IO ES) -> (IO ES -> IO ES)) }
		  | PREP_VAL		{getPREPVAL ::  ([([String], IO ES -> IO ES)])} -- used in "hall discovered phobos with a telescope" as "with".  
		  | PREPN_VAL		{getPREPNVAL :: [String]} --used for mapping between prepositions and their corresponding identifiers in the database.  I.e., "in" -> ["location", "year"]
		  | PREPPH_VAL		{getPREPPHVAL :: ([String], IO ES -> IO ES)}
	      | LINKINGVB_VAL   {getLINKVAL ::   (IO ES -> IO ES)}
	      | SENTJOIN_VAL    {getSJVAL   ::   (IO ES -> IO ES -> IO ES)}
	      | DOT_VAL         {getDOTVAL  ::   IO String}
	      | QM_VAL          {getQMVAL   ::   IO String}
	      | QUEST_VAL       {getQUVAL   ::   IO String}
	      | QUEST1_VAL      {getQU1VAL  ::   (IO ES -> IO String)}
	      | QUEST2_VAL      {getQU2VAL  ::   (IO ES -> IO String)}
	      | QUEST3_VAL      {getQU3VAL  ::   (IO ES -> IO ES -> IO String)}
		  | YEAR_VAL		{getYEARVAL ::   Int}

--            | RESULT [sys_message]
data MemoL    = Start | Tree | Num | Emp | ALeaf String | Expr | Op  | ET
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
type Entity         =  String  
type ES             =  Image -- [Int]
--type Bin_Rel        =  [(Entity,Entity)] -- [(Int, Int)]
--type Relation		= (ES -> Bool) -> ES
type Relation = String

data DisplayTree = B [DisplayTree]
                 | N Int
                   deriving (Show, Eq)


--instance Show AttValue where

showio :: AttValue -> IO String
showio (VAL  j)     = return $ "VAL "    ++ show j
showio (MaxVal j)   = return $ "MaxVal " ++ show j
showio (SubVal j)   = return $ "SubVal " ++ show j
showio (RepVal j)   = return $ "RepVal " ++ show j
showio (Res j)      = return $ "Tree: "  ++ show j
showio (B_OP j)     = return $ "B_OP"
showio (U_OP j)     = return $ "U_OP"
showio (SENT_VAL j) = j >>= return . show 
showio (ErrorVal j) = return j
showio (NOUNCLA_VAL j) = j >>= return . (++) "NOUNCLA_VAL " . unwords . map fst
showio (VERBPH_VAL j)  = j >>= return . (++) "VERBPH_VAL " . unwords . map fst
showio (ADJ_VAL    j)  = j >>= return . (++) "ADJ_VAL " . unwords . map fst
showio (TERMPH_VAL j)  = return "TERMPH_VAL "      
showio (DET_VAL j)     = return "DET_VAL " 
showio (VERB_VAL j)    = return $ "VERB_VAL " ++ j
showio (RELPRON_VAL j)  = return "RELPRON_VAL "
showio (NOUNJOIN_VAL j)  = return "NOUNJOIN_VAL "
showio (VBPHJOIN_VAL j)  = return "VBPHJOIN_VAL "
showio (TERMPHJOIN_VAL j)  = return "TERMPHJOIN_VAL "
showio (PREP_VAL j)  = return "PREP_VAL "
showio (LINKINGVB_VAL j)  = return "LINKINGVB_VAL "
showio (SENTJOIN_VAL j)  = return "SENTJOIN_VAL "
showio (DOT_VAL j) = j 
showio (QM_VAL j) = j 
showio (QUEST_VAL j) = j 
showio (QUEST1_VAL j)  = return "QUEST1_VAL"
showio (QUEST2_VAL j)  = return "QUEST2_VAL"
showio (QUEST3_VAL j)  = return "SENTJOIN_VAL"

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
	(PREP_VAL s1)	   == (PREP_VAL s) = True
	(PREPN_VAL s1)	   == (PREPN_VAL s) = True
	(PREPPH_VAL s1)    == (PREPPH_VAL s) = True
	(YEAR_VAL s1)	   == (YEAR_VAL s) = True
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







