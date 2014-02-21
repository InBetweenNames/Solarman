module TypeAg where


data AttValue = VAL             {getAVAL    ::   Int} 
              | MaxVal          {getAVAL    ::   Int} 
              | SubVal          {getAVAL    ::   Int}
              | RepVal          {getAVAL    ::   Int}
              | Res             {getRVAL    ::   DisplayTree}
              | B_OP            {getB_OP    ::   (Int -> Int -> Int)} 
              | U_OP            {getU_OP    ::   (Int -> Int)} 
              | SENT_VAL        {getSV      ::   Bool}
              | ErrorVal        {getEVAL    ::   String}    
	      | NOUNCLA_VAL     {getAVALS   ::   ES} 
	      | VERBPH_VAL      {getAVALS   ::   ES}  
	      | ADJ_VAL         {getAVALS   ::   ES} 
	      | TERMPH_VAL      {getTVAL    ::   (ES -> Bool)}     
	      | DET_VAL         {getDVAL    ::   (ES -> ES -> Bool)} 
	      | VERB_VAL        {getBR      ::   Bin_Rel}      
	      | RELPRON_VAL     {getRELVAL  ::   (ES -> ES -> ES)}    
	      | NOUNJOIN_VAL    {getNJVAL   ::   (ES -> ES -> ES)}
	      | VBPHJOIN_VAL    {getVJVAL   ::   (ES -> ES -> ES)}    
	      | TERMPHJOIN_VAL  {getTJVAL   ::   ((ES -> Bool) -> (ES -> Bool)  -> (ES -> Bool))}
	      | PREP_VAL        {getPREPVAL ::   (ES -> ES)}
	      | LINKINGVB_VAL   {getLINKVAL ::   (ES -> ES)}
	      | SENTJOIN_VAL    {getSJVAL   ::   (Bool -> Bool -> Bool)}
	      | DOT_VAL         {getDOTVAL  ::   String}
	      | QM_VAL          {getQMVAL   ::   String}
	      | QUEST_VAL       {getQUVAL   ::   String}
	      | QUEST1_VAL      {getQU1VAL  ::   (Bool -> String)}
	      | QUEST2_VAL      {getQU2VAL  ::   (ES -> String)}
	      | QUEST3_VAL      {getQU3VAL  ::   (ES -> ES -> String)}

--            | RESULT [sys_message]
data MemoL    = Start | Tree | Num | Emp | ALeaf String | Expr | Op  | ET
              | Pnoun|Cnoun|Adj|Det|Intransvb|Transvb|Linkingvb|Relpron|Termphjoin|Verbphjoin|Nounjoin|Prep|Indefpron|Sentjoin|Quest1|Quest2|Quest3|Quest4a|Quest4b
              | Snouncla|Relnouncla|Nouncla|Adjs|Detph|Transvbph|Verbph|Termph|Jointermph|Joinvbph|Sent|Two_sent|Question|Quest4|Query
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
type Entity         =  Integer  
type Entityset      =  [Entity]   
type ES             =  Entityset -- [Int]
type Bin_Rel        =  [(Entity,Entity)] -- [(Int, Int)]

data DisplayTree = B [DisplayTree]
                 | N Int
                   deriving (Show, Eq)


instance Show AttValue where
      show (VAL  j)     = "VAL "    ++ show j
      show (MaxVal j)   = "MaxVal " ++ show j
      show (SubVal j)   = "SubVal " ++ show j
      show (RepVal j)   = "RepVal " ++ show j
      show (Res j)      = "Tree: "  ++ show j
      show (B_OP j)     = "B_OP"
      show (U_OP j)     = "U_OP"
      show (SENT_VAL j) = show j
      show (ErrorVal j) = {-show-} j
      show (NOUNCLA_VAL j) = "NOUNCLA_VAL " ++ showListOfInt j      
      show (VERBPH_VAL j)  = "VERBPH_VAL "  ++ showListOfInt j      
      show (ADJ_VAL    j)  = "ADJ_VAL "     ++ showListOfInt j      
      show (TERMPH_VAL j)  = "TERMPH_VAL "      
      show (DET_VAL j)     = "DET_VAL " 
      show (VERB_VAL j)    = "VERB_VAL "    ++   showListOfPInt j
      show (RELPRON_VAL j)  = "RELPRON_VAL "
      show (NOUNJOIN_VAL j)  = "NOUNJOIN_VAL "
      show (VBPHJOIN_VAL j)  = "VBPHJOIN_VAL "
      show (TERMPHJOIN_VAL j)  = "TERMPHJOIN_VAL "
      show (PREP_VAL j)  = "PREP_VAL "
      show (LINKINGVB_VAL j)  = "LINKINGVB_VAL "
      show (SENTJOIN_VAL j)  = "SENTJOIN_VAL "
      show (DOT_VAL j) = {-show-} j
      show (QM_VAL j) = {-show-} j
      show (QUEST_VAL j) = {-show-} j
      show (QUEST1_VAL j)  = "QUEST1_VAL"
      show (QUEST2_VAL j)  = "QUEST2_VAL"
      show (QUEST3_VAL j)  = "SENTJOIN_VAL"



showListOfInt (x:xs) = show x ++ " " ++ showListOfInt xs
showListOfInt []     = " "
showListOfPInt ((a,b):abs) = show a ++ " & " ++ show b ++ " " ++ showListOfPInt abs
showListOfPInt []    = " "

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
      (PREP_VAL j)       == (PREP_VAL j') = True
      (LINKINGVB_VAL j)  == (LINKINGVB_VAL j') = True
      (SENTJOIN_VAL j)   == (SENTJOIN_VAL j') = True
      (DOT_VAL j)        == (DOT_VAL j') = True
      (QM_VAL j)         == (QM_VAL j') = True
      (QUEST_VAL j)      == (QUEST_VAL j') = True
      (QUEST1_VAL j)     == (QUEST1_VAL j') = True
      (QUEST2_VAL j)     == (QUEST2_VAL j') = True
      (QUEST3_VAL j)     == (QUEST3_VAL j') = True
      _            ==_              = False






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
--------- *********************** --------------







