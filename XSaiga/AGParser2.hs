{-# LANGUAGE Trustworthy #-}
module XSaiga.AGParser2 where
import Prelude hiding ((*>))
import Text.PrettyPrint.HughesPJ as PPH hiding (empty)  -- ((PPH.<+>), vcat, text, render,($$),nest,Doc)
import Data.List
import XSaiga.TypeAg2
import Control.Monad
import Control.Applicative hiding ((<|>), (*>))




---- ************************************ -----------------------

type Instance = IRec
                
data SorI     = S | I |NILL  deriving (Eq,Ord,Show, Enum) 
                -- Synthesized or inherited or NILL


type IRec     = (SorI, Id) 
                -- uniquely determines synthesized or inherited attribute for an NT
                
data Useless  = OF|ISEQUALTO  deriving (Show, Eq)
                -- for decorating the semantic rules


       
data SRule_   = NT {iden :: Id, par :: NTType} | Rule {par' :: SeqType} | DRule {par'' :: SRule_ } 

type NTType  = Id -> InsAttVals -> M Foo
type SeqType = Id -> InsAttVals -> [SemRule] -> Result -> M Foo
type AltType = NTType
type SemRule =  (Instance,(InsAttVals, Id) -> InsAttVals)
------------
data Tree a = Leaf (a,Instance)
            | Branch [Tree a] 
            | SubNode ((NodeName, Instance), (Start1,End))
              deriving (Eq)


type NodeName = MemoL
type Start1   = (Int, InsAttVals)
type Start    = ((Int,InsAttVals), [String])
type End      = (Int, InsAttVals)
type Atts     = [AttValue] -- [(AttType, AttValue)]
type InsAttVals = [(Instance, Atts)]


type Mtable   = [(MemoL
                 ,[(Start1,(Context,Result))]
                 )
                ] 
type Result   = [((Start1, End),[Tree MemoL])]

type State    = Mtable
type Context  = ([MemoL],[(Int,[(MemoL, Int)])])

type M a = Start -> Context -> StateM a
type Seq a = Id -> InsAttVals -> [SemRule] -> Result -> Start -> Context -> StateM a
type Foo = (Context, Result)

-- ============================
newtype StateM t = State { unState :: State -> (t,State) }

instance Functor StateM where
    fmap  = liftM

instance Applicative StateM where
    pure  = return
    (<*>) = ap  -- defined in Control.Monad

instance Monad StateM where
  -- defines state propagation
  State m            >>= k 
         = State (\s -> let (a,y) = m s in unState (k a) y)
  return k             =  State (\s -> (k,s))


 -- extracts the state from the monad
get       :: StateM State
get        = State (\s -> (s,s))
put       :: State -> StateM ()
put s      = State (\_ -> ((),s))
modify    :: (State -> State) -> StateM ()
modify sf  = State (\s -> ((), sf s))

-- =============================

--------------- ******************************************** ---------------------------

(<|>) :: AltType -> AltType -> AltType
(p <|> q) idx inhx ((i,a),inp) c 
 = do (l1,m) <- p idx inhx ((i,[]),inp) c 
      (l2,n) <- q idx inhx ((i,[]),inp) c
      return ((union (fst l1) (fst l2),[]) ,(m ++ n))

--------------------------------------------------------
(*>) :: SeqType -> SeqType -> SeqType
(p *> q) idx inhx semx resx 
                 ((i,a),inp) cc 
 = do (l,m) <- p idx inhx semx resx 
                 ((i,[]),inp) cc 
      let passCtxt  e          
           = if e == i then cc 
             else empty_cuts
          mergeCuts e prev new 
           = if e == i 
             then (union prev new) 
             else prev  
       
          apply_to_all q [] l cc  
           = return ((fst l,[]),[])
   
          apply_to_all q (r:rs) l cc 
            = do (l1,m) <- q `add_P` (r,cc)
                 (l2,n) <- apply_to_all q rs l cc
                 return ((union (fst l1) (fst l2),[]),( m ++  n))
              
          q `add_P` (rp,cc) 
            = do let e  = pickEnd rp
                 (l1,m) <- (q idx inhx semx resx (e,inp) (passCtxt (fst e))) 
                 return ((mergeCuts (fst e) (fst l) (fst l1),[]),(addP m rp))       
   
      (apply_to_all q m l cc) -- CHANGE cc HERE
                        
pickEnd (((s,iA),(e,a)),t) = (e,[]) 
       
addP  [] ((s1,e1),t1)                    = []
addP  ((((s2,inA2),(e2,synA2)),t2):restQ) (((s1,inA1),(e1,synA1)),t1)  
 = (((s1,[]),(e2,[])),  a_P_Q_branch) : addP  restQ (((s1,inA1),(e1,synA1)),t1)
                                                                         
   where
   a_P_Q_branch = addToBranch (((s2,inA2),(e2,synA2)),t2)  (((s1,inA1),(e1,synA1)),t1)

-------- ************* ---------------
addToBranch (q1,((SubNode (name2,q)):ts2)) 
            (p1,((SubNode (name1,p)):ts1)) 
                            = [Branch [(SubNode (name1,p)),(SubNode (name2,q))]]

addToBranch (q1,((Branch t2):ts2)) 
            (p1,((Branch t1):ts1))
                            = [Branch (t1++t2)]

addToBranch (q1,((Branch t2):ts)) 
            (p1,((SubNode (name1,p)):ts1))
                            = [Branch ((SubNode (name1,p)):t2)]

addToBranch (q1,((SubNode (name2,q)):ts2)) 
            (p1,((Branch t1):ts))
                            = [Branch (t1++[(SubNode (name2,q))])]
                            

addToBranch (q1,((SubNode (name2,q)):ts2)) 
            (p1,[Leaf (x,i)])
                            = [Branch [(SubNode ((x,i) ,p1)),(SubNode (name2,q))]]
addToBranch (q1,[Leaf (x,i)]) 
            (p1,((SubNode (name1,p)):ts1)) 
                            = [Branch [(SubNode (name1,p)),(SubNode ((x,i),q1))]]
addToBranch (q1,((Branch t2):ts)) 
            (p1,[Leaf (x,i)])
                            = [Branch ((SubNode ((x,i),p1)):t2)]
addToBranch (q1,[Leaf (x,i)]) 
            (p1,((Branch t1):ts))
                            = [Branch (t1++[(SubNode ((x,i),q1))])]
addToBranch (q1,[Leaf (x2,i2)]) 
            (p1,[Leaf (x1,i1)])                         = [Branch [(SubNode ((x1,i1),p1)),(SubNode ((x2,i2),q1))]]

-------- ************* ---------------


empty_cuts = ([],[])                   
empty :: [(Instance, Atts)] -> M Foo
empty atts (x,inp) l = return (empty_cuts,[((x,(fst x,atts)), [Leaf (Emp, (NILL,O0))])])

term :: String -> Instance -> [(Instance, Atts)] -> [(Instance, Atts)] -> M Foo
term c id atts iatts ((r,a),dInp) l 
 |r - 1 == length dInp       = return (empty_cuts,[])
 |dInp!!(r - 1) == c         = return (empty_cuts,[(((r,[]),(r+1,atts)),[Leaf (ALeaf c, id)])])
 |otherwise                  = return (empty_cuts,[])  

        
memoize name f id downAtts ((inp,dAtts),dInput) context 
 = do s <- get        
      case (lookupT name inp (snd context) s) of   
       Just lRes -> do let lookUpRes = addNode name (S, id) (inp,downAtts) (snd1 lRes) 
                       return (fst1 lRes,lookUpRes)
       Nothing
           | funccount (snd context) inp name > (length dInput) - inp + 1
               -> return (([name],[]),[])
           | otherwise -> do let iC = ([],(incContext (snd context) name inp))
                             (l,newRes) <- f id downAtts ((inp,dAtts),dInput) iC 
                   -- let ((l,newRes),s1) = unState (f id downAtts (inp,dAtts) iC) s
                             let l1          = makeContext (fst l) (findContext (snd context) inp)
                             s1             <- get
                             let udtTab      = udt (l1,newRes) s1 name (inp,downAtts)  
                             let newFoundRes = addNode name (S, id) (inp,downAtts) newRes
                             put udtTab
                             return ( l1 ,newFoundRes)

        


findContext []     inp                    = []
findContext ((st,rest):sr) inp| st == inp = [(st,rest)]
                      | otherwise = findContext sr inp
                                                 
        
      

funccount []         inp name         = 0
funccount ((key,funcp):rest) inp name | key == inp = findf funcp 
                          | otherwise  = funccount rest inp name

                 where
                 findf  []           = 0
                 findf  ((tk,fc):rx) | tk == name = fc 
                         | otherwise  = findf rx        
        
            
fst1 [(a,b)] = a
snd1 [(a,b)] = b   

makeContext [] [(st,((n,c):ncs))]          = ([],[])
makeContext (r:rs) []                      = ((r:rs),[])
makeContext [] []                          = ([],[])
makeContext (r:rs) [(st,((n,c):ncs))]      = ((r:rs), [(st,makeContext_ (r:rs) ((n,c):ncs))])

makeContext_ [] ((n,c):ncs)      = []
makeContext_ (r:rs) ((n,c):ncs)  = makeContext__ r ((n,c):ncs) ++ makeContext_ rs ((n,c):ncs)

makeContext__ r [] = []
makeContext__ r ((n,c):ncs) | r == n    = (n,c): makeContext__ r ncs
                            | otherwise = makeContext__ r ncs


incContext [] name inp  = [(inp,[(name,1)])]
incContext ((st,((n,c):nc)):sn) name inp  
                  | st == inp = ((st, (addNT ((n,c):nc)) name inp ) :sn) 
                  | otherwise = ((st,((n,c):nc)): incContext sn name inp )
 

addNT []  name inp                     = [(name,1)]
addNT ((n,c):nc) name inp  | n == name = ((n,(c + 1)):nc)
                           | otherwise = ((n,c):addNT nc name inp)
                           

addNode name id (s',dA)  []                            
                       = []                           
addNode name id (s',dA)  oldRes -- ((((s,newIh),(e,atts)),t):rs)  
 = let res     = packAmb oldRes
       newSh x = [ ((sOri, snd id),attVal)| ((sOri, idOld),attVal)<- x]
   in  [(((s,newIh),(e, newSh atts)),[SubNode ((name, id),((s,nub dA),(e, newSh atts)))]) 
       | (((s,newIh),(e,atts)),t) <- oldRes]
mapName i []            = []
mapName i ((i',r):rest) = (i,r): mapName i rest


packAmb []                                                  = []
packAmb [((s1,e1),t1)]                                      = [((s1,e1),t1)]
packAmb [((s1,e1),t1),((s2,e2),t2)]    | isEq (s1,e1) (s2,e2) = [((s2,(fst e2, groupAtts (snd e1 ++ snd e2))), t1++t2)]
                                       | otherwise          = [((s1,e1),t1),((s2,e2),t2)]

packAmb (((s1,e1),t1):((s2,e2),t2):xs) | isEq (s1,e1) (s2,e2) = packAmb (((s2,(fst e2, groupAtts (snd e1 ++ snd e2))), t1++t2):xs)
                                       | otherwise          = ((s1,e1),t1):packAmb (((s2,e2),t2):xs)

isEq ((s1,b1),(e1,y1)) ((s2,b2),(e2,y2))
 | s1 == s2 && e1 == e2 = True
 | otherwise            = False
 

lookupT name inp context mTable 
 | lookupT1 name inp context mTable == [] = Nothing
 | otherwise                              = Just (lookupT1 name inp context mTable) 


lookupT1 name inp context mTable | res_in_table == [] = [] 
                                 | otherwise          = checkUsability inp context (lookupRes (res_in_table !! 0) inp)
                        
                   where 
                   res_in_table = [pairs|(n,pairs) <- mTable,n == name]


lookupRes [] inp                       = []
lookupRes (((i,dA),res):rs) inp | i == inp  = [res]
                                | otherwise = lookupRes rs inp 


checkUsability inp context [] = []
checkUsability inp context [((re,sc),res)] | re == []  =  [((re,sc),res)]
                                           | otherwise =  checkUsability_ (findInp inp context) (findInp inp sc) [((re,sc),res)]


findInp inp []                     = []
findInp inp ((s,c):sc) | s == inp  = c
                       | otherwise = findInp inp sc

 
checkUsability_ [] [] [(sc,res)]             = [(sc,res)]
checkUsability_ ((n,cs):ccs) [] [(sc,res)]   = [(sc,res)] -- if lc at j is empty then re-use
checkUsability_ [] ((n1,cs1):scs) [(sc,res)] = []         -- if cc at j is empty then don't re-use
checkUsability_ ((n,cs):ccs) ((n1,cs1):scs) [(sc,res)] 
                                             | and $ condCheck ((n,cs):ccs) ((n1,cs1):scs) = [(sc,res)]
                                             | otherwise                                   = []
                                             
                                             

condCheck ((n,cs):ccs) [(n1,cs1)]     = [condCheck_ ((n,cs):ccs) (n1,cs1)]
condCheck ((n,cs):ccs) ((n1,cs1):scs) = condCheck_ ((n,cs):ccs) (n1,cs1) : condCheck ((n,cs):ccs) scs

condCheck_ [] (n1,cs1)                  = False
condCheck_ ((n,cs):ccs) (n1,cs1) 
                 | n1 == n && cs >= cs1 = True
                 | n1 == n && cs < cs1  = False
                 | otherwise            = condCheck_ ccs (n1,cs1)
                          


udt :: (Context, Result) -> State -> MemoL -> Start1 -> State
udt res mTable name (inp,dAtts)  
               = update mTable name (inp,dAtts) res 
               
update [] name (inp,dAtts) res                 = [(name,[((inp,dAtts), res)])]
update ((key, pairs):rest) name (inp,dAtts) res 
               | key == name = (key,my_merge (inp,dAtts)  res pairs):rest
               | otherwise   = ((key, pairs): update rest name (inp,dAtts) res )


my_merge (inp,dAtts)  res [] = [((inp,nub dAtts), res)]
my_merge (inp,dAtts)  res (((i,dA), es):rest) 
               |inp == i  = ((inp,nub dAtts{-nub (dA ++ dAtts)-}), res):rest 
               |otherwise = ((i,dA), es): my_merge (inp,dAtts) res rest 


pickResult ((c,r),t) = r


--------------- ************************* semantics of ATTRIBUTE GRAMMAR ************************* --------------------------

terminal syn semRules id inhAtts ((i,a),inp) c  
 = do  syn (S,id) [((S,id),semRules)] inhAtts ((i,[]),inp) c

nt :: NTType -> Id -> SeqType
nt fx idx id inhAtts semRules altFromSibs 
 = let groupRule'' id rules         = [rule | (ud,rule) <- rules, id == ud]
       ownInAtts                    = mapInherited (groupRule'' (I, idx) semRules)  altFromSibs inhAtts id 
   in fx idx ownInAtts 

parser :: SeqType -> [SemRule] -> Id -> InsAttVals -> M Foo
parser synRule semRules id inhAtts i c
 =      do
           s <- get
           let ((e,altFromSibs),d)     =
                let sRule                        = groupRule'' (S, LHS) semRules
                    ((l,newRes),st)              = unState ((synRule id inhAtts semRules altFromSibs) i c) s
                    groupRule'' id rules         = [rule | (ud,rule) <- rules, id == ud]
                in  ((l, mapSynthesize  sRule newRes inhAtts id),st)
           put d
           return (e,altFromSibs)
      
                          
                                       
                     
mapSynthesize []   res  downAtts id   = res
mapSynthesize sems res  downAtts id
 = let appSems' [] vals        = []
       appSems' (r:rules) vals 
        = let [((ud, id'), atts)] = r (vals, id) 
          in  [((ud, id), atts)] ++ appSems' rules vals  
   in  [(((st,inAtts), (en,appSems' sems (findAtts t))),[t]) 
       |(((st,inAtts), (en,synAtts)),[t]) <- res]
   

-- mapSem' [] _ _            = []
mapInherited sems res []       id    
 = let appSems [] vals        = []
       appSems (r:rules) vals = r (vals, id) ++ appSems rules vals 
   in  concat [appSems sems (findAtts t) | (((st,inAtts), (en,synAtts)),[t]) <- res]
   
mapInherited sems []  downAtts id    
 = let appSems [] vals        = []
       appSems (r:rules) vals = r (vals, id) ++ appSems rules vals   
   in  appSems sems downAtts -- concat ( map (appSems id sems) (group downAtts))

mapInherited sems res downAtts id    
 = let appSems [] vals        = []
       appSems (r:rules) vals = r (vals, id) ++ appSems rules vals   
   in  concat [appSems sems ((groupAtts downAtts) ++ synAtts ++ (findAtts t)) 
              | (((st,inAtts), (en,synAtts)),[t]) <- res]

--------------------------------------
gMax []        = []
gMax [(a,[b])] = [(a,[b])]

gMax [(a,[b]),(a1,[b1])] | getAVAL b >= getAVAL b1 = [(a,[b])]
                         | otherwise  = [(a1,[b1])]

gMax ((a,[b]):(a1,[b1]):rest) | getAVAL b >= getAVAL b1 = gMax ((a,[b]):rest)
                              | otherwise  = gMax ((a1,[b1]):rest)


groupAtts []                    = []
groupAtts [(a,b)]               = [(a,b)]
groupAtts [(a,b),(a1,b1)]       = [(a,b++b1)]
groupAtts ((a,b):(a1,b1):rest)  = (a,b++b1): groupAtts rest 

--------------------------------------
findAtts (Branch (t:ts))                 
 = findAtts t ++ findAtts (Branch ts)
findAtts (SubNode ((n,i),((s,v'),(e,v)))) =  v' ++ v
findAtts (Leaf (a,id))                    =  [] 
findAtts (Branch [])                      =  []

addAtts x y = x ++ y

--------------------------------------------------------


rule_i          = rule I
rule_s          = rule S

rule s_or_i typ oF pID isEq userFun listOfExp 
 = let formAtts  id spec =  (id, (forNode id . spec))
       forNode   id atts = [(id, atts)]
       resType           = userFun  listOfExp
   in  formAtts (s_or_i,pID) (setAtt (typ undefined) . resType)



---- **** -----

synthesized = valOf S
inherited   = valOf I

valOf :: SorI -> (a -> AttValue) -> Useless -> Id -> [(Instance, Atts)] ->  Id -> [AttValue]
valOf ud typ o_f x  ivs x' | x == LHS   = getAttVals (ud , x') ivs typ
                           | otherwise  = getAttVals (ud , x ) ivs typ

getAttVals :: Instance -> [(Instance, Atts)] -> (a -> AttValue) -> [AttValue]
getAttVals x ((i,v):ivs) typ =
 let typeCheck typ t = if (typ undefined) == t then True else False
     getAttVals_ typ (t:tvs) = if (typ undefined) == t then (t :getAttVals_ typ tvs)
                               else getAttVals_ typ tvs                
     getAttVals_ typ []      = [] -- ErrorVal {-- 100 --} "ERROR id found but no value"
 in  
     if(i == x) then getAttVals_ typ v else   getAttVals x ivs typ
getAttVals x [] typ          = [ErrorVal {-- 200 --} "ERROR no id"]

 
-------- ************************************** ------------

------------------------- user functions ------------------   
apply :: [(Instance,Atts)] -> Id -> ([(Instance,Atts)] -> Id -> AttValue) -> Int
apply  y i x   = getAVAL (x y i)
apply_ y i x   = getB_OP (x y i)

apply__ :: [(Instance,Atts)] -> Id -> ([(Instance,Atts)] -> Id -> AttValue) -> DisplayTree
apply__ y i x  = getRVAL (x y i)

applyMax  y i x   = getAVAL (foldr (getMax) (MaxVal 0) (x y i))
getMax    x   y   = MaxVal  (max (getAVAL x) (getAVAL y))

findMax      spec  = \(atts,i) -> 
                       MaxVal (foldr (max) 0 (map (applyMax atts i) spec))
convertRep   spec  = \(atts,i) -> 
                       RepVal (foldr (max) 0 (map (applyMax atts i) spec))

makeTree  (x:xs)     = \(atts,i) -> Res (B           (map (apply__ atts i) (x:xs)))

mt [a,b,c] = (B [a,b,c]) 
mt [a]     = (B [a])

----------- for arithmetic expr -----------------  
applyBiOp [e1,op,e2] 
 = \atts ->  VAL ((getAtts getB_OP atts op ) (getAtts getAVAL atts e1 ) (getAtts getAVAL atts e2))
getAtts f (y,i) x 
 = f (head (x y i))

----------- general copy ------------------------
copy      [b]     
 = \(atts,i) -> head (b atts i)
getTypVal ((a,b):abs) t | a undefined == t = b t
                        | otherwise        = getTypVal abs t


----------- for arithmetic expr -----------------  

toTree    [b]        = \(atts,i) -> Res (N ((map (apply atts i) [b])!!0))

                      


-- JUNK TEST -----



-------- MAIN ----------
--------------- EXAMPLE EX-SPEC FOR TREE-REPLACEMENT ----------------

start  = memoize Start 
      (parser 
       (nt tree T0)
       [
        rule_i RepVal OF T0  ISEQUALTO convertRep    [synthesized  MaxVal OF T0]
       ]
      )
      
tree   = memoize Tree 
        (   parser 
           (nt tree T1 *> nt tree T2 *> nt num T3)
       [ rule_s   MaxVal OF LHS  ISEQUALTO findMax [  synthesized  MaxVal OF T1, 
                                                 synthesized  MaxVal OF T2,
                                                 synthesized  MaxVal OF T3
                                               ],       
        rule_i   RepVal OF T1   ISEQUALTO convertRep    [inherited RepVal OF LHS],
        rule_i   RepVal OF T2   ISEQUALTO convertRep    [inherited RepVal OF LHS],
        rule_i   RepVal OF T3   ISEQUALTO convertRep    [inherited RepVal OF LHS]
       ]
 <|> 
            parser 
           (nt num N1)
           [ rule_i   RepVal OF N1   ISEQUALTO convertRep    [inherited    RepVal OF LHS],
             rule_s   MaxVal OF LHS  ISEQUALTO findMax       [synthesized  MaxVal OF N1]
           ]           


  )
  

num  = memoize Num
       (
        terminal (term "1") [MaxVal 1] <|> 
    terminal (term "2") [MaxVal 2] <|>
    terminal (term "3") [MaxVal 3] <|>  
    terminal (term "4") [MaxVal 4] <|>  
    terminal (term "5") [MaxVal 5]  
       )
------------------------------------------------ Arithmetic Expression ------------------------------------------------

eT 
 = memoize ET
   (
   parser
   (nt expr E1)
   [rule_s VAL OF LHS ISEQUALTO copy [synthesized VAL  OF E1]]
   )
 
expr = memoize Expr
       (
        parser
        (nt expr E1 *> nt op O1 *> nt expr E2)
        [rule_s VAL OF LHS ISEQUALTO applyBiOp [synthesized VAL  OF E1,
                                                synthesized B_OP OF O1,
                                                synthesized VAL  OF E2] 
        ]
         
        <|> 
        parser
        (nt num N1)
        [rule_s VAL OF LHS  ISEQUALTO copy [synthesized MaxVal OF N1]]
       )

op   = memoize Op
       (
    terminal (term "+") [B_OP (+)] <|> 
    terminal (term "-") [B_OP (-)] <|>  
    terminal (term "*") [B_OP (*)] <|>   
    terminal (term "/") [B_OP (div)]    
       )

------------------------------------------------ Arithmetic Expression ------------------------------------------------

----------- PrettyPrint ------------------------
po :: (PP' a) => (String -> IO ()) -> a -> IO ()
po act x = do
    stuff <- pp' x
    act $ render80 stuff
    
render80 = renderStyle (style{lineLength = 80})


class PP' a where
      pp' :: a -> IO Doc

instance PP' Doc where
      pp' c = return c

instance PP' Char where
      pp' c = return $ text $ show c
instance PP' AttValue where
      pp' (VAL i) = showio (VAL i) >>= return . text
      pp' (B_OP i)= showio (B_OP i) >>= return .text
      
      
instance PP' Int where
      pp' i = return $ text $ show i
    
instance PP' Id where
      pp' i = return $ text $ show i       

instance PP' a => PP' (Maybe a) where
      pp' Nothing  = return $ text "Nothing"
      pp' (Just x) = pp' x >>= (\y -> return $ parens $ text "Just" PPH.<+> y)

instance (PP' a, PP' b) => PP' (a,b) where
      -- pp' (a,b) = parens $ pp' a PPH.<+> text "->" PPH.<+> pp' b
      pp' (a,b) =  pp' a >>= \z -> (pp' b >>= (\y -> return $ z PPH.<+> text "->" PPH.<+> y))
instance PP' a => PP' [a] where
      pp' []     = return $ brackets $ text ""
      pp' (x:xs) = liftM sep $ (liftM2 (:)) (pp' x) (sequence [ pp' y | y <- xs ])
                      
instance (Show t) => PP' (Tree t) where
      -- pp' Empty             = text "{_}"
      pp' (Leaf x)          = return $ text "Leaf" PPH.<+> text (show x)
      pp' (Branch ts)       = liftM2 (PPH.<+>) (return $ text "Branch") (liftM brackets $ liftM sep $ liftM (punctuate comma) $ sequence $ map pp' ts)
      --pp' (SubNode (x,(s,e))) = return $ text "SubNode" PPH.<+> text (show x) PPH.<+> text (show (s,e)) 
      -- PPH.<+> pp' ts

{-TODO:
format :: Mtable -> Doc
format t
 = vcat
   [ text (show s) 
     $$                                          
     nest 3 (pp' [text "START at:" PPH.<+> pp' i PPH.<+>  text "; Inherited atts:" PPH.<+>  vcat [vcat [text (showID id0) PPH.<+> vcat [text (show ty0v0) |ty0v0 <-val0]|(id0,val0)<-inAt1]] PPH.<+>
                  text "" $$ vcat [{-    text "Inherited Atts. -" 
                                   
                                   $$   
                                       vcat [vcat [text (showID id1) PPH.<+> vcat [text (show ty1v1)  |ty1v1<-val1]|(id1,val1)<-(nub inAtt2)]] 
                                   
                                   $$-}  nest 3
                                   (   text "END at:" PPH.<+> pp' end                                    
                                   $$  text "Synthesized Atts. -"
                                   
                                   $$  vcat [vcat [text (showID id1) PPH.<+> vcat [text (show ty1v1)  |ty1v1<-val1]|(id1,val1)<-synAtts]] 
                                   
                                   $$  text "Packed Tree -"
                                   
                                   $$  pp' ts)
                                       |(((st,inAtt2),(end,synAtts)), ts)<-rs] 
                                   
                | ((i,inAt1),((cs,ct),rs)) <- sr ])
   
   
   | (s,sr) <- t ]
-}
   

showID (x,y) = show y -- only the instance


--- ** printing ony own atts ** ---
{-TODO:
formatAtts :: MemoL -> Mtable -> Doc
formatAtts key t
 = vcat
   [ text (show s) 
     $$                                          
     nest 3 (pp' [text "START at:" PPH.<+> pp' i PPH.<+>  text "; Inherited atts:" PPH.<+>  vcat [vcat [text (showID id0) PPH.<+> vcat [text (show ty0v0) |ty0v0 <-val0]|(id0,val0)<-inAt1]] PPH.<+>
                  text "" $$ vcat [nest 3
                                   (   text "END at:" PPH.<+> pp' end                                    
                                   $$  text "Synthesized Atts. -"
                                   
                                   $$  vcat [vcat [text (showID id1) PPH.<+> vcat [text (show ty1v1)  |ty1v1<-val1]|(id1,val1)<-synAtts]] 
                                   
                                   -- $$  text "Packed Tree -"
                                   
                                   {- $$  pp' ts -} )
                                       |(((st,inAtt2),(end,synAtts)), ts)<-rs] 
                                   
                | ((i,inAt1),((cs,ct),rs)) <- sr ])
   
   
   | (s,sr) <- t, s == key ]
-}
 
formatAttsFinalAlt :: MemoL -> Int -> State -> IO [Doc]
formatAttsFinalAlt  key e t  = 
    --return [pp' [vcat [(vcat [vcat [vcat [text (show ty1v1)  |ty1v1<-val1]
    sequence [(sequence [liftM vcat $ sequence [(liftM vcat $ sequence [liftM vcat $ sequence [liftM vcat $ sequence [liftM text (showio ty1v1) | ty1v1<-val1]
                            |(id1,val1)<-synAtts]] )
                            |(((st,inAtt2),(end,synAtts)), ts)<-rs, end == e]                 
        | ((i,inAt1),((cs,ct),rs)) <- sr ]) >>= pp' | (s,sr) <- t, s == key ]

{-formatAttsFinal  key t  = 
   [(pp' [vcat [(vcat [vcat [vcat [text (show ty1v1)  |ty1v1<-val1]|(id1,val1)<-synAtts]] )|(((st,inAtt2),(end,synAtts)), ts)<-rs] 
                                   
                | ((i,inAt1),((cs,ct),rs)) <- sr ]) | (s,sr) <- t, s == key ]-}
                    
-- *************** for printing the fist element of the return pair ***************
{-TODO:
formatForFst ::Result -> Doc
formatForFst res = vcat 
                 -- [text (show ty0v0) |ty0v0 <-val0]|(id0,val0)<-inAt1]] PPH.PPH.<+> text "" $$ vcat 
                 
                 [   text "START at:" PPH.<+> pp' st 
                 
                 {-
                 $$  text "Inherited Atts. -" 
                 $$  vcat [vcat [text (showID id1) PPH.<+> vcat [text (show ty1v1)  |ty1v1<-val1]|(id1,val1)<-inAtt2]] 
                 -}
                 
                 PPH.<+> text "END at:" PPH.<+> pp' end 
                 $$  nest 3 
                     (text "Synthesized Atts. of"
                 
                 PPH.<+> vcat [vcat [text (showID id1) PPH.<+> text "::" PPH.<+> vcat [text (show ty1v1)  |ty1v1<-val1]|(id1,val1)<-synAtts]]
                 
                 $$  pp' ts)
                    |(((st,inAtt2),(end,synAtts)), ts)<-res] 


-}
-- *************** for printing the fist element of the return pair ***************

----------- PrettyPrint --------------------------

