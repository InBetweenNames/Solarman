{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module XSaiga.AGParser2 where
import Prelude hiding ((*>))
import Data.List
import Data.Maybe
import qualified Data.Text as T
import XSaiga.TypeAg2
import Control.Monad
import Control.Applicative hiding ((<|>), (*>))
import Control.Monad.State.Lazy



---- ************************************ -----------------------

                
data SorI     = S | I |NILL  deriving (Eq,Ord,Show, Enum) 
                -- Synthesized or inherited or NILL

type Instance = (SorI, Id) 
                -- uniquely determines synthesized or inherited attribute for an NT
                
data Useless  = OF|ISEQUALTO  deriving (Show, Eq)
                -- for decorating the semantic rules
       
type Atts     = [AttValue] -- [(AttType, AttValue)]

type InsAttVals = [(Instance,Atts)]

type Start    = ((Int,InsAttVals), [T.Text])

type Context  = ([MemoL],[(Int,[(MemoL, Int)])])

--These are the same??
type Start1   = (Int, InsAttVals)
type End      = (Int, InsAttVals)

data Tree = Leaf (MemoL,Instance)
            | SubNode ((MemoL, Instance), (Start1,End))
            | Branch [Tree] 
              deriving (Eq)

type Result   = [((Start1, End),[Tree])]

--Also called an "MTable" or "State" originally
type MemoTable = [(MemoL ,[(Start1,(Context,Result))])] 

type M = Start -> Context -> State MemoTable (Context, Result)

type NTType  = Id -> InsAttVals -> M

type SemRule = (Instance, (InsAttVals, Id) -> InsAttVals)

type SeqType = Id -> InsAttVals -> [SemRule] -> Result -> M

------------


--------------- ******************************************** ---------------------------

(<|>) :: NTType -> NTType -> NTType
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
empty :: InsAttVals -> M
empty atts (x,_) l = return (empty_cuts,[((x,(fst x,atts)), [Leaf (Emp, (NILL,O0))])])

        
memoize name f id downAtts ((inp,dAtts),dInput) context 
 = do s <- get        
      case (lookupT name inp (snd context) s) of   
       Just lRes -> do let lookUpRes = addNode name (S, id) (inp,downAtts) (snd lRes) 
                       return (fst lRes,lookUpRes)
       Nothing
           | funccount (snd context) inp name > (length dInput) - inp + 1
               -> return (([name],[]),[])
           | otherwise -> do let iC = ([],(addNT name inp $ snd context))
                             (l,newRes) <- f id downAtts ((inp,dAtts),dInput) iC 
                   -- let ((l,newRes),s1) = unMemoTable (f id downAtts (inp,dAtts) iC) s
                             let l1          = makeContext (fst l) (findContext inp (snd context))
                             s1             <- get
                             let udtTab      = udt (l1,newRes) name (inp,downAtts) s1 
                             let newFoundRes = addNode name (S, id) (inp,downAtts) newRes
                             put udtTab
                             return ( l1 ,newFoundRes)

findWithFst key    = find ((== key) . fst)

filterWithFst key  = filter ((== key) . fst)

replaceSnd key def f list
  = before ++ replaceFirst replacePoint
  where
    (before, replacePoint) = break ((== key) . fst) list
    replaceFirst [] = [(key, def)]
    replaceFirst ((a, b):nc) = (a, f b):nc                           

--TODO: Should this return all matches?  why return a list?
findContext inp = maybeToList . findWithFst inp

funccount list inp name = fromMaybe 0 $ do
  (_, funcp) <- findWithFst inp list
  (_, fc)    <- findWithFst name funcp
  return fc

makeContext [] _          = empty_cuts
makeContext rs []         = (rs, [])
makeContext rs [(st,ncs)] = (rs, [(st, concatMap (flip filterWithFst ncs) rs)])

--Merged with incContext
addNT name inp = replaceSnd inp [(name,1)] $ replaceSnd name 1 (+1)

addNode name id (s',dA) [] = []  

addNode name id (s',dA)  oldResult -- ((((s,newIh),(e,atts)),t):rs)  
 = let res     = packAmb oldResult
       newSh x = [ ((sOri, snd id),attVal)| ((sOri, idOld),attVal)<- x]
   in  [(((s,newIh),(e, newSh atts)),[SubNode ((name, id),((s,nub dA),(e, newSh atts)))]) 
       | (((s,newIh),(e,atts)),t) <- oldResult]

packAmb [] = []
packAmb [x] = [x]
packAmb (x:y:ys) = if isEq x y then packAmb $ (packer x y):ys else x:packAmb (y:ys)
  where
  packer ((s1, e1), t1) ((s2, e2), t2) = ((s2, (fst e2, groupAtts (snd e1 ++ snd e2))), t1 ++ t2)
  isEq (((s1,_),(e1,_)),_) (((s2,_),(e2,_)),_) = s1 == s2 && e1 == e2

lookupT name inp context mTable
  | null res_in_table = Nothing 
  | otherwise          = checkUsability inp context (lookupRes inp (res_in_table !! 0)) 
  where 
    res_in_table = map snd $ filterWithFst name mTable

lookupRes inp res_in_table = find (\((i,_), _) -> i == inp) res_in_table >>= return . snd

checkUsability inp context res = res >>= (\x@((re,sc),res) -> if null re then Just x else checkUsability_ (findInp inp context) (findInp inp sc) x)
  where
  --Want Nothing to be the case if list is empty or the inp could not be found
  findInp inp sc = findWithFst inp sc >>= (empty . snd)
  empty [] = Nothing
  empty x  = Just x
  checkUsability_  _       Nothing scres   = Just scres -- if lc at j is empty then re-use
  checkUsability_ Nothing  _       _       = Nothing         -- if cc at j is empty then don't re-use
  checkUsability_ (Just ccs) (Just scs) scres  | and $ condCheck ccs scs = Just scres
                                               | otherwise = Nothing
  condCheck ccs = map (condCheck_ ccs)
  condCheck_ ccs (n1, cs1) = maybe False (\(_, cs) -> cs >= cs1) $ findWithFst n1 ccs 

{-nub (dA ++ dAtts)-}
--udt == "update table"?
udt :: (Context, Result) -> MemoL -> Start1 -> MemoTable -> MemoTable
udt res name (inp, dAtts) = replaceSnd name [((inp,dAtts),res)] (my_merge (inp,nub dAtts) res)

replaceSndG key def list
  = before ++ replaceFirst replacePoint
  where
    (before, replacePoint) = break ((== key) . fst . fst) list
    replaceFirst [] = [def]
    replaceFirst ((a, b):nc) = def:nc                           

my_merge (inp,ndAtts) res = replaceSndG inp ((inp,ndAtts),res) 

{-
my_merge (inp,ndAtts) res [] = [((inp,ndAtts), res)]
my_merge (inp,ndAtts) res (((i,dA), es):rest) 
               |inp == i  = ((inp,ndAtts), res):rest 
               |otherwise = ((i,dA), es): my_merge (inp,ndAtts) res rest 
-}

--------------- ************************* semantics of ATTRIBUTE GRAMMAR ************************* --------------------------

--                               atts          iatts      Context was "l"
terminal :: T.Text -> Atts -> Id -> InsAttVals -> M
terminal str semRules id _ ((i,a),inp)
 = (term str) ((i,[]),inp)
    where
    inst = (S, id)
    atts = [(inst,semRules)]
    term :: T.Text -> M
    term str ((r,a),dInp) _ 
     |r - 1 == length dInp       = return (empty_cuts,[])
     |dInp!!(r - 1) == str       = return (empty_cuts,[(((r,[]),(r+1,atts)),[Leaf (ALeaf str, inst)])])
     |otherwise                  = return (empty_cuts,[])  


nt :: NTType -> Id -> SeqType
nt fx idx id inhAtts semRules altFromSibs 
 = let groupRule'' inst       = map snd $ filter ((inst ==) . fst) semRules
       ownInheritedAtts       = mapInherited (groupRule'' (I, idx)) altFromSibs inhAtts id 
   in fx idx ownInheritedAtts 

parser :: SeqType -> [SemRule] -> Id -> InsAttVals -> M
parser synRule semRules id inhAtts i c
 =      do
           s <- get
           let ((e,altFromSibs),d)     =
                let sRule                        = groupRule'' (S, LHS) semRules
                    ((l,newRes),st)              = runState ((synRule id inhAtts semRules altFromSibs) i c) s
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
   

applySemantics vals id = concatMap (\rule -> rule (vals, id)) 

mapInherited sems res [] id    
  = concat [applySemantics (findAtts t) id sems | (((st,inAtts), (en,synAtts)),[t]) <- res]
   
mapInherited sems []  downAtts id    
  = applySemantics downAtts id sems -- concat ( map (appSems id sems) (group downAtts))

mapInherited sems res downAtts id    
  = concat [applySemantics ((groupAtts downAtts) ++ synAtts ++ (findAtts t)) id sems 
              | (((st,inAtts), (en,synAtts)),[t]) <- res]

groupAtts []                    = []
groupAtts [(a,b)]               = [(a,b)]
groupAtts [(a,b),(_,b1)]       = [(a,b++b1)]
groupAtts ((a,b):(_,b1):rest)  = (a,b++b1): groupAtts rest 

--------------------------------------
findAtts (Branch ts)                  = concatMap findAtts ts
findAtts (SubNode (_,((_,v'),(_,v)))) = v' ++ v
findAtts (Leaf _)                     = [] 

--------------------------------------------------------


rule_i          = rule I
rule_s          = rule S

--TODO: more undefined...
rule s_or_i typ oF pID isEq userFun listOfExp 
 = let formAtts  id spec =  (id, (forNode id . spec))
       forNode   id atts = [(id, atts)]
       resType           = userFun  listOfExp
   in  formAtts (s_or_i,pID) (setAtt (typ undefined) . resType)



---- **** -----

synthesized = valOf S
inherited   = valOf I

valOf :: SorI -> (a -> AttValue) -> Useless -> Id -> InsAttVals ->  Id -> Atts
valOf ud typ o_f x  ivs x' | x == LHS   = getAttVals (ud , x') ivs typ
                           | otherwise  = getAttVals (ud , x ) ivs typ

--TODO: Whoa!!!!  not cool! 
getAttVals :: Instance -> InsAttVals -> (a -> AttValue) -> Atts
getAttVals x ((i,v):ivs) typ =
 let getAttVals_ typ (t:tvs) = if (typ undefined) == t then (t :getAttVals_ typ tvs)
                               else getAttVals_ typ tvs                
     getAttVals_ typ []      = [] -- ErrorVal {-- 100 --} "ERROR id found but no value"
 in  
     if(i == x) then getAttVals_ typ v else   getAttVals x ivs typ
getAttVals x [] typ          = [ErrorVal {-- 200 --} "ERROR no id"]

 
-------- ************************************** ------------

------------------------- user functions ------------------   
apply :: InsAttVals -> Id -> (InsAttVals -> Id -> AttValue) -> Int
apply  y i x   = getAVAL (x y i)
apply_ y i x   = getB_OP (x y i)

apply__ :: InsAttVals -> Id -> (InsAttVals -> Id -> AttValue) -> DisplayTree
apply__ y i x  = getRVAL (x y i)

applyMax  y i x   = getAVAL (foldr getMax (MaxVal 0) (x y i))
getMax    x   y   = MaxVal  (max (getAVAL x) (getAVAL y))

findMax spec (atts,i) = MaxVal (foldr max 0 (map (applyMax atts i) spec))

convertRep spec (atts,i) = RepVal (foldr max 0 (map (applyMax atts i) spec))

makeTree (x:xs) (atts,i) = Res (B (map (apply__ atts i) (x:xs)))

mt [a,b,c] = (B [a,b,c]) 
mt [a]     = (B [a])

----------- for arithmetic expr -----------------  
applyBiOp [e1,op,e2] atts = VAL ((getAtts getB_OP atts op ) (getAtts getAVAL atts e1 ) (getAtts getAVAL atts e2))
getAtts f (y,i) x = f (head (x y i))

----------- general copy ------------------------
copy [b] (atts,i) = head (b atts i)
getTypVal ((a,b):abs) t | a undefined == t = b t
                        | otherwise        = getTypVal abs t


----------- for arithmetic expr -----------------  

toTree [b] (atts,i) = Res (N ((map (apply atts i) [b])!!0))

                      


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
        terminal "1" [MaxVal 1] <|> 
    terminal "2" [MaxVal 2] <|>
    terminal "3" [MaxVal 3] <|>  
    terminal "4" [MaxVal 4] <|>  
    terminal "5" [MaxVal 5]  
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
    terminal "+" [B_OP (+)] <|> 
    terminal "-" [B_OP (-)] <|>  
    terminal "*" [B_OP (*)] <|>   
    terminal "/" [B_OP div]    
       )
 
--The nastiest list comprehension I have ever seen in my life     
--The unformatted parse tree

attsFinalAlt :: MemoL -> Int -> MemoTable -> [[[[Atts]]]]
attsFinalAlt  key e t  =  [ [ [ map snd synAtts | ((_,(end,synAtts)), _)<-rs, end == e] | (_,(_,rs)) <- sr ] | (s,sr) <- t, s == key ]
              
--The unformatted flattened parse trees
formatAttsFinalAlt :: MemoL -> Int -> MemoTable -> Atts
formatAttsFinalAlt key e t =  concat $ concat $ concat $ concat $ attsFinalAlt key e t

                   
