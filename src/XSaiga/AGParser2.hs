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
import Control.Monad.State.Strict

import qualified Data.Map.Strict as Map

import Debug.Trace

{-
Finished improvements:
    * Use a map for the top level of the memo table
    * Cassini no longer refers to the wrong entry voyager_2
    * superterminals, needed for dictionaryless pnouns, cnouns, adjs
    * disambiguated grammar for years and locations

TODO Improvements:
    * Decouple TypeAg2 from AGParser2 (parameterize AttValue)
    * Find another way to find "equivalence" between AttValues.  See TypeAg2 where the annoying bit is with instance Eq
    * Move data Id to here from TypeAg2
    * Use a hashmap instead of a red/black tree map (requires hashable MemoL)
    * MemoL needs to be unique per memoize block, is this able to be generated automatically?
    * Move the example code to a new file (with the +-/* grammar), use the parameterized code to do this
    * Make a better syntax for the parser.  The list based syntax leaves a lot to be desired.
    * New name for the parser.  XSaiga-NG?
    * okay so memoized and unmemoized return the same type... but unmemoized never seems to work?  need type level correction
    *** Cnouns, pnouns should not need to be in dictionary.  should be able to "try" a terminal as a pnoun or cnoun as needed.
    *** in should be disambiguated for years and locations
    *** "or" and "and" should not be ambiguous except when used with each other.  for example
        "hall or phobos or deimos and hall or kuiper or galileo" should only be ambiguous around the "and":
        "(hall or (phobos or (deimos))) and (hall or (kuiper or (galileo)))"
        "hall or (phobos or ((deimos and hall) or (kuiper or galileo)))"
        in other words, "or" and "and" should go to the left, but should still be ambiguous with respect to each other
    *** would be nice to be able to tag a superterminal with the key used to tag it for presentation
        eg, "hall discovered" => (Pnoun_(hall)) discovered
            "which moons were discovered by hall in 1877" => which (Cnoun_(moons)) (were discovered [by (Pnoun_(hall)), in (Year_(1877))])

    "every" seems to be able to answer "no" style questions kind of


-}

---- ************************************ -----------------------

                
data SorI     = S | I | NILL  deriving (Eq,Ord,Show, Enum) 
                -- Synthesized or inherited or NILL

type Instance = (SorI, Id) 
                -- uniquely determines synthesized or inherited attribute for an NT
                
data Useless  = OF|ISEQUALTO  deriving (Show, Eq)
                -- for decorating the semantic rules
       
type Atts     = [AttValue] -- [(AttType, AttValue)]

type InsAttVals = [(Instance,Atts)]

type Start    = ((Int,InsAttVals), [T.Text]) --What purpose does this T.text serve?

type Context  = ([MemoL],[(Int,[(MemoL, Int)])])

--These are the same??
type Start1   = (Int, InsAttVals)
type End      = (Int, InsAttVals)

data Tree = Leaf (MemoL,Instance)
            | SubNode ((MemoL, Instance), (Start1,End))
            | Branch [Tree] 
              deriving (Eq, Show)

type Result   = [((Start1, End),[Tree])]

--Also called an "MTable" or "State" originally
type MemoTable = Map.Map MemoL [(Start1,(Context,Result))]

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

--a3 = [MemoL]
-- ((Int, b1), t a1) appears twice (appears to be Start)
-- a2 = MemoL
memoize :: MemoL -> (Id -> InsAttVals -> M) -> Id -> InsAttVals -> M
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

{-replaceSnd :: MemoL
    -> [(Start1,(Context, Result))]
    -> ([(Start1,(Context,Result))] -> [(Start1,(Context,Result))])
    -> [(MemoL,[(Start1,(Context,Result))])]
    -> [(MemoL,[(Start1,(Context,Result))])]
-}
replaceSnd key def f list
  = before ++ replaceFirst replacePoint
  where
    (before, replacePoint) = break ((== key) . fst) list
    replaceFirst [] = [(key, def)]
    replaceFirst ((a, b):nc) = (a, f b):nc 
    
{-replaceSnd' :: MemoL
    -> [(Start1,(Context, Result))]
    -> ([(Start1,(Context,Result))] -> [(Start1,(Context,Result))])
    -> MemoTable
    -> MemoTable
-}
replaceSnd' key def f map = Map.insertWith (\new_value -> \old_value -> f old_value) key def map 

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

lookupT :: MemoL -> Int -> [(Int,[(MemoL, Int)])] -> MemoTable -> Maybe (Context,Result)
lookupT name inp context mTable = do
    res_in_table <- Map.lookup name mTable
    checkUsability inp context (lookupRes inp res_in_table)

lookupRes :: Int -> [(Start1,(Context,Result))] -> Maybe (Context, Result)
lookupRes inp res_in_table = find (\((i,_), _) -> i == inp) res_in_table >>= return . snd

checkUsability :: Int -> [(Int,[(MemoL, Int)])] -> Maybe (Context, Result) -> Maybe (Context, Result)
checkUsability inp context res = res >>= (\x@((re,sc),res) -> if null re then Just x else checkUsability_ (findInp inp context) (findInp inp sc) x)
  where
  --Want Nothing to be the case if list is empty or the inp could not be found
  findInp :: Int -> [(Int,[(MemoL, Int)])] -> Maybe [(MemoL, Int)]
  findInp inp sc = findWithFst inp sc >>= (empty . snd)
  empty :: [(MemoL, Int)] -> Maybe [(MemoL, Int)]
  empty [] = Nothing
  empty x  = Just x
  checkUsability_ :: Maybe [(MemoL, Int)] -> Maybe [(MemoL, Int)] -> (Context, Result) -> Maybe (Context, Result)
  checkUsability_  _       Nothing scres   = Just scres -- if lc at j is empty then re-use
  checkUsability_ Nothing  _       _       = Nothing    -- if cc at j is empty then don't re-use
  checkUsability_ (Just ccs) (Just scs) scres  | and $ condCheck ccs scs = Just scres
                                               | otherwise = Nothing
  condCheck :: [(MemoL, Int)] -> [(MemoL, Int)] -> [Bool]
  condCheck ccs = map (condCheck_ ccs)
  condCheck_ :: [(MemoL, Int)] -> (MemoL, Int) -> Bool
  condCheck_ ccs (n1, cs1) = maybe False (\(_, cs) -> cs >= cs1) $ findWithFst n1 ccs 

{-nub (dA ++ dAtts)-}
--udt == "update table"?
udt :: (Context, Result) -> MemoL -> Start1 -> MemoTable -> MemoTable
udt res name (inp, dAtts) = replaceSnd' name [((inp,dAtts),res)] (my_merge (inp,nub dAtts) res)

replaceSndG :: Int -> (Start1, (Context, Result)) -> [(Start1, (Context, Result))] -> [(Start1, (Context, Result))]
replaceSndG key def list
  = before ++ replaceFirst replacePoint
  where
    (before, replacePoint) = break ((== key) . fst . fst) list
    replaceFirst [] = [def]
    replaceFirst ((a, b):nc) = def:nc                           

my_merge :: Start1 -> (Context, Result) -> [(Start1, (Context, Result))] -> [(Start1, (Context, Result))]
my_merge (inp,ndAtts) res = replaceSndG inp ((inp,ndAtts),res) 

{-
my_merge (inp,ndAtts) res [] = [((inp,ndAtts), res)]
my_merge (inp,ndAtts) res (((i,dA), es):rest) 
               |inp == i  = ((inp,ndAtts), res):rest 
               |otherwise = ((i,dA), es): my_merge (inp,ndAtts) res rest 
-}

--------------- ************************* semantics of ATTRIBUTE GRAMMAR ************************* --------------------------

--                               atts          iatts      Context was "l"

--superterminal is like terminal, but does not require all possibilities to be enumerated.
--use "f" to determine if a token may make a suitable terminal
--Nothing -> means throw it out
--Just a -> use it
superterminal :: MemoL -> (T.Text -> Maybe Atts) -> Id -> InsAttVals -> M
superterminal key f = memoize key (superterminal' f)

superterminal' :: (T.Text -> Maybe Atts) -> Id -> InsAttVals -> M
superterminal' f id _ q@((r,a),dInp)
 = term q --trace ((show $ T.intercalate " " dInp) ++ show q) $ term q
    where
    inst = (S, id)
    instAttVals atts =  [(inst,atts)]
    term :: M --NTType
    term ((r,a),dInp) _ = if r - 1 == length dInp then return (empty_cuts,[]) else let str = dInp!!(r - 1) in case f str of
        Nothing -> return (empty_cuts, [])
        Just atts -> return (empty_cuts,[(((r,[]),(r+1,instAttVals atts)),[Leaf (ALeaf str, inst)])])

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

attsFinalAlt :: MemoL -> Int -> MemoTable -> [[[Atts]]]
attsFinalAlt  key e t  =  maybe [] allTrees (Map.lookup key t)
    where
        allTrees sr = [ [ map snd synAtts | ((_,(end,synAtts)), _)<-rs, end == e] | (_,(_,rs)) <- sr ]
              
--Using a start, end, and memoization key, locate all valid parses that match.  In the case of ambiguity, there may be more than one result.
--These three conditions are sufficient to guarantee the result is unique and valid.
lookupTable :: MemoL -> Int -> Int -> MemoTable -> [Tree]
lookupTable key start end t = maybe [] allTrees (Map.lookup key t)
    where allTrees sr = concat $ concat $ [ [ tree | ((_,(_end, _)), tree) <- results, end == _end] | ((_start, _), (_, results)) <- sr, start == _start]

--The memo table itself must be "unravelled" to reveal the parse tree.  If there is only one valid parse, this is easy.
--If there are multiple valid parses due to ambiguity, then we must return all valid trees.
--But the problem is that the originator for a particular SubNode or Branch is lost during parsing -- only the differing AttValue is kept.
--This is because this information isn't necessary to get a valid parse.
--However, we'd sure like to have it.  Note that in ambiguity, there will be cases where the start, end, and key match and only the AttValue differs.
--But this is not helpful to us, because we can't meaningfully compare those.
--It should be possible to modify the code somehow to preserve this, but I'm not sure how, as it's very hard to read.
--So, instead, produce a reduced memo table using eqAmb where ambiguous parses are reduced to 1 parse, discarding AttValue.
--Then walk the tree recursively, branching off whenever the syntax tree starts to actually diverge to ensure we get the relevant info.
--In other words, we have to just produce all unique trees we can that are valid parses and hope to god that we can recover the AttValues later somehow,
--because there's no direct reference to where a particular AttValue came from.  If there are 3 possible AttValues from 3 ambiguous parses that could have yielded
--an AttValue, then we have no way of knowing which one it actually was because start, end, and key will match for *all* of them.  Ouch.
--NOTE: a Leaf is just a terminal, a Branch is a sequence of non-terminals and terminals, and a SubNode corresponds to a reference to a terminal or non-terminal.
--Actually, I think that Branch may only have SubNodes, which makes it essentially a representation of a non-terminal.  This may have to do with the compact memory representation.
--Anyway, only bother to put brackets in on Branches, as this is where the syntax tree does the real "splitting" it seems.
--To "unpack" a SubNode, you must traverse the MemoTable and find the entry corresponding to the start, end, and key

--But there is a problem with this.
--TODO: it looks like traversing this way will guarantee that the AttValues retrieved later during formatAttsFinalAlt will be in the same order as the returned trees
--(so that the trees will map correspondingly to their attvalues).  But I haven't proved this at all!  It would be sure nice if the tree were embedded somehow in the actual parse.
data SyntaxTree = SyntaxTreeNT [SyntaxTree] | SyntaxTreeT T.Text deriving (Show)

findAllParseTrees t (Leaf (ALeaf str, _)) = [SyntaxTreeT str]
--SubNodes introduce ambiguity?
findAllParseTrees t (SubNode ((key, _), ((_start, _), (_end, _)))) = let trees = nubBy eqAmb (lookupTable key _start _end t) in concatMap (findAllParseTrees t) trees
--nubBy eqAmb necessary for Branch?  Ambiguity?
findAllParseTrees t (Branch tree) = map SyntaxTreeNT $ sequence $ map (findAllParseTrees t) tree

findAllParseTreesT' key end t = zip sems trees
    where
        sems = formatAttsFinalAlt key end t
        trees = concat $ sequence $ map (findAllParseTrees t) $ nubBy eqAmb (lookupTable key 1 end t)

findAllParseTreesFormatted formatTree key end t = map (\(x, y) -> (x, formatTree y)) $ findAllParseTreesT' key end t

findAllParseTreesFormatted' = findAllParseTreesFormatted syntaxTreeToLinearGeneric'

--Compare trees up until Atts.  We're looking for a syntactic match and cannot compare atts meaningfully.
eqAmb :: Tree -> Tree -> Bool
eqAmb (Leaf x) (Leaf y) = x == y
eqAmb (SubNode x) (SubNode y) = x == y
eqAmb (Branch []) (Branch []) = True
eqAmb (Branch []) _ = False
eqAmb _ (Branch []) = False
eqAmb (Branch (x:xs)) (Branch (y:ys)) = eqAmb x y && eqAmb (Branch xs) (Branch ys)
eqAmb _ _ = False

--the rules:
--brackets are collapsed
--expressions are separated by spaces

shouldSpaceGeneric x y = isWord x && isWord y || isWord x && isOpeningBracket y || isClosingBracket x && isWord y || isClosingBracket x && isOpeningBracket y
        where
            isBracket x = x == "(" || x == ")"
            isClosingBracket x = x == ")"
            isOpeningBracket x = x == "("
            isWord x = not $ isBracket x

intercalateBracketsGeneric [] = ""
intercalateBracketsGeneric (x:y:xs) | shouldSpaceGeneric x y = x `T.append` " " `T.append` intercalateBracketsGeneric (y:xs)
intercalateBracketsGeneric (x:xs) = x `T.append` intercalateBracketsGeneric xs

syntaxTreeToLinearGeneric :: SyntaxTree -> [T.Text]
syntaxTreeToLinearGeneric (SyntaxTreeT x) = [x]
syntaxTreeToLinearGeneric (SyntaxTreeNT ts) = ["("] ++ concatMap syntaxTreeToLinearGeneric ts ++ [")"]

syntaxTreeToLinearGeneric' :: SyntaxTree -> T.Text
syntaxTreeToLinearGeneric' (SyntaxTreeT x) = x
syntaxTreeToLinearGeneric' (SyntaxTreeNT ts) = intercalateBracketsGeneric $ concatMap syntaxTreeToLinearGeneric ts

--WIP
diffTree :: SyntaxTree -> SyntaxTree -> [SyntaxTree]
diffTree (SyntaxTreeT t1) a@(SyntaxTreeT t2) = if t1 == t2 then [] else [a]
diffTree (SyntaxTreeT t1) a@(SyntaxTreeNT ts2) = [a]
diffTree (SyntaxTreeNT ts1) a@(SyntaxTreeT t2) = [a]
diffTree (SyntaxTreeNT []) (SyntaxTreeNT xs)= xs
diffTree (SyntaxTreeNT xs) (SyntaxTreeNT []) = []
diffTree (SyntaxTreeNT (t1:ts1)) a@(SyntaxTreeNT (t2:ts2)) = diffTree t1 t2 ++ diffTree (SyntaxTreeNT ts1) (SyntaxTreeNT ts2)
--need edit distance...

--a moon spins = SyntaxTreeNT [SyntaxTreeT "a", SyntaxTreeT "moon", SyntaxTreeT "spins"]
--discover (a moon) = SyntaxTreeNT [SyntaxTreeT "discover", SyntaxTree NT[SyntaxTreeT "a", SyntaxTree "moon"]]

--The unformatted flattened parse trees
formatAttsFinalAlt :: MemoL -> Int -> MemoTable -> Atts
formatAttsFinalAlt key e t =  concat $ concat $ concat $ attsFinalAlt key e t

meaning_of p dInp key
 = let dInput     = T.words dInp
       appParser  = runState (p T0 [] ((1,[]), dInput) ([],[])) Map.empty
       upperBound = (length dInput) + 1
   in  formFinal key upperBound (snd $ appParser)

meaning_of_ p dInp key
 = let dInput     = T.words dInp
       appParser  = runState (p T0 [] ((1,[]), dInput) ([],[]))  Map.empty
       upperBound = (length dInput) + 1
   in  (snd $ appParser)

formAtts key ePoint t = maybe [] allAtts $ Map.lookup key t
        where allAtts sr = concat $ concat $ concat [[[  val1 |(id1,val1)<-synAtts]
                                                    |(((st,inAtt2),(end,synAtts)), ts)<-rs, st == 1 && end == ePoint]
                                                    |((i,inAt1),((cs,ct),rs)) <- sr ]

formFinal key ePoint t = maybe [] final $ Map.lookup key t
    where final sr = concat $ concat $ concat $ [[[  val1 |(id1,val1)<-synAtts]
                                                    |(((st,inAtt2),(end,synAtts)), ts)<-rs, st == 1 && end == ePoint]
                                                    |((i,inAt1),((cs,ct),rs)) <- sr ]

--test1 p p_ inp = do putStr  $ render80 $ format{-Atts p_-} $ snd $ runState (p T0 [] ((1,[]),words inp) ([],[])) []
test :: (Start -> Context -> State MemoTable (Context, Result))
    -> [T.Text]
    -> ((Context, Result), MemoTable)
test p input = runState (p ((1,[]),input) ([],[])) Map.empty

--formatParseIO = mapM id . map showio . parse

findStart st ((s,ss):rest) | s == st   = [(s,ss)]
                           | otherwise = findStart st rest
findStart st []                        = []