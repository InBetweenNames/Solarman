{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}

module XSaiga.AGParser2 where
import Prelude hiding ((*>))
import Data.List
import Data.Maybe
import qualified Data.Text as T
import XSaiga.TypeAg2
import Control.Monad
import Control.Applicative hiding ((<|>), (*>))
import Control.Monad.State.Strict
import Data.Constructors.EqC

--import qualified Data.Map.Strict as Map
import qualified Data.HashMap.Lazy as HashMap.Lazy
import qualified Data.Map.Strict as Map --should probably just be a strict map
import qualified Data.Set as Set

import qualified Data.Foldable as Fold
import qualified Data.Vector as Vector

import Debug.Trace

{-
Finished improvements:
    * Use a map for the top level of the memo table
    * Cassini no longer refers to the wrong entry voyager_2
    * superterminals, needed for dictionaryless pnouns, cnouns, adjs
    * disambiguated grammar for years and locations
    * Decouple TypeAg2 from AGParser2 (parameterize AttValue)
    * Using a vector of T.Text instead of a list for faster indexing (O(1))
    * Using a set for the set of MemoL's instead of a list acting as a set
    * Using maps for Context
    * Merging Map of Maps into just a singular Map keyed over (Int,MemoL)
    * More types explicitly named and written out
    * Simplified logic for checking contexts
    * Add more type annotations that explain what is going on a bit better
    * Remove pointless atts attached to the input string.  They were ignored/not used at all anyway!
    * terminalSet for efficient terminals
    * superterminals to "try" a terminal of any sequence of characters
    * Replace [Tree] with Tree in Result

TODO Improvements:

    * Alternative representation for table: instead of (Start1, End), use ((Start :: Int,End :: Int),[(InhAtts,SynAtts)]) and key on (Int,Int).  Each pair of atts is a valid parse.
    * Simplified memotable list of results: it no longer stores inherited atts (Start1) beside the set of results, it just stores the start position (Int)
    * multi-terminal: a terminal consisting of N or more tokens.  Like "refractor telescope 1" or "asaph hall"
    * Better representation for attributes.  In particular, synthesized and inherited atts should be separated, and they should have names.
      Those names should be independent of the type.  Example. Rule.value = "100", Rule.size = 3 (for synthesized).  These names should not be hardcoded!
      Also, if no duplicate instances are permitted, then we should make it a map and get rid of the lists.  Note that whatever is done, lazy evaluation must be permitted.
    * Elimination of all space leaks
    * Should we consider using HashMap?  Seems to do better in benchmarks
    * Use Unboxed/Storable Vector/HashMap?
    * Find another way to signal an error for slot mismatch (S1/S3 given and S1/S2 requested)
    * Find another way to find "equivalence" between AttValues.  See TypeAg2 where the annoying bit is with instance Eq
    * Move data Id to here from TypeAg2
    * MemoL needs to be unique per memoize block, is this able to be generated automatically?
    * Move the example code to a new file (with the +-/* grammar), use the parameterized code to do this
    * Make a better syntax for the parser.  The list based syntax leaves a lot to be desired.
    * New name for the parser.  XSaiga-NG?
    * Decouple error reporting from AttValue
    * okay so memoized and unmemoized return the same type... but unmemoized never seems to work?  need type level correction
        * REASON: terminals return Leafs, not SubNodes.  it's the memoization that creates the SubNode.  So terminal by itself, unmemoized, won't work due to findAtts only looking at SubNodes.
    * Need to find all instances where empty lists are used to denote "not found" and singletons are used to denote "found" and replace with Maybe
    *** "or" and "and" should not be ambiguous except when used with each other.  for example
        "hall or phobos or deimos and hall or kuiper or galileo" should only be ambiguous around the "and":
        "(hall or (phobos or (deimos))) and (hall or (kuiper or (galileo)))"
        "hall or (phobos or ((deimos and hall) or (kuiper or galileo)))"
        in other words, "or" and "and" should go to the left, but should still be ambiguous with respect to each other
    *** would be nice to be able to tag a superterminal with the key used to tag it for presentation
        eg, "hall discovered" => (Pnoun_(hall)) discovered
            "which moons were discovered by hall in 1877" => which (Cnoun_(moons)) (were discovered [by (Pnoun_(hall)), in (Year_(1877))])
    *** which x discovered which y using a telescope

    "every" seems to be able to answer "no" style questions kind of


-}

---- ************************************ -----------------------

--TODO: NILL seems to be only used for "empty" which is a function that seems useful only for building recognizers?
--and it seems like "empty" recognizes any terminal anyway?  what is the use of that?
data SorI     = S | I | NILL  deriving (Eq,Ord,Show, Enum)
                -- Synthesized or inherited or NILL

type Instance = (SorI, Id)
                -- uniquely determines synthesized or inherited attribute for an NT

data Useless  = OF|ISEQUALTO  deriving (Show, Eq)
                -- for decorating the semantic rules

type Error = T.Text

type Atts att   = [att] -- [(AttType, AttValue)]

--TODO: make this a map!  these are supposed to be unique per parse.  Note that it is possible to get the same instance twice (e.g. multiple rule_s)
--This is supposed to represent different synthesized attributes for the same production.
type InsAttVals att = [(Instance,Atts att)]

type Start     = (Int, Vector.Vector T.Text) --What purpose does this Int serve?

--NOTE: may want to use HashMap instead: https://github.com/haskell-perf/dictionaries#from-list-int-keys
type ContextMap = Map.Map (Int,MemoL) Int
type Context  = (Set.Set MemoL, ContextMap)

--These are the same??
type Start1 att  = (Int, InsAttVals att)
type End att    = Start1 att

type StartEnd att = ((Int,Int),(InsAttVals att, InsAttVals att)) --NOTE: want both start AND end recorded because need it later when computing parse tree

data Tree att = Leaf (MemoL,Instance)
            | SubNode ((MemoL, Instance), StartEnd att)
            | Branch [Tree att]
              deriving (Show)

type Result att  = [(StartEnd att,Tree att)] --TODO: how is the StartEnd here different than the one in the Tree?

type ParseResult att = (Context,Result att)

--Also called an "MTable" or "State" originally
type MemoTable att = Map.Map (MemoL,Int) (ParseResult att) -- The "Int" corresponds to the Int of a Start1, that is the starting position

type M att = Start -> Context -> State (MemoTable att) (ParseResult att)

--NOTE: the parser itself also returns this.  Parser sems to play the role of attaching semantics to syntactic rules.  Without parser, it just is a recognizer.
--TODO: should these be labeled separately, even if they have the same type?
--TODO: is nttype <|> parser (nt blah T0) [rules] valid?  would it work the same as if the atts were just copied?
--NOTE: this was called AltType before
type NTType att = Id -> (InsAttVals att) -> (M att)

type SemRule att = (Instance, (InsAttVals att, Id) -> att)

type SeqType att = Id -> (InsAttVals att) -> [SemRule att] -> (Result att) -> (M att)

------------


--------------- ******************************************** ---------------------------
--Originally called AltType?
(<|>) :: NTType att -> NTType att -> NTType att
(p <|> q) idx inhx (i,inp) c
 = do (l1,m) <- p idx inhx (i,inp) c
      (l2,n) <- q idx inhx (i,inp) c
      return ((Set.union (fst l1) (fst l2),Map.empty), (m ++ n))

--TODO: space leak here?
--TODO: change lists of attributes to vectors or something that's not a list
--------------------------------------------------------
(*>) :: SeqType att -> SeqType att -> SeqType att
(p *> q) idx inhx semx resx (i,inp) cc
 = do (l,m) <- p idx inhx semx resx (i,inp) cc
      let passCtxt  e
           = if e == i then cc
             else empty_cuts
          mergeCuts e prev new
           = if e == i
             then (Set.union prev new)
             else prev

          combiner q cc r (l2,n) = do
                (l1,m) <- q `add_P` (r,cc)
                return ((Set.union (l1) (fst l2),Map.empty),(m ++ n))

          apply_to_all q rs l cc = Fold.foldrM (combiner q cc) (((fst l,Map.empty),[])) rs

          q `add_P` (rp,cc)
            = do let e  = pickEnd rp
                 (l1,m) <- (q idx inhx semx resx (e,inp) (passCtxt e))
                 return ((mergeCuts e (fst l) (fst l1)),(addP m rp))

      (apply_to_all q m l cc) -- CHANGE cc HERE

--TODO: Make the below readable
pickEnd :: (StartEnd att,Tree att) -> Int
pickEnd (((s,e),(iA,a)),t) = e --NOTE: This is a Start1's Int

--NOTE: addP = addParse?
addP :: Result att -> (StartEnd att, Tree att) -> Result att
addP  [] ((s1,e1),t1)                    = []
addP  ((((s2,e2),(inA2,synA2)),t2):restQ) (((s1,e1),(inA1,synA1)),t1)
 = (((s1,e2),([],[])),  a_P_Q_branch) : addP  restQ (((s1,e1),(inA1,synA1)),t1)

   where
   a_P_Q_branch = addToBranch (((s2,e2),(inA2,synA2)),t2)  (((s1,e1),(inA1,synA1)),t1)

-------- ************* ---------------
--NOTE: it seems the top level Tree is simply a singleton?
addToBranch :: (StartEnd att, Tree att) -> (StartEnd att, Tree att) -> Tree att
addToBranch (q1,((SubNode (name2,q))))
            (p1,((SubNode (name1,p))))
                            = Branch [(SubNode (name1,p)),(SubNode (name2,q))]

addToBranch (q1,((Branch t2)))
            (p1,((Branch t1)))
                            = Branch (t1++t2)

addToBranch (q1,((Branch t2)))
            (p1,((SubNode (name1,p))))
                            = Branch ((SubNode (name1,p)):t2)

addToBranch (q1,((SubNode (name2,q))))
            (p1,((Branch t1)))
                            = Branch (t1++[(SubNode (name2,q))])


addToBranch (q1,((SubNode (name2,q))))
            (p1,Leaf (x,i))
                            = Branch [(SubNode ((x,i) ,p1)),(SubNode (name2,q))]
addToBranch (q1,Leaf (x,i))
            (p1,((SubNode (name1,p))))
                            = Branch [(SubNode (name1,p)),(SubNode ((x,i),q1))]
addToBranch (q1,((Branch t2)))
            (p1,Leaf (x,i))
                            = Branch ((SubNode ((x,i),p1)):t2)
addToBranch (q1,Leaf (x,i))
            (p1,((Branch t1)))
                            = Branch (t1++[(SubNode ((x,i),q1))])
addToBranch (q1,Leaf (x2,i2))
            (p1,Leaf (x1,i1))                         = Branch [(SubNode ((x1,i1),p1)),(SubNode ((x2,i2),q1))]

-------- ************* ---------------


empty_cuts :: Context
empty_cuts = (Set.empty,Map.empty)
empty :: InsAttVals att -> M att
empty atts (x,_) l = return (empty_cuts,[(((x,x),([],atts)), Leaf (Emp, (NILL,O0)))])


--NOTE: this is used like: test (question T0 []) (vWords), so id and downAtts come from that!  The start, context from test come later.
--      This evalutes to runState (question T0 [] ((1,[]),input) empty_cuts) Map.empty.
--      Meaning id = T0, downAtts = [], inp = 1, dAtts = [], dInput = input, context = empty_cuts.
--TODO: what is the difference between downAtts and dAtts?
--TODO: remove dAtts entirely, nothing seems to use?  it just gets passed down to the terminals unaltered and is ignored
--NOTE: dAtts is only passed to f.
--TODO: space leak here?
memoize :: (Eq att) => MemoL -> NTType att -> NTType att
memoize name f id downAtts (inp,dInput) context
 = do s <- get
      case (lookupT name inp (snd context) s) of
       Just lRes -> do let lookUpRes = addNode name (S, id) (inp,downAtts) (snd lRes)
                       return (fst lRes,lookUpRes)
       Nothing
           | funccount (snd context) inp name > (length dInput) - inp + 1 --NOTE: number of remaining input tokens is (length dInput) - inp + 1
               -> return ((Set.singleton name,Map.empty),[]) --TODO: how to return a failed parse?  good opportunity for error values!
           | otherwise -> do let iC = (Set.empty,(addNT name inp $ snd context))
                             (l,newRes) <- f id downAtts (inp,dInput) iC --NOTE: iC, inputContext?
                   -- let ((l,newRes),s1) = unMemoTable (f id downAtts (inp,dAtts) iC) s
                             let l1          = makeContext (fst l) inp (snd context)
                             s1             <- get
                             let udtTab      = udt (l1,newRes) name inp s1 --NOTE: "f" above may alter the memo table, so we need s1 here
                             let newFoundRes = addNode name (S, id) (inp,downAtts) newRes --NOTE: addNode seems to refer specifically to synthesized atts here
                             put udtTab
                             return (l1 ,newFoundRes)

--TODO: must preserve strictness with replaceSnd'!
replaceSnd' !key def f map = Map.insertWith (\_ -> \old_value -> f old_value) key def map


--NOTE: Should this return all matches?  why return a list?
--findWithFst_orig key = find ((== key) . fst)
--Answer: No, because before, empty/singleton lists were treated like Maybe, and lists of pairs are treated like maps

--NOTE: findWithDefault resolved it the space leak here using Map.lookup!
--NOTE: funccount needs name to be strict to not have a space leak
--NOTE: using Maybe also seems to cause a space leak, maybe because Maybe is lazy?
funccount :: ContextMap -> Int -> MemoL -> Int
funccount list !inp !name = Map.findWithDefault 0 (inp,name) list

--NOTE: a singleton with an empty map is treated the same as an empty map anyway, in condCheck, so just unify the two cases
--      i.e., passing a Just $ Map.empty from a Map.lookup in condCheck is the same thing as passing in Nothing.
--NOTE: this differs from the original a bit -- the original could return a singleton with an empty map.  this one won't do that.
--Implications: it seems before that (inp, Map.empty) was treated the same as not having inp in the map at all.  replaceSnd' treats both the same anyway.
--NOTE: Be aware of this in case this broke something
{-
Old definition that for sure worked is as follows:
makeContext :: Set.Set MemoL -> Maybe (Int, Map.Map MemoL Int) -> Context
makeContext rs js | Set.null rs     = empty_cuts
                  | Just (st,ncs) <- js = (rs, Map.singleton st (Map.filterWithKey (\k _ -> k `elem` rs) ncs)) -- (rs, [(st, concatMap (flip filterWithFst ncs) rs)])
                  | otherwise           = (rs, Map.empty)
-}
makeContext :: Set.Set MemoL -> Int -> ContextMap -> Context
makeContext rs st js = (rs, memos)
    where
        memos = Map.restrictKeys js (Set.map (\q -> (st,q)) rs) -- (rs, [(st, concatMap (flip filterWithFst ncs) rs)])

--Merged with incContext
addNT name inp = replaceSnd' (inp,name) 1 (1+)

--NOTE: could be a Start1 att or and End att
addNode :: Eq att => MemoL -> Instance -> (Int, InsAttVals att) -> Result att -> Result att
addNode name id (s',dA) [] = []

--TODO: more list incomprehensions
addNode name id (s',dA)  oldResult -- ((((s,newIh),(e,atts)),t):rs)
 = let --res     = packAmb oldResult --NOTE: never actually used.  Seems to have been an attempt to reconcile multiple attributes from different rules?
       s_id = snd id
       newSh x = [ ((sOri, s_id),attVal)| ((sOri, _),attVal)<- x]
   in  [let newShAtts = newSh atts in (((s,e),(newIh, newShAtts)),SubNode ((name, id),((s,e),(nub dA, newShAtts))))
       | (((s,e),(newIh,atts)),t) <- oldResult] --NOTE: here t is not enforced as a singleton list, but also note that t is thrown out

--NOTE: Does not seem to be used???  WAS referenced above but not used in addNode with let res = packAmb oldResult
--NOTE: this seems broken somehow, as when it is used above instead of oldResult, it will skip valid parses.
--NOTE: it seems like this is only able to merge 2 things?  Was it intended to do more than that?  No reference in the literature.
--NOTE: packAmb relies on there being a list of trees (for t1 ++ t2).  But it seems that singleton lists of trees are enforced in a lot of places.
{-
packAmb :: Result att -> Result att
packAmb [] = []
packAmb [x] = [x]
packAmb (x:y:ys) = if isEq x y then packAmb $ (packer x y):ys else x:packAmb (y:ys)
  where
  packer :: (StartEnd att, Tree att) -> (StartEnd att, Tree att) -> (StartEnd att, Tree att)
  packer ((s1, e1), t1) ((s2, e2), t2) = ((s2, (fst e2, groupAtts (snd e1 ++ snd e2))), t1 ++ t2)
  isEq :: (StartEnd att, Tree att) -> (StartEnd att, Tree att) -> Bool
  isEq (((s1,_),(e1,_)),_) (((s2,_),(e2,_)),_) = s1 == s2 && e1 == e2
-}
lookupT :: MemoL -> Int -> ContextMap -> MemoTable att -> Maybe (ParseResult att)
lookupT name inp context mTable = do
    res_in_table <- Map.lookup (name,inp) mTable
    if checkUsability inp context res_in_table then return res_in_table else Nothing --TODO: lookupRes uses Maybe which might cause a space leak

--TODO: another example right here of where a list is clearly used as a map!
lookupRes :: Int -> [(Int,(ParseResult att))] -> Maybe (ParseResult att)
lookupRes inp res_in_table = find (\(i, _) -> i == inp) res_in_table >>= return . snd

checkUsability :: Int -> ContextMap -> (ParseResult att) -> Bool
checkUsability inp context x@((re,sc),_) = null re || checkUsability_ (findInp inp context) (findInp inp sc)
  where
  --NOTE: In old version, want Nothing to be the case if list is empty or the inp could not be found
  --TODO: ensure findImp is strict
  findInp :: Int -> ContextMap -> ContextMap
  findInp inp = Map.takeWhileAntitone (\(k,_) -> k == inp) . Map.dropWhileAntitone (\(k,_) -> k /= inp)

  checkUsability_ :: ContextMap -> ContextMap -> Bool
  checkUsability_ = condCheck

  --NOTE: the use of Maybe in the original checkUsability complicates the logic for no reason.  The results are the same for Just Map.empty and Nothing.

  --checkUsability_  _         Nothing      = True -- if lc at j is empty then re-use
  --checkUsability_ Nothing    _            = False    -- if cc at j is empty then don't re-use
  --checkUsability_ (Just ccs) (Just scs)  | condCheck ccs scs = True
  --                                       | otherwise = False
  --TODO: this is really unoptimal!  this could be significantly improved
  --The idea: each sc should have a corresponding cc that is greater than it
  --NOTE: second arg was called scs
  condCheck :: ContextMap -> ContextMap -> Bool
  condCheck ccs = Map.foldrWithKey' (condCheck_ ccs) True
  condCheck_ :: ContextMap -> (Int,MemoL) -> Int -> Bool -> Bool
  --NOTE: findWithDefault set to -1 to avoid the Maybe, and -1 is strictly below any of the allowed values of cs1
  condCheck_ ccs n1 cs1 b = (let cs = Map.findWithDefault (-1) n1 ccs in cs >= cs1) && b

{-nub (dA ++ dAtts)-}
--udt == "update table"?
--TODO: nub is odd choice here, find out why it's needed.  seems to be used to remove duplicate attributes in the same slot (S1, S2... etc).  But why is this needed?
--TODO: at the very least we should be using a map for those attributes to prevent this from happening!
udt :: (Eq att) => (ParseResult att) -> MemoL -> Int -> MemoTable att -> MemoTable att
udt res name inp = Map.insert (name,inp) res

--TODO: this absolutely sucks.  need to do this *right*.
--NOTE: it looks like the Start1's inp is used as a unique key?  Perhaps we should do: Map.Map Int (InsAttVals, (ParseResult att)) instead?
replaceSndG :: Int -> (ParseResult att) -> [(Int, (ParseResult att))] -> [(Int, (ParseResult att))]
replaceSndG inp res list
  = before ++ replaceFirst replacePoint
  where
    (before, replacePoint) = break ((== inp) . fst) list
    def = (inp,res)
    replaceFirst [] = [def]
    replaceFirst ((a, b):nc) = def:nc

my_merge :: Int -> (ParseResult att) -> [(Int, (ParseResult att))] -> [(Int, (ParseResult att))]
my_merge inp res = replaceSndG inp res

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
superterminal :: (Eq att) => MemoL -> (T.Text -> Maybe (Atts att)) -> NTType att
superterminal key f = memoize key (superterminal' f)

superterminal' :: (Eq att) => (T.Text -> Maybe (Atts att)) -> NTType att
superterminal' f id _ q@(r,dInp)
 = term q --trace ((show $ T.intercalate " " dInp) ++ show q) $ term q
    where
    inst = (S, id)
    instAttVals atts =  [(inst,atts)]
    --term :: M att --NTType
    term (r,dInp) _ = if r - 1 == Vector.length dInp then return (empty_cuts,[]) else let str = Vector.unsafeIndex dInp (r - 1) in case f str of
        Nothing -> return (empty_cuts, [])
        Just atts -> return (empty_cuts,[(((r,r+1),([],instAttVals atts)),Leaf (ALeaf str, inst))])

terminal :: (Eq att) => T.Text -> Atts att -> NTType att
terminal str semRules id _ (i,inp)
 = term str ((i,[]),inp)  --NOTE: the downAtts are ignored regardless?
    where
    inst = (S, id)
    atts = [(inst,semRules)]
    --term :: T.Text -> M att
    term str ((r,_),dInp) _
     |r - 1 == Vector.length dInp            = return (empty_cuts,[])
     |Vector.unsafeIndex dInp (r - 1) == str = return (empty_cuts,[(((r,r+1),([],atts)),Leaf (ALeaf str, inst))])
     |otherwise                              = return (empty_cuts,[])

--NOTE: This takes into account multiple rules as if (terminal b x <|> terminal a z) were used
terminalSet :: (Eq att) => MemoL -> HashMap.Lazy.HashMap (MemoL,T.Text) [Atts att] -> NTType att
terminalSet key hashMap id _ (i,inp)
 =  actualTerm
    where
    selectedSemRules | i - 1 == length inp = Nothing
                     | otherwise = HashMap.Lazy.lookup (key,word) hashMap
    word = Vector.unsafeIndex inp (i - 1) --shouldn't be evaled until needed
    actualTerm _ = case selectedSemRules of
        Just semRulesList -> --TODO: rename attValueList
            let inst = (S, id) in
            let gen = (\semRules -> let atts = [(inst,semRules)] in (((i,i+1),([],atts)),Leaf (ALeaf word, inst))) in
                return (empty_cuts, map gen semRulesList)
        Nothing -> return (empty_cuts,[])

nt :: (Show att) =>  NTType att -> Id -> SeqType att
nt fx idx id inhAtts semRules altFromSibs
 = let groupRule'' inst       = filter ((inst ==) . fst) semRules
       ownInheritedAtts       = mapInherited (groupRule'' (I, idx)) altFromSibs inhAtts id --NOTE: multiple rule_i allowed!
   in fx idx ownInheritedAtts

--NOTE: So it seems a terminal may contain one or more atts (multiple atts per slot, like S0, S1...)
--      but it seems like a rule will always yield one att.
--      multiple atts are therefore obtained from different rules.
parser :: (Show att) =>  SeqType att -> [SemRule att] -> NTType att
parser synRule semRules id inhAtts i c
 =      do
           s <- get
           let ((e,altFromSibs),d)     =
                let sRule                        = groupRule'' (S, LHS) semRules --NOTE: multiple rule_s allowed!
                    ((l,newRes),st)              = runState ((synRule id inhAtts semRules altFromSibs) i c) s --NOTE: whoa. altFromSibs being used recursively.
                    groupRule'' id rules         = [(id,rule) | (ud,rule) <- rules, id == ud] --TODO: way inefficient.  Need better representation for rules if we're going to do this!
                in  ((l, mapSynthesize  sRule newRes inhAtts id),st)
           put d
           return (e,altFromSibs)

--NOTE: see note above.  a rule evaluates to a single att.  But here, we wrap it into a singleton list of atts.  We do the same with inherited atts.
--TODO: why?  can we improve the InsAttVals representation with this?
mapSynthesize :: (Show att) => [SemRule att] -> Result att -> InsAttVals att -> Id -> Result att
mapSynthesize []   res  downAtts id   = res
mapSynthesize sems res  downAtts id
 = let appSems' :: [SemRule att] -> InsAttVals att -> InsAttVals att
       appSems' [] vals        = []
       appSems' ((i,r):rules) vals
        = let ((ud, _), atts) = (i, [r (vals, id)]) --NOTE: it's a singleton att, but terminals may have *multiple* atts.  TODO: it is POSSIBLE to have multiple rule_s.  They would each have the same instance!  Only the first one would be respected by default?
          in  ((ud, id), atts) : appSems' rules vals
   in  [(((st,en), (inAtts,appSems' sems (findAtts t))),t) --TODO: this may be why using nt without memoize doesn't work for terminals -- findAtts specifically checks SubNodes and not Leafs
       |(((st,en), (inAtts,synAtts)),t) <- res] --TODO: why is findAtts needed?  why doesn't using synAtts work?
           --NOTE: the actual synAtts are ignored entirely!  it expects the atts to be in t.  Note that each rule adds an element to the result.

--TODO: (vals,id) could be separate arguments without ANY real problem!!!!!
--TODO: what do we do if the union of two InsAttVals maps contains a DUPLICATE VALUE?! before this was just a concatMap, would RETAIN duplicate KEYS
--TODO: do we just join them together?!! WTF.  This may be related to the fact that a rule really returns just a regular old att instead of an InsAttVals att!!!!
--NOTE: terminals may have multiple atts, but a rule must always give a single att?
applySemantics :: InsAttVals att -> Id -> [(Instance,(InsAttVals att, Id) -> att)] -> InsAttVals att
applySemantics vals id = map (\(i,rule) -> (i,[rule (vals, id)])) --NOTE: it's a singleton att, but terminals may have *multiple* atts. TODO: separate representation for inherited atts?

--NOTE: inherited atts have form OF Id that is NOT LHS
--      synthesized atts have the form OF LHS.  Could this be part of what's going on here?
mapInherited :: (Show att) => [(Instance,(InsAttVals att, Id) -> att)] -> Result att -> InsAttVals att -> Id -> InsAttVals att
mapInherited sems res [] id
  = concat [applySemantics (findAtts t) id sems | (((st,en), (inAtts,synAtts)),t) <- res] --TODO: why list of trees if singleton enforced here? is it possible to have no tree?  or multiple trees?

mapInherited sems []  downAtts id
  = applySemantics downAtts id sems -- concat ( map (appSems id sems) (group downAtts))

mapInherited sems res downAtts id
  = concat [applySemantics (downAtts ++ synAtts ++ (findAtts t)) id sems --TODO: purpose of findAtts here??
              | (((st,en), (inAtts,synAtts)),t) <- res]

--NOTE: what is this even for??  why is the first thing in the second tuple completely ignored?  why does it work in pairs?????
--NOTE: this function seems to not actually do anything meaningful!!!!  WAS used above in (groupAtts downAtts)
--NOTE: when used above in (groupAtts downAtts), only the case [(a,b)] was actually hit
--NOTE: this is referenced in packAmb where the other cases ARE hit.  But that function seems broken?
--NOTE: the first thing in the tuple would have been the instance, which would have been (I, id).
--Were they guaranteed to be the same?  Was this an attempt to merge attributes from different rule_i?
groupAtts []                    = []
groupAtts [(a,b)]               = [(a,b)]
groupAtts [(a,b),(_,b1)]       = [(a,b++b1)]
groupAtts ((a,b):(_,b1):rest)  = (a,b++b1): groupAtts rest

showTree (Branch ts) = "(" ++ concatMap showTree ts ++ ")"
showTree (SubNode _) = "SubNode"
showTree (Leaf _)    = "Leaf"

--------------------------------------
--TODO: optimize!
--TODO: what should happen if same instance is repeated? error?
findAtts :: (Show att) => Tree att -> InsAttVals att
--findAtts t = trace ("Tree: " ++ showTree t) $ findAtts' t
findAtts (Branch ts)                  = concatMap findAtts ts
findAtts (SubNode (_,((_,_),(v',v)))) = v' ++ v --NOTE: it is likely that v' are the inherited atts and v are the synthesized atts
findAtts (Leaf _)                     = []

--------------------------------------------------------


rule_i          = rule I
rule_s          = rule S

--TODO: more undefined...
--seems that there's a basic error catch here: if the left hand side does not agree with the right hand side data constructor, then it should
--not be defined by setAtt.  "typ undefined" is compared to resType, to see if the same data constructor was used.
--Also, no matter the declared type, if the right hand side is ErrorVal, we get ErrorVal, full stop.
--This thing has bitten me so many times... it would be nice to actually have it done right.
--TODO: multiple rule_s are permitted, but the typ, the only disambiguating thing for rule differentation, is ignored.  Need to handle this.
rule :: (Eq att) => SorI -> ignored -> Useless -> Id -> Useless -> ([InsAttVals att -> Id -> Atts att] -> (InsAttVals att, Id) -> att) -> [InsAttVals att -> Id -> Atts att] -> SemRule att
rule s_or_i typ oF pID isEq userFun listOfExp
 = let formAtts  id spec =  (id, spec)
       forNode   id atts = (id, atts)
       resType           = userFun  listOfExp
   in  formAtts (s_or_i,pID) resType --TODO: it's just a singleton att?! Why not just return the att?!!

---- **** -----

synthesized :: Eq att => (a -> att) -> Useless -> Id -> InsAttVals att -> Id -> Atts att
synthesized = valOf S
inherited   = valOf I

valOf :: (Eq att) => SorI -> (a -> att) -> Useless -> Id -> InsAttVals att ->  Id -> Atts att
valOf ud typ o_f x  ivs x' | x == LHS   = getAttVals (ud , x') ivs typ --the inherited Id from parent? (there should be type level enforcement here)
                           | otherwise  = getAttVals (ud , x ) ivs typ --the synthesized id? or inherited id from sibling? (there should be type level enforcement here)

--TODO: Whoa!!!!  not cool!
--This appears to be invoked if an Id is requested and not used
--TODO: it seems that an instance may point to multiple different atts... may need to go back to the drawing board
--NOTE: it looks like it just takes the first i == x and returns getAttVals_ for that.  no worry about duplicate instances here at least.
getAttVals :: (Eq att) => Instance -> InsAttVals att -> (a -> att) -> Atts att
getAttVals x ((i,v):ivs) typ =
 let getAttVals_ typ (t:tvs) = if (typ undefined) == t then (t :getAttVals_ typ tvs) --NOTE: linear search here too, perhaps we need to store attributes in a Map.Map (Instance,Type) [AttValue]? (recall multiple atts allowed)
                               else getAttVals_ typ tvs
     getAttVals_ typ []      = [] -- ErrorVal {-- 100 --} "ERROR id found but no value" (TODO: this appears to be a *legitimate* case that is NOT AN ERROR for inherited atts!!!  this was COMMENTED OUT IN THE ORIGINAL)
 in
     if(i == x) then getAttVals_ typ v else   getAttVals x ivs typ
--getAttVals x [] typ          = [ErrorVal {-- 200 --} "ERROR no id"] --TODO: note, the error being reported would be here.  this is the ONLY case of error reporting...
getAttVals x [] typ = error "ERROR no id"

-------- ************************************** ------------

------------------------- user functions ------------------
apply :: InsAttVals att -> Id -> (InsAttVals att -> Id -> AttValue) -> Int
apply  y i x   = getAVAL (x y i)
apply_ y i x   = getB_OP (x y i)

apply__ :: InsAttVals att -> Id -> (InsAttVals att -> Id -> AttValue) -> DisplayTree
apply__ y i x  = getRVAL (x y i)

applyMax :: InsAttVals att -> Id -> (InsAttVals att -> Id -> Atts AttValue) -> Int
applyMax  y i x   = getAVAL (foldr getMax (MaxVal 0) (x y i))
getMax :: AttValue -> AttValue -> AttValue
getMax    x   y   = MaxVal  (max (getAVAL x) (getAVAL y))

findMax :: [InsAttVals att -> Id -> Atts AttValue] -> (InsAttVals att, Id) -> AttValue
findMax spec (atts,i) = MaxVal (foldr max 0 (map (applyMax atts i) spec))

convertRep :: [InsAttVals att -> Id -> Atts AttValue] -> (InsAttVals att, Id) -> AttValue
convertRep spec (atts,i) = RepVal (foldr max 0 (map (applyMax atts i) spec))

makeTree :: [InsAttVals att -> Id -> AttValue] -> (InsAttVals att, Id) -> AttValue
makeTree (x:xs) (atts,i) = Res (B (map (apply__ atts i) (x:xs)))

mt :: [DisplayTree] -> DisplayTree
mt [a,b,c] = (B [a,b,c])
mt [a]     = (B [a])

----------- for arithmetic expr -----------------
applyBiOp :: [InsAttVals AttValue -> Id -> Atts AttValue] -> (InsAttVals AttValue, Id) -> AttValue
applyBiOp [e1,op,e2] atts = VAL ((getAtts getB_OP atts op ) (getAtts getAVAL atts e1 ) (getAtts getAVAL atts e2))
getAtts :: (att -> a) -> (InsAttVals att, Id) -> (InsAttVals att -> Id -> Atts att) -> a
getAtts f (y,i) x = f (head (x y i))  --NOTE: this ignores all other matching atts?!  This should return a *list*, it clearly says *atts*!!!!
--at the very least it should be called getFirstAtt or something

----------- general copy ------------------------
copy :: [(InsAttVals att -> Id -> Atts att)] -> (InsAttVals att, Id) -> att
copy [b] (atts,i) = head (b atts i) --NOTE: again, only the first att is returned!
getTypVal :: Eq att => [(a -> att, att -> p)] -> att -> p --NOTE: this function is very unsafe, it has no base case.  Seems to apply a function to the first matching att.  It also does not seem to be used!
getTypVal ((a,b):abs) t | a undefined == t = b t
                        | otherwise        = getTypVal abs t


----------- for arithmetic expr -----------------

toTree :: [InsAttVals AttValue -> Id -> AttValue] -> (InsAttVals AttValue, Id) -> AttValue
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
        rule_i   RepVal OF T1   ISEQUALTO convertRep    [inherited RepVal OF LHS], --NOTE: seems that for inherited atts, the Ids may cause trouble if they're the same as the parent (T0 here)
        rule_i   RepVal OF T2   ISEQUALTO convertRep    [inherited RepVal OF LHS], --NOTE: i think you can inherit from siblings too
        rule_i   RepVal OF T3   ISEQUALTO convertRep    [inherited RepVal OF LHS]  --NOTE: possible to enter an infinite loop by referring to self
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

attsFinalAlt :: MemoL -> Int -> MemoTable att -> [[Atts att]]
attsFinalAlt  key e t  =  maybe [] allTrees (Map.lookup (key,1) t)
    where
        allTrees (_,rs) = [ map snd synAtts | (((_,end),(_,synAtts)), _)<-rs, end == e]

--Using a start, end, and memoization key, locate all valid parses that match.  In the case of ambiguity, there may be more than one result.
--These three conditions are sufficient to guarantee the result is unique and valid.
lookupTable :: MemoL -> Int -> Int -> MemoTable att -> [Tree att]
lookupTable key start end t = maybe [] allTrees (Map.lookup (key,start) t)
    where allTrees (_, results) = [ tree | (((_,_end),_), tree) <- results, end == _end]

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
--NOTE: it looks like traversing this way will guarantee that the AttValues retrieved later during formatAttsFinalAlt will be in the same order as the returned trees
--(so that the trees will map correspondingly to their attvalues).  But I haven't proved this at all!  It would be sure nice if the tree were embedded somehow in the actual parse.
--TODO: guarantee this behaviour!
data SyntaxTree = SyntaxTreeNT [SyntaxTree] | SyntaxTreeT T.Text deriving (Show)

findAllParseTrees t (Leaf (ALeaf str, _)) = [SyntaxTreeT str]
--SubNodes introduce ambiguity?
findAllParseTrees t (SubNode ((key, _), ((_start, _end), _))) = let trees = nubBy eqAmb (lookupTable key _start _end t) in concatMap (findAllParseTrees t) trees
--nubBy eqAmb necessary for Branch?  Ambiguity?
findAllParseTrees t (Branch tree) = map SyntaxTreeNT $ sequence $ map (findAllParseTrees t) tree

findAllParseTreesT' key end t = zip sems trees
    where
        sems = formatAttsFinalAlt key end t
        trees = concat $ sequence $ map (findAllParseTrees t) $ nubBy eqAmb (lookupTable key 1 end t)

findAllParseTreesFormatted formatTree key end t = map (\(x, y) -> (x, formatTree y)) $ findAllParseTreesT' key end t

findAllParseTreesFormatted' = findAllParseTreesFormatted syntaxTreeToLinearGeneric'

--Compare trees up until Atts.  We're looking for a syntactic match and cannot compare atts meaningfully.
eqAmb :: (Eq att) => Tree att -> Tree att -> Bool
eqAmb (Leaf x) (Leaf y) = x == y
eqAmb (SubNode x) (SubNode y) = x == y
eqAmb (Branch []) (Branch []) = True
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
formatAttsFinalAlt :: MemoL -> Int -> MemoTable att -> Atts att
formatAttsFinalAlt key e t =  concat $ concat $ attsFinalAlt key e t

meaning_of :: NTType att -> T.Text -> MemoL -> Atts att
meaning_of p dInp key
 = let dInput     = Vector.fromList $ T.words dInp
       appParser  = runState (p T0 [] (1, dInput) empty_cuts) Map.empty
       upperBound = (length dInput) + 1
   in  formFinal key upperBound (snd $ appParser)

meaning_of_ :: NTType att -> T.Text -> MemoL -> MemoTable att
meaning_of_ p dInp key
 = let dInput     = Vector.fromList $ T.words dInp
       appParser  = runState (p T0 [] (1, dInput) empty_cuts) Map.empty
       upperBound = (length dInput) + 1
   in  (snd $ appParser)

--TODO: these are the same?!!
formAtts key ePoint t = maybe [] allAtts $ Map.lookup key t
        where allAtts sr = concat $ concat $ concat [[[  val1 |(id1,val1)<-synAtts]
                                                    |(((st,end),(inAtt2,synAtts)), ts)<-rs, st == 1 && end == ePoint]
                                                    |(i,((cs,ct),rs)) <- sr ]

formFinal key ePoint t = maybe [] final $ Map.lookup (key,1) t
    where final (_,rs) = concat $ concat $ [[  val1 |(id1,val1)<-synAtts]
                                                    |(((st,end),(inAtt2,synAtts)), ts)<-rs, st == 1 && end == ePoint] --TODO: if the start in the table is the same as here... why do we need it in two places?

--test1 p p_ inp = do putStr  $ render80 $ format{-Atts p_-} $ snd $ runState (p T0 [] ((1,[]),words inp) ([],[])) []
test :: M att
    -> Vector.Vector T.Text
    -> ((ParseResult att), MemoTable att)
test p input = runState (p (1,input) empty_cuts) Map.empty
--NOTE: above, (1,[]) is a Start1 and ((1,[]),input) is a Start.  The Start1 corresponds to the inherited atts to begin with?  But those are also supplied in p.
--      context always starts empty.

--formatParseIO = mapM id . map showio . parse

findStart st ((s,ss):rest) | s == st   = [(s,ss)]
                           | otherwise = findStart st rest
findStart st []                        = []
