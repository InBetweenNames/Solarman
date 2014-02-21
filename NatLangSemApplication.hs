module NatLangSemApplication where
-- import Text.PrettyPrint.HughesPJ as PPH hiding (empty)  -- ((PPH.<+>), vcat, text, render,($$),nest,Doc)
import Data.List
import AGParser
import TypeAg


{-


||-------------------------------------------------------------------------------------
||  BASIC GRAMMAR OF THE QUERY LANGUAGE
||-----------------------------------------------------------------------------


|| snouncla          ::= cnoun | adjs cnoun
||
|| relnouncla        ::= snouncla relpron joinvbph
||                     | snouncla
||
|| nouncla           ::= relnouncla nounjoin nouncla
||                     | relnouncla relpron linkingvb nouncla
||                     | relnouncla
||
|| adjs              ::= adj | adj adjs
|| detph             ::= indefpron | det nouncla
||
|| transvbph         ::= transvb jointermph
||                     | linkingvb passtrvb prep jointermph
||
|| verbph            ::= transvbph | intransvb
||                     | linkingvb det nouncla
||
|| termph            ::= pnoun | detph
||
|| jointermph        ::= termph | termph termphjoin jointermph
||                     | apply_termphrase_pronoun
||
|| joinvbph          ::= verbph
||                     | verbph verbphjoin joinvbph
||
|| sent              ::= jointermph joinvbph
||                    
|| two_sent          ::= sent sentjoin sent
||
|| query             ::= question terminator
||
|| question          ::= quest1 sent 
||                     | quest2 joinvbph 
||                     | quest3 nouncla joinvbph 
||                     | quest4 nouncla joinvbph 
||                     | sent terminator
||                     | two_sent 
-- extra rules
1. termphrase ::= simple_termphrase
               | termphrase termpharse_join termphrase

||-----------------------------------------------------------------------------
|| BASIC SEMANTIC TYPES (in addition to Miranda's existing primitive types)
||-----------------------------------------------------------------------------

entity         ==  num  
entityset      ==  [entity]   
string         ==  [char]
es             ==  entityset
b              ==  bool
bin_rel        == [(entity,entity)]
-}
{-
||-----------------------------------------------------------------------------
|| ATTRIBUTE TYPES
||-----------------------------------------------------------------------------


attribute  ::=
 LITERAL_VAL            terminal   
 |  SENT_VAL            b  
 |  NOUNCLA_VAL         es
 |  VERBPH_VAL          es  
 |  ADJ_VAL             es 
 |  TERMPH_VAL         (es -> b)          
 |  DET_VAL            (es -> es -> b) 
 |  VERB_VAL            bin_rel                
 |  RELPRON_VAL        (es -> es -> es)      
 |  NOUNJOIN_VAL       (es -> es -> es) 
 |  VBPHJOIN_VAL       (es -> es -> es)      
 |  TERMPHJOIN_VAL     ((es -> b) -> (es -> b)  -> (es -> b))                             

 |  PREP_VAL            (es -> es)       
 |  LINKINGVB_VAL       (es -> es)       
 |  SENTJOIN_VAL        (b -> b -> b)
 |  DOT_VAL             [char]  
 |  QM_VAL              [char]
 |  QUEST_VAL           string
 |  QUEST1_VAL          (b -> string)
 |  QUEST2_VAL          (es -> string)
 |  QUEST3_VAL          (es -> es -> string)
 |  RESULT [sys_message]
-- ||---------------------------------------------------------------------------------
-}
reserved_words  = []
special_symbols = ['.', '?','\n']


-- ||-----------------------------------------------------------------------------
-- || THE DICTIONARY

dictionary = 
 [("man",                Cnoun,     [NOUNCLA_VAL set_of_men]),
  ("men",                Cnoun,     [NOUNCLA_VAL set_of_men]),
  ("thing",              Cnoun,     [NOUNCLA_VAL set_of_things]),
  ("things",             Cnoun,     [NOUNCLA_VAL set_of_things]),
  ("planets",            Cnoun,     [NOUNCLA_VAL set_of_planet]),
  ("planet",             Cnoun,     [NOUNCLA_VAL set_of_planet]),
  ("woman",              Cnoun,     [NOUNCLA_VAL set_of_woman]),   
  ("women",              Cnoun,     [NOUNCLA_VAL set_of_woman]),  
  ("sun",                Cnoun,     [NOUNCLA_VAL set_of_sun]), 
  ("moon",               Cnoun,     [NOUNCLA_VAL set_of_moon]), 
  ("moons",              Cnoun,     [NOUNCLA_VAL set_of_moon]),
  ("satellite",          Cnoun,     [NOUNCLA_VAL set_of_moon]),
  ("satellites",         Cnoun,     [NOUNCLA_VAL set_of_moon]),
  ("atmospheric",        Adj,       [ADJ_VAL     set_of_atmospheric]),
  ("blue",               Adj,       [ADJ_VAL     set_of_blue]),
  ("blue",               Adj,       [ADJ_VAL     set_of_depressed]),
  ("solid",              Adj,       [ADJ_VAL     set_of_solid]),     
  ("brown",              Adj,       [ADJ_VAL     set_of_brown]),   
  ("gaseous",            Adj,       [ADJ_VAL     set_of_gaseous]),  
  ("green",              Adj,       [ADJ_VAL     set_of_green]),  
  ("red",                Adj,       [ADJ_VAL     set_of_red]),  
  ("ringed",             Adj,       [ADJ_VAL     set_of_ringed]),  
  ("vacuumous",          Adj,       [ADJ_VAL     set_of_vacuumous]),
  ("exist",              Intransvb, [VERBPH_VAL  set_of_things]),
  ("exists",             Intransvb, [VERBPH_VAL  set_of_things]),
  ("spin",               Intransvb, [VERBPH_VAL  set_of_spin]),
  ("spins",              Intransvb, [VERBPH_VAL  set_of_spin]),
  ("the",                Det,       [DET_VAL function_denoted_by_a]),
  ("a",                  Det,       [DET_VAL function_denoted_by_a]),
  ("one",                Det,       [DET_VAL function_denoted_by_one]), 
  ("an",                 Det,       [DET_VAL function_denoted_by_a]), 
  ("some",               Det,       [DET_VAL function_denoted_by_a]), 
  ("any",                Det,       [DET_VAL function_denoted_by_a]), 
  ("no",                 Det,       [DET_VAL function_denoted_by_none]), 
  ("every",              Det,       [DET_VAL function_denoted_by_every]), 
  ("all",                Det,       [DET_VAL function_denoted_by_every]),  
  ("two",                Det,       [DET_VAL function_denoted_by_two]),
  ("bernard",            Pnoun,     [TERMPH_VAL (test_wrt 55)]),
  ("bond",               Pnoun,     [TERMPH_VAL (test_wrt 67)]),
  ("venus",              Pnoun,     [TERMPH_VAL (test_wrt 10)]),
  ("cassini",            Pnoun,     [TERMPH_VAL (test_wrt 65)]),
  ("dollfus",            Pnoun,     [TERMPH_VAL (test_wrt 63)]),
  ("Fouuntain",          Pnoun,     [TERMPH_VAL (test_wrt 62)]),
  ("galileo",            Pnoun,     [TERMPH_VAL (test_wrt 56)]),
  ("hall",               Pnoun,     [TERMPH_VAL (test_wrt 54)]),
  ("herschel",           Pnoun,     [TERMPH_VAL (test_wrt 64)]),
  ("huygens",            Pnoun,     [TERMPH_VAL (test_wrt 66)]),
  ("kowal",              Pnoun,     [TERMPH_VAL (test_wrt 57)]),
  ("kuiper",             Pnoun,     [TERMPH_VAL (test_wrt 69)]),
  ("larsen",             Pnoun,     [TERMPH_VAL (test_wrt 61)]),
  ("lassell",            Pnoun,     [TERMPH_VAL (test_wrt 70)]),
  ("melotte",            Pnoun,     [TERMPH_VAL (test_wrt 60)]),
  ("nicholson",          Pnoun,     [TERMPH_VAL (test_wrt 59)]),
  ("perrine",            Pnoun,     [TERMPH_VAL (test_wrt 58)]),
  ("pickering",          Pnoun,     [TERMPH_VAL (test_wrt 68)]),
  ("almathea",           Pnoun,     [TERMPH_VAL (test_wrt 21)]),
  ("ariel",              Pnoun,     [TERMPH_VAL (test_wrt 47)]),
  ("callisto",           Pnoun,     [TERMPH_VAL (test_wrt 25)]),
  ("charon",             Pnoun,     [TERMPH_VAL (test_wrt 53)]),
  ("deimos",             Pnoun,     [TERMPH_VAL (test_wrt 20)]),
  ("dione",              Pnoun,     [TERMPH_VAL (test_wrt 40)]),
  ("earth",              Pnoun,     [TERMPH_VAL (test_wrt 11)]),
  ("enceladus",          Pnoun,     [TERMPH_VAL (test_wrt 38)]),
  ("europa",             Pnoun,     [TERMPH_VAL (test_wrt 23)]),
  ("ganymede",           Pnoun,     [TERMPH_VAL (test_wrt 24)]),
  ("hyperion",           Pnoun,     [TERMPH_VAL (test_wrt 43)]),
  ("iapetus",            Pnoun,     [TERMPH_VAL (test_wrt 44)]),
  ("io",                 Pnoun,     [TERMPH_VAL (test_wrt 22)]),
  ("janus",              Pnoun,     [TERMPH_VAL (test_wrt 36)]),
  ("jupiter",            Pnoun,     [TERMPH_VAL (test_wrt 13)]),
  ("jupitereighth",      Pnoun,     [TERMPH_VAL (test_wrt 32)]),
  ("jupitereleventh",    Pnoun,     [TERMPH_VAL (test_wrt 31)]),
  ("jupiterfourteenth",  Pnoun,     [TERMPH_VAL (test_wrt 34)]),
  ("jupiterninth",       Pnoun,     [TERMPH_VAL (test_wrt 33)]),
  ("jupiterseventh",     Pnoun,     [TERMPH_VAL (test_wrt 29)]),
  ("jupitersixth",       Pnoun,     [TERMPH_VAL (test_wrt 27)]),
  ("jupitertenth",       Pnoun,     [TERMPH_VAL (test_wrt 28)]),
  ("jupiterthirteenth",  Pnoun,     [TERMPH_VAL (test_wrt 26)]),
  ("jupitertwelfth",     Pnoun,     [TERMPH_VAL (test_wrt 30)]),
  ("luna",               Pnoun,     [TERMPH_VAL (test_wrt 18)]),
  ("mars",               Pnoun,     [TERMPH_VAL (test_wrt 12)]),
  ("mercury",            Pnoun,     [TERMPH_VAL (test_wrt  9)]),
  ("mimas",              Pnoun,     [TERMPH_VAL (test_wrt 37)]),
  ("miranda",            Pnoun,     [TERMPH_VAL (test_wrt 46)]),
  ("neptune",            Pnoun,     [TERMPH_VAL (test_wrt 16)]),
  ("nereid",             Pnoun,     [TERMPH_VAL (test_wrt 52)]),
  ("oberon",             Pnoun,     [TERMPH_VAL (test_wrt 50)]),
  ("phobos",             Pnoun,     [TERMPH_VAL (test_wrt 19)]),
  ("phoebe",             Pnoun,     [TERMPH_VAL (test_wrt 45)]),
  ("pluto",              Pnoun,     [TERMPH_VAL (test_wrt 17)]),
  ("rhea",               Pnoun,     [TERMPH_VAL (test_wrt 41)]),
  ("saturn",             Pnoun,     [TERMPH_VAL (test_wrt 14)]),
  ("saturnfirst",        Pnoun,     [TERMPH_VAL (test_wrt 35)]),
  ("sol",                Pnoun,     [TERMPH_VAL (test_wrt  8)]),
  ("tethys",             Pnoun,     [TERMPH_VAL (test_wrt 39)]),
  ("titan",              Pnoun,     [TERMPH_VAL (test_wrt 42)]),
  ("titania",            Pnoun,     [TERMPH_VAL (test_wrt 49)]),
  ("triton",             Pnoun,     [TERMPH_VAL (test_wrt 51)]),
  ("umbriel",            Pnoun,     [TERMPH_VAL (test_wrt 48)]),
  ("uranus",             Pnoun,     [TERMPH_VAL (test_wrt 15)]),
  ("venus",              Pnoun,     [TERMPH_VAL (test_wrt 10)]),
  ("discover",           Transvb,   [VERB_VAL (trans_verb rel_discover)]),
  ("discovers",          Transvb,   [VERB_VAL (trans_verb rel_discover)]),
  ("discovered",         Transvb,   [VERB_VAL (trans_verb rel_discover)]),
  ("orbit",              Transvb,   [VERB_VAL (trans_verb rel_orbit)]),
  ("orbited",            Transvb,   [VERB_VAL (trans_verb rel_orbit)]),
  ("orbits",             Transvb,   [VERB_VAL (trans_verb rel_orbit)]),
  ("is",                 Linkingvb, [LINKINGVB_VAL  id]),
  ("was",                Linkingvb, [LINKINGVB_VAL  id]), 
  ("are",                Linkingvb, [LINKINGVB_VAL  id]),
  ("were",               Linkingvb, [LINKINGVB_VAL  id]),
  ("that",               Relpron,   [RELPRON_VAL    intersect]),
  ("who",                Relpron,   [RELPRON_VAL    intersect]),
  ("which",              Relpron,   [RELPRON_VAL    intersect]),
  ("and",                Verbphjoin,[VBPHJOIN_VAL   intersect]),
  ("or",                 Verbphjoin,[VBPHJOIN_VAL   union]),
  ("and",                Nounjoin,  [NOUNJOIN_VAL   intersect]),
  ("or",                 Nounjoin,  [NOUNJOIN_VAL   union]),
  ("by",                 Prep,      [PREP_VAL       id]),
  ("and",                Termphjoin,[TERMPHJOIN_VAL termph_and]),
  ("or",                 Termphjoin,[TERMPHJOIN_VAL termph_or]),
  ("and",                Sentjoin,  [SENTJOIN_VAL   sand]),
  ("does",               Quest1,    [QUEST1_VAL     yesno]),
  ("did",                Quest1  ,  [QUEST1_VAL     yesno]),
  ("do",                 Quest1,    [QUEST1_VAL     yesno] ),
  ("what",               Quest2,    [QUEST2_VAL     whatq]),
  ("who",                Quest2,    [QUEST2_VAL     whoq]),
  ("which",              Quest3,    [QUEST3_VAL     whichq]),
  ("what",               Quest3,    [QUEST3_VAL     whichq]),
  ("how",                Quest4a,   [QUEST3_VAL     howmanyq]),
  ("many",               Quest4b,   [QUEST3_VAL     howmanyq])  ]
  ++
  [("human",       Cnoun,    meaning_of nouncla "man or woman" Nouncla),
   ("person",      Cnoun,    meaning_of nouncla "man or woman" Nouncla), 
   ("discoverer",  Cnoun,    meaning_of nouncla 
                               "person who discovered something" Nouncla),
   ("discoverers", Cnoun,    meaning_of nouncla 
                               "person who discovered something" Nouncla), 
   ("humans",      Cnoun,    meaning_of nouncla "man or woman" Nouncla), 
   ("people",      Cnoun,    meaning_of nouncla "man or woman" Nouncla),
   ("orbit",       Intransvb,meaning_of verbph  "orbit something" Verbph),
   ("orbits",      Intransvb,meaning_of verbph  "orbit something" Verbph),
   ("anyone",      Indefpron,meaning_of detph   "a person" Detph),
   ("anything",    Indefpron,meaning_of detph   "a thing" Detph),
   ("anybody",     Indefpron,meaning_of detph   "a person" Detph),
   ("someone",     Indefpron,meaning_of detph   "a person" Detph),
   ("something",   Indefpron,meaning_of detph   "a thing" Detph),
   ("somebody",    Indefpron,meaning_of detph   "a person" Detph),
   ("everyone",    Indefpron,meaning_of detph   "every person" Detph),
   ("everything",  Indefpron,meaning_of detph   "every thing" Detph),
   ("everybody",   Indefpron,meaning_of detph   "every person" Detph),
   ("nobody",      Indefpron,meaning_of detph   "no person" Detph),
   ("noone",       Indefpron,meaning_of detph   "no person" Detph)]

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
prep            =  pre_processed Prep
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


type Mtable   = [(MemoL
                 ,[(Start1,(Context,Result))]
                 )
                ] 
type Result   = [((Start1, End),[Tree MemoL])]
||-----------------------------------------------------------------------------
|| THE ATTRIBUTE GRAMMAR
||-----------------------------------------------------------------------------
-}

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



nouncla 
 = memoize Nouncla 
   (parser (nt relnouncla S1 *> nt nounjoin S2 *> nt nouncla S3)
    [rule_s NOUNCLA_VAL OF LHS ISEQUALTO apply_middle2 [synthesized NOUNCLA_VAL  OF S1,
                                                        synthesized NOUNJOIN_VAL OF S2,
                                                        synthesized NOUNCLA_VAL  OF S3]]
    <|>
    parser (nt relnouncla S1 *> nt relpron S2 *> nt linkingvb S3 *> nt nouncla S4)
    [rule_s NOUNCLA_VAL  OF LHS ISEQUALTO apply_middle3 [synthesized NOUNCLA_VAL  OF S1,
                                                         synthesized RELPRON_VAL  OF S2,
                                                         synthesized NOUNCLA_VAL  OF S4]]
    <|>
    parser (nt relnouncla S1)
    [rule_s NOUNCLA_VAL  OF LHS ISEQUALTO copy [synthesized NOUNCLA_VAL  OF S1]]
   )

------------------------------------------------------------------------------
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
transvbph 
 = memoize Transvbph
   (parser (nt transvb S1 *> nt jointermph S2)
    [rule_s VERBPH_VAL OF LHS ISEQUALTO applytransvb [synthesized VERB_VAL    OF S1,
                                                      synthesized TERMPH_VAL  OF S2]]
    <|>
    parser (nt linkingvb S1 *> nt transvb S2 *> nt prep S3 *> nt jointermph S4)
    [rule_s VERBPH_VAL  OF LHS ISEQUALTO drop3rd [synthesized LINKINGVB_VAL  OF  S1,
                                                  synthesized VERB_VAL       OF  S2,
                                                  synthesized PREP_VAL       OF  S3,
                                                  synthesized TERMPH_VAL     OF  S4]]
   )

-------------------------------------------------------------------------------

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

termph    
 = memoize Termph  
   (
   parser (nt pnoun S1) 
   [rule_s TERMPH_VAL OF LHS ISEQUALTO copy [synthesized TERMPH_VAL OF S1]]
   <|>  
   parser (nt detph S2)
   [rule_s TERMPH_VAL OF LHS ISEQUALTO copy [synthesized TERMPH_VAL OF S2]]
   )
             

------------------------------------------------------------------------------------
jointermph 
 = memoize Jointermph 
   (
{-    parser (nt jointermph S1 *> nt termphjoin S2 *> nt termph S3)
        [rule_s TERMPH_VAL  OF LHS ISEQUALTO appjoin1 [synthesized TERMPH_VAL     OF S1,
                                                       synthesized TERMPHJOIN_VAL OF S2,
                                                       synthesized TERMPH_VAL     OF S3]]
   <|>
-}
    parser (nt jointermph S1 *> nt termphjoin S2 *> nt jointermph S3)
    [rule_s TERMPH_VAL  OF LHS ISEQUALTO appjoin1 [synthesized TERMPH_VAL     OF S1,
                                                   synthesized TERMPHJOIN_VAL OF S2,
                                                   synthesized TERMPH_VAL     OF S3]]
   <|>
    parser (nt termph S4)
    [rule_s TERMPH_VAL  OF LHS ISEQUALTO copy [synthesized TERMPH_VAL  OF S4]]
   )
------------------------------------------------------------------------------------
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
sent  
 = memoize Sent
   (
    parser (nt jointermph S1  *> nt joinvbph S2)
    [rule_s SENT_VAL OF LHS ISEQUALTO apply_termphrase [synthesized TERMPH_VAL  OF  S1,
                                                        synthesized VERBPH_VAL  OF  S2]]
   )
-- **************************************************************************** --
two_sent 
 = memoize Two_sent
   (
    parser (nt sent S1 *> nt sentjoin S2 *> nt sent S3)
    [rule_s SENT_VAL OF LHS ISEQUALTO sent_val_comp [synthesized SENT_VAL      OF  S1,
                                                     synthesized SENTJOIN_VAL  OF  S2,
                                                     synthesized SENT_VAL      OF  S3]] 
   )
------------------------------------------------------------------------------------

question   
 = memoize Question  
   (
    parser (nt quest1 S1  *> nt sent S2 )
    [rule_s QUEST_VAL  OF LHS ISEQUALTO ans1 [synthesized QUEST1_VAL  OF  S1,
                                              synthesized SENT_VAL    OF  S2]]  
    <|>
    parser (nt quest2 S1 *> nt joinvbph S2)
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
 = \atts -> NOUNCLA_VAL (intersect (getAtts getAVALS atts x) (getAtts getAVALS atts y))

intrsct2         [x, y]         
 = \atts -> ADJ_VAL (intersect (getAtts getAVALS atts x) (getAtts getAVALS atts y))

applydet         [x, y]                 
 = \atts -> TERMPH_VAL ((getAtts getDVAL atts x) (getAtts getAVALS atts y) )

applytransvb     [x, y]     
 = \atts -> VERBPH_VAL ((make_trans_vb (getAtts getBR atts x)) (getAtts getTVAL atts y))

applyvbph        [z]    
 = \atts -> VERBPH_VAL (getAtts getAVALS atts z)
 
appjoin1         [x, y, z]     
 = \atts -> TERMPH_VAL ((getAtts getTJVAL atts y) (getAtts getTVAL atts x) (getAtts getTVAL atts z))

appjoin2         [x, y, z]    
 = \atts -> VERBPH_VAL ((getAtts getVJVAL atts y) (getAtts getAVALS atts x) (getAtts getAVALS atts z))

apply_middle1    [x, y, z]    
 = \atts -> NOUNCLA_VAL ((getAtts getRELVAL atts y) (getAtts getAVALS atts x) (getAtts getAVALS atts z))

apply_middle2    [x, y, z]                  
 = \atts -> NOUNCLA_VAL ((getAtts getNJVAL atts y) (getAtts getAVALS atts x) (getAtts getAVALS atts z))

apply_middle3    [x, y, z]    
 = \atts -> NOUNCLA_VAL ((getAtts getRELVAL atts y) (getAtts getAVALS atts x) (getAtts getAVALS atts z))

drop3rd          [w, x, y, z]     
 = \atts -> VERBPH_VAL ((make_trans_vb (invert (getAtts getBR atts x))) (getAtts getTVAL atts z))

apply_termphrase [x, y]     
 = \atts -> SENT_VAL ((getAtts getTVAL atts x) (getAtts getAVALS atts y) )
 
sent_val_comp    [s1, f, s2]      
 = \atts -> SENT_VAL ((getAtts getSJVAL atts f) (getAtts getSV atts s1) (getAtts getSV atts s2))

ans1             [x, y]       
 = \atts -> QUEST_VAL ((getAtts getQU1VAL atts x) (getAtts getSV atts y) )

ans2             [x, y]     
 = \atts -> QUEST_VAL ((getAtts getQU2VAL atts x) (getAtts getAVALS atts y))

ans3             [x, y, z]     
 = \atts -> QUEST_VAL ((getAtts getQU3VAL atts x) (getAtts getAVALS atts y) (getAtts getAVALS atts z))

truefalse        [x]       
 = \atts -> if (getAtts getSV atts x) then (QUEST_VAL "true.") else (QUEST_VAL "false.")


{-
||-----------------------------------------------------------------------------
|| THE SEMANTICS - PART II : Functions used to obtain objects denoted by 
||   proper nouns, verbs, etc.
||-----------------------------------------------------------------------------

|| FUNCTION USED TO DEFINE OBJECTS ASSOCIATED WITH PROPER NOUNS
-}
test_wrt e s = e `elem` s

-- FUNCTION USED TO DEFINE MEANINGS OF VERBS IN TERMS OF RELATIONS
make_trans_vb rel p = [x | (x, image_x) <- collect rel, p image_x]

-- trans_verb rel p = [x | (x, image_x) <- collect rel, p image_x]

trans_verb x = x

-- passtr_verb rel = trans_verb (invert rel)

passtr_verb x = x

-- FUNCTIONS DENOTED BY TERMPHRASE CONJOINERS

termph_and p q       = g where g x = (p x) && (q x)
termph_or  p q       = g where g x = (p x) || (q x)

-- FUNCTIONS DENOTED BY SENTENCE CONJOINERS

sand True True = True
sand any any'  = False

-- FUNCTIONS DENOTED BY DETERMINERS

function_denoted_by_a xs ys       = length( intersect xs ys ) > 0
function_denoted_by_every xs ys   = includes xs ys
function_denoted_by_none xs ys    = length( intersect xs ys ) == 0
function_denoted_by_one xs ys     = length( intersect xs ys ) == 1
function_denoted_by_two xs ys     = length( intersect xs ys ) == 2

-- SET AND LIST OPERATORS

collect []          = []
collect ((x,y) : t) = (x, y:[e2 | (e1, e2) <- t, e1 == x]) :
                        collect [(e1, e2) | (e1, e2) <- t, e1 /= x]

invert           = map swap where swap (x, y) = (y, x)

-- intersect as bs  = as \\ (as \\ bs)

-- union     as bs  = as ++ (bs \\ as)

includes  as bs  = (as \\ bs) == []
---------------


{-
||-----------------------------------------------------------------------------
|| THE SEMANTICS - PART III : The database
||-----------------------------------------------------------------------------
-}


-- THE UNIVERSE OF DISCOURSE

set_of_things      = [8..70]

-- SETS DENOTED BY COMMON NOUNS

set_of_sun        = [8]
set_of_planet     = [9..17]
set_of_moon       = [18..53]
set_of_men        = [54..70]
set_of_woman      = []

-- SETS DENOTED BY ADJECTIVES

set_of_red         = [12, 13, 14, 22]
set_of_blue        = [11, 14, 15, 16]
set_of_depressed   = [54]
set_of_green       = [11, 15, 16]
set_of_brown       = [9, 10, 17]
set_of_ringed      = [13, 14, 15, 16]
set_of_gaseous     = [13, 14, 15, 16]
set_of_solid       = (union set_of_planet set_of_moon)
                                     -- set_of_gaseous
set_of_atmospheric = [ 10, 11, 12, 22, 42 ]
set_of_vacuumous   = (union set_of_planet
                          set_of_moon) -- set_of_atmospheric


-- SETS DENOTED BY INTRANSITIVE VERBS

set_of_spin      = [8..53]


-- BINARY RELATIONS


rel_orbit   =    [(9,8),(10,8),(11,8),(12,8),(13,8),
                  (14,8),(15,8),(16,8),(17,8),(18,11),
                  (19,12),(20,12),(21,13),(22,13),(23,13),
                  (24,13),(25,13),(26,13),(27,13),(28,13),
                  (29,13),(30,13),(31,13),(32,13),(33,13),
                  (34,13),(35,14),(36,14),(37,14),(38,14),
                  (39,14),(40,14),(41,14),(42,14),(43,14),
                  (44,14),(45,14),(46,15),(47,15),(48,15),
                  (49,15),(50,15),(51,16),(52,16),(53,17)]


rel_discover =  [(54,19),(54,20),(55,21),(56,22),
                (56,23),(56,24),(56,25),(57,26),(57,34),
                (58,27),(58,29),(59,28),(59,30),(59,31),
                (59,33),(60,32),(61,35),(62,35),(63,36),
                (64,37),(64,38),(64,49),(64,50),(65,39),
                (65,40),(65,41),(65,44),(66,42),(67,43),
                (68,45),(69,46),(69,52),(70,47),(70,48),
                (70,51)]
{-
||----------------------------------------------------------------------------
|| FUNCTIONS USED TO GENERATE OUTPUT STRINGS
||----------------------------------------------------------------------------
-}

yesno x = if x then "yes." else "no"


whowhatq x y   = if x == "anim" then (whoq y) else (whatq y)

whoq xs        = check "no one that i know of." [name_of e | e <- s]
                 where s = intersect (set_of_men ++ set_of_woman) xs

whatq xs       = check "nothing." [name_of e | e <- xs]

whichq xs ys   = check "none." [name_of e | e <- intersect xs ys]

howmanyq xs ys = show (length (intersect xs ys) )


check str x    = if x == [] then str else (unwords x)


name_of e      = if names /= [] then (head names)  else show e
                  where names = [x | (x,y) <- name_list, y == e]

number n       = show n

{-
||--------------------------------------------------------------------------------
|| LIST OF NAMES FOR OUTPUT
||--------------------------------------------------------------------------------
-}
name_list = 
 [("bernard",    55),("bond",       67),("venus",      10), 
  ("cassini",    65),("dollfus",    63),("Fouuntain",   62), 
  ("galileo",    56),("hall",       54),("herschel",   64),
  ("huygens",    66),("kowal",      57),("kuiper",     69), 
  ("larsen",     61),("lassell",    70),("melotte",    60), 
  ("nicholson",  59),("perrine",    58),("pickering",  68), 
  ("almathea",   21),("ariel",      47),("callisto",   25), 
  ("charon",     53),("deimos",     20),("dione",      40), 
  ("earth",      11),("enceladus",  38),("europa",     23), 
  ("ganymede",   24),("hyperion",   43),("iapetus",    44), 
  ("io",         22),("janus",      36),("jupiter",    13), 
  ("jupitereighth",      32),("jupitereleventh",    31), 
  ("jupiterfourteenth",  34),("jupiterninth",       33), 
  ("jupiterseventh",     29),("jupitersixth",       27),
  ("jupitertenth",       28),("jupiterthirteenth",  26), 
  ("jupitertwelfth",     30),("luna",               18), 
  ("mars",               12),("mercury",             9), 
  ("mimas",              37),("miranda",            46), 
  ("neptune",            16),("nereid",             52), 
  ("oberon",             50),("phobos",             19), 
  ("phoebe",             45),("pluto",              17), 
  ("rhea",               41),("saturn",             14), 
  ("saturnfirst",        35),("sol",                 8), 
  ("tethys",             39),("titan",              42), 
  ("titania",            49),("triton",             51), 
  ("umbriel",            48),("uranus",             15) ]


test1 p p_ inp = do putStr  $ render80 $ format{-Atts p_-} $ snd $ unState (p T0 [] ((1,[]),words inp) ([],[])) [] 
test p input = unState (p ((1,[]),input) ([],[])) [] 



main  i = formatAttsFinalAlt Question  ((length (words i))+1) $ snd $ test (question T0 []) (words i)


findStart st ((s,ss):rest) | s == st   = [(s,ss)]
                           | otherwise = findStart st rest
findStart st []                        = []     

input = words i1

i0 = "1 + 2 * 5"

i1 = "which moons that were discovered by hall orbit mars" -- OK -- {-of the-}
i2 = "who discovered a moon that orbits mars" -- OK
i3 = "did hall discover every moon that orbits mars" -- OK 
i4 = "how many moons were discovered by hall and kuiper" -- OK
i5 = "how many moons were discovered by hall or kuiper" -- OK
i6 = "every moon was discovered by a man" --OK
i7 = "which planets are orbited by a moon that was discovered by galileo" --OK
i8 = "which moons were discovered by nobody" --OK
i9 = "is every planet orbited by a moon" --NO -- "is" is not recognized throuth any rule
i10 = "which planets are orbited by two moons" --OK
i11 = "who was the discoverer of phobos" --GIVES WHOLE BUNCH OF NAMES
i12 = "hall discovered a moon that orbits mars" --OK
i13 = "which moons that orbit mars were discovered by hall" -- OK -- {-of the-}
i14 = "every moon orbits every planet" -- OK
i15 = "every planet is orbited by a moon" --OK
i16 = "a planet is orbited by a moon" --OK
i17 = "does phobos orbit mars" -- OK
--
i18 = "did hall discover deimos or phobos and miranda"
i19 = "did hall discover deimos or phobos and miranda or deimos and deimos"
-- "1 + 2 * 5" -- "1 5 2 3 2" -- 1 5 2 3 2 1 5 2 3 2" 


setsToTriples pairs eventNo = concatMap (\a -> (show a) ++ ",\n") triples
	where
	triples = setsToTriples_ pairs eventNo

setsToTriples_ :: [([Integer], String)] -> Int -> [(String, String, String)]
setsToTriples_ [] eventNo = []
setsToTriples_ ((set, name):sets) eventNo = (setToTriple set name eventNo) ++ (setsToTriples_ sets (eventNo + (length set)))

setToTriple set setName eventNo = concatMap (\(s, n) ->
		[("event" ++ show(n + eventNo), "subject",s),
		("event" ++ show(n + eventNo), "type", "membership"),
		("event" ++ show(n + eventNo), "object", setName)]) zipSet
		where
			zipSet = zip (map fst $ filter (\(s, n) -> n `elem` set) name_list) [1..]
