# Solarman Triple Store

Files:

AGParser.hs -- Need this for parsing
(it's from Solarman 1)
TypeAG.hs -- Datatypes for above
NatLangSemApplication.hs -- Most recent official version of Solarman

Getts.hs -- Library for doing Triple Store stuff, mostly identical to what is discussed in paper.  Avoids semantics.
SolarmanTriplestore.hs -- Prototype Solarman with triple store backend

New progress:
-Added more sets of adjectives to the triple store
-Added a convenience function to the old solarman to convert sets to triples (setsToTriples)
-Can construct queries using combinators: try this in GHCi "a moon (orbited (a planet))" it works!

