# Solarman with Triple Store Backend

Files:
====

* AGParser.hs -- Need this for parsing (it's from Solarman 1)

* TypeAG.hs -- Datatypes for above

* NatLangSemApplication.hs -- Most recent official version of Solarman

* Getts.hs -- Library for doing Triple Store stuff, mostly identical to what is discussed in paper.  Avoids semantics.

* SolarmanTriplestore.hs -- Solarman with triple store backend

New progress:

* Added more sets of adjectives to the triple store

* Added a convenience function to the old solarman to convert sets to triples (setsToTriples)

* Can construct queries using combinators: try this in GHCi "a moon (orbited (a planet))" it works!

* Reached full compatibility with old test cases on "main function"

TypeAG2 and AGParser2 are the versions of the parser and semantics that the new solarman uses

How to use
===

To use SolarmanTriplestore just load it up in GHCi and use the "main" function.

>main "which moons were discovered by hall"

>=>[phobos deimos]