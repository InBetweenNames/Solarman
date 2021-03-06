# Solarman with Semantic Web Triplestore Backend

Files:
====

* AGParser.hs -- Need this for parsing (it's from Solarman 1)

* TypeAG.hs -- Datatypes for above

* NatLangSemApplication.hs -- Most recent official version of Solarman

* Getts.hs -- Library for doing Triple Store stuff, mostly identical to what is discussed in paper.  Avoids semantics.

* SolarmanTriplestore.hs -- Solarman with triple store backend

* InteractiveGenerator.hs -- Generates Interactive.hs based on dictionary

* Interactive.hs -- An interface suitable for running queries over the internet in a safe environment.  Uses SafeHaskell and NoImplicitPrelude to prevent arbitrary code from running.  This should be run as an isolated user just in case! 

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

To use the natural language frontend, a CGI version is provided in Main.hs which can be compiled to an executable (solarman.cgi) which is conformant to the CGI standard

To do web queries directly with the combinators, Interactive.hs can be used.  Interactive.hs allows for direct queries to be performed using "-e" and has convenient variables for each thing in the dictionary. For instance, the following is possible:

~~~
ghci util/Interactive.hs -package XSaiga -e ":set -XSafe" -e ":t discover"
~~~

This module uses SafeHaskell to export only "safe" functions suitable for use in a web query.  That is, to our best knowledge, Interactive.hs cannot be used to execute arbitrary code on a server.  Only the provided combinators may be used, and in addition the ($) and (.) operators.

Note that a workaround is in place until [this commit](https://gitlab.haskell.org/ghc/ghc/commit/7cdcd3e12a5c3a337e36fa80c64bd72e5ef79b24) lands
into a proper GHC release.  Once that is done, it should be possible to directly do:

~~~
ghci util/Interactive.hs -package XSaiga -XSafe -e ":t discover"
~~~

Disabling the implicit prelude and enabling XSafe ensures that no System.IO* functions are available to the query.

To use Interactive.hs, you should install this package and move it to a different directory.

Building
===

To build, use `cabal v2-build` or alternatively use `stack build`.

If you want to use `Interactive.hs`, make sure you have `write-ghc-environment-files: always` set in your
`cabal.project.local` before building.

Note that the `Interactive.hs` instructions will change once the previously mentioned GHC bug is fixed in a release.

For cross-compilation, see `CROSSBUILD.md`.

Note that for GHC 8.8.1, you will need to use `--allow-newer` and use patched versions of dependencies that have
instances for `MonadFail` where needed, as `fail` inside `Monad` was officially removed with this release.
This is not a bug with XSaiga or Solarman, it's just something the Haskell ecosystem will have to work through
until it's all fixed.
