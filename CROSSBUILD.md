NOTE: integer-simple builds seem to cause problems with x509 for some reason.  Not sure why.  Use `integer-gmp`.
NOTE: ghc spits out warnings about using newer LLVM versions, but these don't seem to actually cause problems
NOTE: ghc-8.10.1 seems to emit binaries that segfault immediately on arm, emitting undefined instructions.  Is this related to
the wasm problem?  Wasm uses ghc-8.8.3 though
NOTE: ghc-8.8.3 seems to work just fine on arm

For cross-compiling with glibc, build with:

 cabal v2-build --allow-newer --with-ghc=armv7a-hardfloat-linux-gnueabi-ghc --with-ghc-pkg=armv7a-hardfloat-linux-gnueabi-ghc-pkg

NOTE: FastCGI needs libfcgi -- can't statically link it with glibc due to getaddrinfo.
[More info] (https://news.ycombinator.com/item?id=9317211) -- name resolution libraries are loaded dynamically,
statically linking these is tricky.  In principle it shouldn't matter, but whatever.

However, you can do that with musl!

For cross-compiling with musl, build with:

  cabal v2-build --allow-newer --with-ghc=armv7a-hardfloat-linux-musleabi-ghc --with-hc-pkg=armv7a-hardfloat-linux-musleabi-ghc-pkg --enable-executable-static

Note the --enable-executable-static, this is because musl can be statically linked, so we may as well take advantage of
that.  You can even run these executables on glibc systems.  Very nice.

NOTE: if you get a build error, try passing --with-gcc=armv7a-hardfloat-linux-musleabi-gcc.  Also, delete your package
cache for the problematic packages, like cryptonite.  cabal doesn't seem to be aware of libc changes, so it just groups
all packages for the same ghc-version/arch under the same directory.  This is probably a bug in cabal.

To build for wasm (using Asterius):

* Install stack
* Add `~/.local/bin` to your `PATH`, but only AFTER `~/.cabal/bin`, otherwise `cabal` will refer to Stack's cabal, which
    is significantly older.
* Pull down latest Asterius and build it with `stack build asterius`
* Boot Asterius with `stack exec ahc-boot` -- note that `stack exec` is needed for `ahc-` related activities
* Navigate to XSaiga and `stack exec ahc-cabal v2-configure -- --disable-shared --allow-newer --constraint "cryptonite
    -integer-gmp" -fasterius`.  The `-fasterius` is important to enable the Asterius workarounds.
* If the dependencies don't work out, make sure it's booted.
* Now do `stack exec ahc-cabal -- v2-build --disable-shared --allow-newer --constraint "cryptonite -integer-gmp" -fasterius`
* If you get build errors from things like `x509`, and notice older versions, put newer versions in `asterius-deps/*`
    and try again
* If you get build errors from things like `html-entities` where the compiler itself breaks, pass `--disable-shared` to
    `ahc-cabal`.  This is because WASM builds have no concept of dynamic linking and there's no graceful handling in
    Asterius to handle this.  `ahc-cabal` is supposed to fix that, but I think the `-dynamic-too` flag trips it up.
* Some notes:
  * I had to add cross-deps/hs-certificate/{x509,x509-store,x509-system,x509-util,x509-validation}, latest versions from
      Git
  * I also had to add `html-entities` from latest git, but I'm not sure why this worked.  Now it stopped working again.
      Argh!  For some reason I built it once with `-v` and it worked.  Bleh.  Okay.  Keep trying with `-v`.  It will
      work.
  * I had to add `--constraint "cryptonite -integer-gmp"` to the command line for cryptonite to work
  * I had to add `hsc2hs` and a few other things (listed under `asterius-deps` in `cabal.project`).
  * One of my workarounds for hsparql involves a small edit to the code.  I'll publish that in a PR later.  This isn't used right now so no biggie.


To run:

~~~
stack exec ahc-dist -- --browser --input-mjs solarman.mjs --input-exe dist-newstyle/build/x86_64-linux/ghc-8.8.3/XSaiga-1.6.1.0/x/solarman.cgi/build/solarman.cgi/solarman.cgi
~~~

Change `--browser` to `--run` if you want to run it on the command line.  `--run` doesn't work just yet.

The in-store version works fine, but is slow. It is self contained, requiring no network or SPARQL dependencies.
It works with both Asterius and native code generation.

The Asterius version has Javascript-based implementations for the getts* functions.  Originally, it was only
going to have a marshalling function, but it seems that `createSelectQuery` from `hsparql` crashes with Asterius
for some reason.  I took this as an opportunity to just do the entire backend in JS.  No XML dependencies
for Asterius!!  But this isn't reflected in the cabal stuff yet -- need to make hsparql a conditional thing.
The slowest part is the reduced triplestore right now.

Note that the CGI interface doesn't work, and I can't seem to get functions to export other than `main`.
But that's okay -- the approach we'll take for the demo is to define three JS functions: getQuery(), setQuery() and updateResults(),
for input and output respectively, in Javascript.  Probably a fourth doQuery() function too.
getQuery() is an async function that just returns a global variable, input_query, which is updated via the interface using setQuery() (or doQuery()).
updateResults() takes in a JSON object as used in XSaiga.CGI to report
results and updates the result section with it, formatting as normal.  When we want to make a query, first update
`input_query`, and then call `main`.  `main` will then callback to JS using `getQuery` to get the query string,
execute the query, and then will call `updateResults` with the newly formed JSON object.

Now to make the reduced triplestore a bit faster and improve the speed in general.  So far, the SPARQL queries are sent out over the network!!!
Exciting times.



