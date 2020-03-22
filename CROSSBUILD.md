For cross-compiling, build with:

 cabal v2-build --allow-newer --with-ghc=armv7a-hardfloat-linux-gnueabi-ghc --with-ghc-pkg=armv7a-hardfloat-linux-gnueabi-ghc-pkg

FastCGI needs libfcgi -- can't statically link it with glibc due to getaddrinfo but could do that with musl

For wasm:

* Install stack
* Add `~/.local/bin` to your `PATH`, but only AFTER `~/.cabal/bin`, otherwise `cabal` will refer to Stack's cabal, which
    is significantly older.
* Pull down latest Asterius and build it with `stack build asterius`
* Boot Asterius with `stack exec ahc-boot` -- note that `stack exec` is needed for `ahc-` related activities
* Navigate to XSaiga and `stack exec ahc-cabal v2-configure -- --disable-shared --allow-newer --constraint "cryptonite
    -integer-gmp"`
* If the dependencies don't work out, make sure it's booted.
* Now do `stack exec ahc-cabal v2-build -- --disable-shared --allow-newer --constraint "cryptonite -integer-gmp"`
* If you get build errors from things like `x509`, and notice older versions, put newer versions in `asterius-deps/*`
    and try again
* If you get build errors from things like `html-entities` where the compiler itself breaks, pass `--disable-shared` to
    `ahc-cabal`.  This is because WASM builds have no concept of dynamic linking and there's no graceful handling in
    Asterius to handle this.  `ahc-cabal` is supposed to fix that, but I think the `-dynamic-too` flag trips it up.
* Some notes:
  * I had to add cross-deps/hs-certificate/{x509,x509-store,x509-system,x509-util,x509-validation}, latest versions from
      Git
  * I also had to add html-entities from latest git, but I'm not sure why this worked.  Now it stopped working again.
      Argh!  For some reason I built it once with `-v` and it worked.  Bleh.  Okay.  Keep trying with `-v`.  It will
      work.
  * I had to add `--constraint "cryptonite -integer-gmp"` to the command line for cryptonite to work
  * I got `html-entities` building after adding `--disable-shared` to the Cabal command line as shown above
