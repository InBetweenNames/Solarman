Build with:

 cabal v2-build --allow-newer --with-ghc=armv7a-hardfloat-linux-gnueabi-ghc --with-ghc-pkg=armv7a-hardfloat-linux-gnueabi-ghc-pkg

FastCGI needs libfcgi -- can't statically link it with glibc due to getaddrinfo but could do that with musl
