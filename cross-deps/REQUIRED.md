The following packages need to be modified for GHC 8.8.1.  Some need MonadFail instances, and some need constraints
on dependencies relaxed (base, time, etc).  You can use `cabal v2-configure --allow-newer` for the constraint relaxation
cases.  The MonadFail cases need to be altered by hand.

aeson
asn1-encoding
attoparsec
cgi
hslogger
memory
MissingH
multipart
regex-base
regex-posix
socks

The following packages need to be modified for cross compilation to change their cabal setup type from Custom to Simple:
comonad
distributive
