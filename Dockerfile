FROM haskell:9.14.1-bookworm
WORKDIR /codegen
# Base image is the latest official Haskell tag ( GHC 9.14.1 on Debian 12 ).
# Bumped from haskell:9.6.7 ( bullseye = oldstable ) to escape a recurring
# CI failure : the bullseye-security Fastly edge was returning 404 for
# a fixed set of .debs ( libxml2 +deb11u10 , libarchive13 +deb11u5 ,
# libicu67 +deb11u1 ) across every workaround we tried ( two-RUN split ,
# single-RUN merge , 3x retry loop , snapshot.debian.org pin ). Bookworm
# has an independent, currently-consistent apt path.
COPY dhscanner.cabal dhscanner.cabal
RUN cabal update
RUN cabal build --only-dependencies
COPY src src
RUN cabal build
CMD ["cabal", "run"]
