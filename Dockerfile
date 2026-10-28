FROM haskell:9.14.1-bookworm
WORKDIR /codegen
COPY dhscanner.cabal dhscanner.cabal
RUN cabal update
RUN cabal build --only-dependencies
COPY src src
RUN cabal build --ghc-options="-optl-static -optl-pthread"
CMD ["cabal", "run"]
