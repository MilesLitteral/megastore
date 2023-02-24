#!/bin/sh
set -e

dir=$(mktemp -d dist-docs.XXXXXX)
upload="dist-newstyle/sdist"
#trap 'rm -r "$dir"' EXIT
#trap 'rm -r "$upload"' EXIT

# assumes cabal 2.4 or later
cabal v2-haddock --builddir="$dir" --haddock-for-hackage --enable-doc
cabal sdist 

# cabal upload -d --publish $upload/*.tar.gz
# cabal upload -d --publish $dir/*-docs.tar.gz