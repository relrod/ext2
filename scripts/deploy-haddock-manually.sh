#!/usr/bin/env bash

cwd="$( cd "${BASH_SOURCE[0]%/*}" && pwd )"
cd "$cwd/.."
f=`mktemp -d`
git clone git@github.com:relrod/ext2.git "$f/ext2.git"
cabal haddock
pushd "$f/ext2.git"
  git checkout gh-pages && git rm -rf *
popd
mv dist/doc/html/ext2/* "$f/ext2.git/"
pushd "$f/ext2.git"
  git add -A
  git commit -m "Manual docs deploy."
  git push origin gh-pages
popd
rm -rf "$f"

if [ $? == 0 ]; then
  echo "*** Done: http://relrod.github.io/ext2/"
  exit 0
else
  echo "*** ERROR!!! Fix the above and try again."
  exit 1
fi
