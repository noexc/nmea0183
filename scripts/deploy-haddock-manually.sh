#!/usr/bin/env bash

cwd="$( cd "${BASH_SOURCE[0]%/*}" && pwd )"
cd "$cwd/.."
f=`mktemp -d`
git clone "git@github.com:noexc/nmea0183.git" "$f/nmea0183.git"
cabal haddock
pushd "$f/nmea0183.git"
  git checkout gh-pages && git rm -rf *
popd
mv dist/doc/html/nmea0183/* "$f/nmea0183.git/"
pushd "$f/nmea0183.git"
  git add -A
  git commit -m "Manual docs deploy."
  git push origin gh-pages
popd
rm -rf "$f"

if [ $? == 0 ]; then
  echo "*** Done: https://noexc.github.io/nmea0183/"
  exit 0
else
  echo "*** ERROR!!! Fix the above and try again."
  exit 1
fi
