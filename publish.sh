#!/usr/bin/env sh

git checkout master
cabal run my-site -- clean
cabal run my-site -- build
git fetch --all
git checkout -b gh-pages --track origin/gh-pages
cp -a _site/. .

git add .
git commit -m "publish"
git push origin gh-pages
git checkout master
git branch -D gh-pages

echo "site published"
