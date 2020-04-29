#!/usr/bin/env sh

git checkout master
stack exec my-site clean
stack exec my-site build
git fetch --all
git checkout -b gh-pages --track origin/gh-pages
cp -a _site/. .

git add .
git commit -m "publish"
git push origin gh-pages
git checkout master
git branch -D gh-pages

echo "site published"
