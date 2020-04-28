git stash
#!/usr/bin/env node

git checkout develop
stack exec my-site clean
stack exec my-site build
git fetch --all
git checkout -b master
cp -a _site/. .

git add .
git commit -m "publish"
git push origin master
git checkout develop
git branch -D master

echo "site published"
