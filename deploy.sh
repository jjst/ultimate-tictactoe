#!/bin/bash

set -o errexit -o nounset -o xtrace

rev=$(git rev-parse --short HEAD)

git init
git config user.name "Jeremie Jost"
git config user.email "jeremiejost@gmail.com"

git remote add upstream "https://$GH_TOKEN@github.com/jjst/elmtimate-tictactoe.git"
git fetch upstream
git reset upstream/gh-pages

envsubst < index.html > index-new.html && mv index-new.html index.html

git add -A main.js index.html 404.html style.css
git commit --allow-empty -m "Rebuild pages at ${rev}"
git push -q upstream HEAD:gh-pages
