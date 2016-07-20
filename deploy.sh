#!/bin/bash

set -o errexit -o nounset

rev=$(git rev-parse --short HEAD)

git init
git config user.name "Jeremie Jost"
git config user.email "jeremiejost@gmail.com"

git remote add upstream "https://$GH_TOKEN@github.com/jjst/elmtimate-tictactoe.git"
git fetch upstream
git reset upstream/gh-pages


git add -A tictactoe.js index.html style.css
git commit -m "Rebuild pages at ${rev}"
git push -q upstream HEAD:gh-pages
