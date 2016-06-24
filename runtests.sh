#!/usr/bin/env bash
readonly OUTPUT=tests.js
elm-make test/*Test.elm --yes --output $OUTPUT && node $OUTPUT
rm -f $OUTPUT
