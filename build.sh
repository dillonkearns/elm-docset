#!/bin/bash

DATE=$(date +"%Y-%m-%d")
PKG_COUNT=$(curl https://package.elm-lang.org/search.json -H "Accept: application/json" --compressed | jq ". | length")

rm -rf doc-dist && mkdir doc-dist
cp Elm.tgz doc-dist/Elm.tgz

cp README-template.md doc-dist/README.md
sed -i "s/DATE/${DATE}/g" doc-dist/README.md
sed -i "s/PKG_COUNT/${PKG_COUNT}/g" doc-dist/README.md

cp docset-template.json doc-dist/docset.json
jq --indent 4 ".version = \"0.19.1/packages-${DATE}\"" docset-template.json > doc-dist/docset.json

