#!/bin/sh

npm run build
npm run uglify

echo $NEW_BASENAME

echo "Calculating SHA"
SHA=`sha256sum out/main.min.js | cut -d' ' -f1`

NEW_BASENAME="main.min.$SHA.js"

echo "Creating fingerprinted JS"
mv out/main.min.js out/$NEW_BASENAME

echo "Creating index.html"
sed -e "s/%%%src%%%/$NEW_BASENAME/" source/index.html.template >out/index.html
