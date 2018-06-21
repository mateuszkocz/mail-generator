#!/bin/sh

# Create the production index.html.
cp src/index.html index.html
sed -i .bk 's/\.temp\/app\.js/dist\/app\.js/g' index.html
sed -i .bk 's/ports\.js/dist\/ports\.js/g' index.html
rm *.bk

# Build the app.
elm-make src/Main.elm --output=dist/app.js
uglifyjs --compress --mangle --output=dist/app.js dist/app.js
uglifyjs --compress --mangle --output=dist/ports.js src/ports.js
