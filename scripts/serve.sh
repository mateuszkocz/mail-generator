#!/bin/sh

function finish {
  rm -rf "src/.temp"
}
trap finish EXIT

elm-live src/Main.elm --output=src/.temp/app.js --dir=src --debug
