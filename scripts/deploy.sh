#!/bin/sh

./scripts/build.sh

git commit -am "(Release)"
git push -u origin master