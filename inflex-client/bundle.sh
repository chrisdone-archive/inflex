#!/bin/bash

# set -e
# set -x

# echo Skipping psc-package build
# time psc-package build

echo Bundling...
#time purs bundle 'output/**/*.js' -m Main --main Main -o app.js
psc-bundle-fast -i output -m Main --main Main -o app.js

# echo Refreshing...
# xdotool search --onlyvisible --name InflexApp windowactivate --sync key  --delay 0 ctrl+shift+r search --onlyvisible --name Emacs windowactivate
