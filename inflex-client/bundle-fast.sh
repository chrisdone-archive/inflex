#!/bin/bash

set -e
set -x

stack exec -- psc-bundle-fast -i output -m Main --main Main -o app.js

# xdotool search --onlyvisible --name InflexApp windowactivate --sync key  --delay 0 ctrl+shift+r search --onlyvisible --name Emacs windowactivate
