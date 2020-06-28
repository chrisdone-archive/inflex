#!/bin/bash

set -e
set -x

stack build psc-package

stack exec -- psc-package build

stack exec -- purs bundle 'output/**/*.js' -m Main --main Main -o app.js
