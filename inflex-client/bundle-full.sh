#!/bin/bash

set -e
set -x

stack exec --allow-different-user -- psc-package build

stack exec --allow-different-user -- purs bundle 'output/**/*.js' -m Main --main Main -o app.js
