#!/bin/sh

NPM=${NPM:=$(command -v npm)}
if [ -z $NPM ]; then
  >&2 echo 'build failed: please install nodejs and npm (https://nodejs.org/)'
  exit 1
fi
"$NPM" install

BOWER=$(npm bin)/bower
"$BOWER" install --production || exit 1

DEP_DIR="bower_components"
PURS=$(npm bin)/psa

SCRIPT_REL_PATH='src/**/*.purs'
DEPS="${DEP_DIR}/purescript-*/${SCRIPT_REL_PATH}"
SRC="./${SCRIPT_REL_PATH}"
"$PURS" compile "$DEPS" "$SRC"
