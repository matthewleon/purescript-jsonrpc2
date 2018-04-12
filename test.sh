#!/bin/sh

# exit when any command fails
set -e

./build.sh

BOWER=${BOWER:-bower}
bower install

PURS=${PURS:-purs}
PURS_REL_PATH='**/*.purs'
SCRIPT_REL_PATH="src/${PURS_REL_PATH}"
DEPS="bower_components/purescript-*/${SCRIPT_REL_PATH}"
SRC="./${SCRIPT_REL_PATH}"
TEST="./test/${PURS_REL_PATH}"
${PURS} compile "${DEPS}" "${SRC}" "${TEST}"

node -e 'require("./output/Test.Main/index.js").main();'
