#!/bin/sh

./build.sh || exit 1

BOWER=${BOWER:-bower}
bower install || exit 1

PURS=${PURS:-purs}
PURS_REL_PATH='**/*.purs'
SCRIPT_REL_PATH="src/$PURS_REL_PATH"
DEPS="bower_components/purescript-*/$SCRIPT_REL_PATH"
SRC="./$SCRIPT_REL_PATH"
TEST="./test/$PURS_REL_PATH"
"$PURS" compile "$DEPS" "$SRC" "$TEST" || exit 1

node -e 'require("./output/Test.Main/index.js").main();'
