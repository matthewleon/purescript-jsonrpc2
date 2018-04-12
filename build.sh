#!/bin/sh

# exit when any command fails
set -e

DEP_DIR="bower_components"
if [ ! -d ${DEP_DIR} ]; then
  BOWER=${BOWER:-$(command -v bower)}
  if [ -z ${BOWER} ]; then
    >&2 echo "build failed: please install bower (https://bower.io/#install-bower)"
    exit 1
  fi
  ${BOWER} install --production
fi

PURS=${PURS:-$(command -v purs)}
if [ -z ${PURS} ]; then
  >&2 echo "build failed: please install purescript (http://www.purescript.org/)"
  exit 1
fi

SCRIPT_REL_PATH='src/**/*.purs'
DEPS="${DEP_DIR}/purescript-*/${SCRIPT_REL_PATH}"
SRC="./${SCRIPT_REL_PATH}"
${PURS} compile "${DEPS}" "${SRC}"
