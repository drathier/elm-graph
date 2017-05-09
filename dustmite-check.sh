#!/bin/bash

set -e
# transform source, fix some syntax errors
python ../transformer.py

set +e
a=$(elm-test || true)

set -e


! echo $a | grep -qF "Unhandled exception while running the tests"

echo $a | grep -qF "$1"