#!/bin/bash


a=$(elm-test 2>&1 || true)

set -e

! echo $a | grep -qF "Unhandled exception while running the tests"

echo $a | grep -qF "$1"