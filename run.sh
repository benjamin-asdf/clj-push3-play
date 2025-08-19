#!/bin/sh

cd "$(dirname "$0")"

source ./activate.sh 

clojure "$@"

