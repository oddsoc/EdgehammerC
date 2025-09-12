#!/bin/bash

CHAPTER=$1
STAGE=$2

CC=target/release/ehc

if [ ! -f $CC ]; then
   cargo build
   CC=target/debug/ehc
else
   cargo build -r
fi

echo "Running tests..."

if [ "$STAGE" == "" ]; then
   echo "Testing $CC against all tests <= chapter $CHAPTER"
   test_compiler $CC --chapter $CHAPTER --extra-credit
else
   echo "Testing $CC against all tests <= chapter $CHAPTER up to $STAGE stage"
   test_compiler $CC --chapter $CHAPTER --stage $STAGE --extra-credit
fi
