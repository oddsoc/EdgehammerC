#!/bin/bash

COMPLETE=15
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
   test_compiler $CC --chapter $CHAPTER --stage $STAGE --extra-credit --latest-only
   echo
   echo "Testing $CC against all tests <= chapter $COMPLETE previously completed" 
   test_compiler $CC --chapter $COMPLETE --extra-credit
fi

# Writing a C Compiler does not cover all the ways arrays can be declared but we
# do so make sure that the compiler can validate these sloppy array declarations.
$CC --validate tests/array_decls.c

