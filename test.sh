#!/bin/bash

LATEST=15
COMPLETE=$LATEST
CHAPTER=${1:-$LATEST}
STAGE=$2

CC=target/release/asjcc

build_release() {
    local features=$1
    if [ "$features" == "" ]; then
        cargo build -r 2>&1
    else
        cargo build -r --features "$features" 2>&1
    fi
}

build_debug() {
    local features=$1
    if [ "$features" == "" ]; then
        cargo build 2>&1
    else
        cargo build --features "$features" 2>&1
    fi
}

run_tests() {
    local cc=$1
    if [ "$STAGE" == "" ]; then
        echo "Testing $cc against all tests <= chapter $CHAPTER"
        test_compiler $cc --chapter $CHAPTER --extra-credit
    else
        echo "Testing $cc against all tests <= chapter $CHAPTER up to $STAGE stage"
        test_compiler $cc --chapter $CHAPTER --stage $STAGE --extra-credit --latest-only
        echo
        echo "Testing $cc against all tests <= chapter $COMPLETE previously completed"
        test_compiler $cc --chapter $COMPLETE --extra-credit
    fi

    # Writing a C Compiler does not cover all the ways arrays can be declared but we
    # do so make sure that the compiler can validate these sloppy array declarations.
    $cc --validate tests/array_decls.c
}

echo "=== Build + test without simd ==="
if [ ! -f $CC ]; then
    build_debug ""
    CC=target/debug/asjcc
else
    build_release ""
fi
run_tests $CC

echo
echo "=== Build + test with simd feature ==="
if [ ! -f $CC ]; then
    build_debug "simd"
    CC_SIMD=target/debug/asjcc
else
    build_release "simd"
    CC_SIMD=target/release/asjcc
fi
run_tests $CC_SIMD
