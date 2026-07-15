#!/bin/sh
RUSTFLAGS="-C target-cpu=native" cargo +nightly bench --features "simd" --bench tokeniser
