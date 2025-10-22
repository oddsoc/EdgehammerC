#!/bin/sh
cargo +nightly bench --features "simd" --bench tokeniser
