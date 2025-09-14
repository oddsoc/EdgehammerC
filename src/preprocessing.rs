//  SPDX-License-Identifier: MIT
/*
 *  Copyright (c) 2025 Andrew Scott-Jones
 *
 *  Permission is hereby granted, free of charge, to any person obtaining a
 *  copy of this software and associated documentation files (the "Software"),
 *  to deal in the Software without restriction, including without limitation
 *  the rights to use, copy, modify, merge, publish, distribute, sublicense,
 *  and/or sell copies of the Software, and to permit persons to whom the
 *  Software is furnished to do so, subject to the following conditions:
 *
 *  The above copyright notice and this permission notice shall be included in
 *  all copies or substantial portions of the Software.
 *
 *  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
 *  OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 *  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 *  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 *  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 *  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 *  DEALINGS IN THE SOFTWARE.
 */

#[cfg(feature = "simd")]
use std::simd::prelude::*;
use std::string::FromUtf8Error;

#[cfg(feature = "simd")]
pub fn preprocess(buf: Vec<u8>) -> Result<String, FromUtf8Error> {
    decode_and_strip_continuations_simd(buf)
}

#[cfg(not(feature = "simd"))]
pub fn preprocess(buf: Vec<u8>) -> Result<String, FromUtf8Error> {
    decode_and_strip_continuations(buf)
}

#[cfg(not(feature = "simd"))]
fn decode_and_strip_continuations(
    mut buf: Vec<u8>,
) -> Result<String, FromUtf8Error> {
    let mut i = 0;
    let mut o = 0;

    while i < buf.len() {
        if buf[i] == b'\\' {
            if i + 1 < buf.len() && buf[i + 1] == b'\n' {
                i += 2;
                continue;
            }
            if i + 2 < buf.len() && buf[i + 1] == b'\r' && buf[i + 2] == b'\n' {
                i += 3;
                continue;
            }
        }

        buf[o] = buf[i];
        i += 1;
        o += 1;
    }

    buf.truncate(o);
    String::from_utf8(buf)
}

#[cfg(feature = "simd")]
fn decode_and_strip_continuations_simd(
    mut buf: Vec<u8>,
) -> Result<String, FromUtf8Error> {
    let mut i = 0;
    let mut o = 0;
    let len = buf.len();

    while i < len {
        let chunk_len = (len - i).min(32);

        let mask = if chunk_len == 32 {
            let chunk = u8x32::from_slice(&buf[i..i + 32]);
            chunk.simd_eq(u8x32::splat(b'\\')).to_bitmask()
        } else {
            1
        };

        if mask != 0 {
            let mut j = 0;
            while j < chunk_len {
                let pos = i + j;

                if buf[pos] == b'\\' {
                    if pos + 1 < len && buf[pos + 1] == b'\n' {
                        i = pos + 2;
                        break;
                    }
                    if pos + 2 < len
                        && buf[pos + 1] == b'\r'
                        && buf[pos + 2] == b'\n'
                    {
                        i = pos + 3;
                        break;
                    }
                }

                buf[o] = buf[pos];
                o += 1;
                j += 1;
            }

            if j == chunk_len {
                i += chunk_len;
            }
        } else {
            buf.copy_within(i..i + 32, o);
            i += 32;
            o += 32;
        }
    }

    buf.truncate(o);
    String::from_utf8(buf)
}
