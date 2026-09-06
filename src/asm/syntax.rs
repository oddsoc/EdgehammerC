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

pub const OPERAND_COL: usize = 8;

pub fn pad_inst(s: &str) -> String {
    let rest = s.strip_prefix('\t').unwrap_or(s);
    let mut out = String::with_capacity(s.len() + 12);
    out.push('\t');
    match rest.find([' ', '\t']) {
        Some(i) if i > 0 => {
            out.push_str(&rest[..i]);
            let pad = OPERAND_COL.saturating_sub(i).max(1);
            for _ in 0..pad {
                out.push(' ');
            }
            out.push_str(&rest[i..].trim_start_matches([' ', '\t']));
        }
        _ => out.push_str(rest),
    }
    out
}
