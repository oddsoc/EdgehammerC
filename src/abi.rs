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

use std::sync::OnceLock;

pub trait Abi: Send + Sync {
    fn pointer_size(&self) -> usize;
    fn pointer_alignment(&self) -> usize;
    fn int_size(&self) -> usize;
    fn int_alignment(&self) -> usize;
    fn long_size(&self) -> usize;
    fn long_alignment(&self) -> usize;
    fn long_long_size(&self) -> usize;
    fn long_long_alignment(&self) -> usize;
    fn double_size(&self) -> usize;
    fn double_alignment(&self) -> usize;
    fn long_double_size(&self) -> usize;
    fn long_double_alignment(&self) -> usize;
    fn void_size(&self) -> usize;
    fn void_alignment(&self) -> usize;
    fn function_size(&self) -> usize;
    fn function_alignment(&self) -> usize;
    fn stack_alignment(&self) -> usize;
    fn max_gp_arg_regs(&self) -> usize;
    fn max_fp_arg_regs(&self) -> usize;
}

static ABI: OnceLock<&'static dyn Abi> = OnceLock::new();

pub fn set_abi(abi: &'static dyn Abi) {
    ABI.set(abi).ok();
}

pub fn abi() -> &'static dyn Abi {
    *ABI.get()
        .expect("ABI not initialized; call abi::set_abi() first")
}
