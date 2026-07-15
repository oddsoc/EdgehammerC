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

use crate::mir::abi::Abi;

pub struct SysvAbi;

impl Abi for SysvAbi {
    fn pointer_size(&self) -> usize {
        8
    }
    fn pointer_alignment(&self) -> usize {
        8
    }
    fn int_size(&self) -> usize {
        4
    }
    fn int_alignment(&self) -> usize {
        4
    }
    fn long_size(&self) -> usize {
        8
    }
    fn long_alignment(&self) -> usize {
        8
    }
    fn long_long_size(&self) -> usize {
        8
    }
    fn long_long_alignment(&self) -> usize {
        8
    }
    fn double_size(&self) -> usize {
        8
    }
    fn double_alignment(&self) -> usize {
        8
    }
    fn long_double_size(&self) -> usize {
        16
    }
    fn long_double_alignment(&self) -> usize {
        16
    }
    fn void_size(&self) -> usize {
        1
    }
    fn void_alignment(&self) -> usize {
        1
    }
    fn function_size(&self) -> usize {
        1
    }
    fn function_alignment(&self) -> usize {
        8
    }
    fn stack_alignment(&self) -> usize {
        16
    }
    fn max_gp_arg_regs(&self) -> usize {
        6
    }
    fn max_fp_arg_regs(&self) -> usize {
        8
    }
}
