//  SPDX-License-Identifier: MIT
/*
 *  Copyright (c) 2025 Andrew Scott-Jones <andrew@edgehammer.io>
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

use std::rc::Rc;

use crate::ir::TAC;

#[derive(Debug, PartialEq, Clone)]
pub enum Code {
    Function { name: String, code: Vec<Rc<Code>> },
    Ret,
    Imm(i64),
}

fn transform(node: Rc<TAC>, mut asm_code: &mut Vec<Rc<Code>>) {
    match &*node {
        TAC::Function { name, code } => {
            let mut func_asm_code: Vec<Rc<Code>> = vec![];

            for instr in code {
                transform(instr.clone(), &mut func_asm_code);
            }

            asm_code.push(Rc::new(Code::Function {
                name: name.clone(),
                code: func_asm_code,
            }));
        }

        TAC::ConstInt(val) => {
            asm_code.push(Rc::new(Code::Imm(*val)));
        }

        TAC::Return(expr) => {
            transform(expr.clone(), &mut asm_code);
            asm_code.push(Rc::new(Code::Ret));
        }
    }
}

pub fn generate(tac_code: Vec<Rc<TAC>>) -> Vec<Rc<Code>> {
    let mut asm_code: Vec<Rc<Code>> = vec![];

    for tac in tac_code {
        transform(tac.clone(), &mut asm_code);
    }
    return asm_code;
}
