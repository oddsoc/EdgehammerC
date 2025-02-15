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

use crate::parser::AST;

#[derive(Debug, PartialEq, Clone)]
pub enum TAC {
    Function { name: String, code: Vec<Rc<TAC>> },
    ConstInt(i64),
    Return(Rc<TAC>),
}

fn transform_expr(expr: Rc<AST>, _tac_code: &mut Vec<Rc<TAC>>) -> Rc<TAC> {
    match &*expr {
        AST::ConstInt(val) => {
            return Rc::new(TAC::ConstInt(*val));
        }

        _ => {
            unreachable!();
        }
    }
}

fn transform(node: Rc<AST>, tac_code: &mut Vec<Rc<TAC>>) {
    match &*node {
        AST::Function {
            name,
            params: _,
            stmts,
            rtype: _,
        } => {
            let mut func_code: Vec<Rc<TAC>> = vec![];

            for stmt in stmts {
                transform(stmt.clone(), &mut func_code);
            }

            tac_code.push(Rc::new(TAC::Function {
                name: name.clone(),
                code: func_code,
            }));
        }

        AST::ConstInt(val) => {
            tac_code.push(Rc::new(TAC::ConstInt(*val)));
        }

        AST::Return { expr } => {
            let e = transform_expr(expr.clone(), tac_code);
            tac_code.push(Rc::new(TAC::Return(e)));
        }

        _ => {
            unreachable!();
        }
    }
}

pub fn generate(ast: Vec<Rc<AST>>) -> Vec<Rc<TAC>> {
    let mut code: Vec<Rc<TAC>> = vec![];

    for node in ast {
        transform(node, &mut code);
    }

    return code;
}
