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

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use std::sync::atomic::{AtomicUsize, Ordering};

use crate::ir::TAC;

macro_rules! code_rc {
    ($variant:ident) => {
        Rc::new(RefCell::new(Code::$variant))
    };

    ($variant:ident ( $($args:expr),* $(,)? )) => {
        Rc::new(RefCell::new(Code::$variant( $($args),* )))
    };

    ($variant:ident { $($field:ident : $value:expr),* $(,)? }) => {
        Rc::new(RefCell::new(Code::$variant {
            $($field: $value),*
        }))
    };
}

#[derive(Debug, PartialEq, Clone)]
pub enum Code {
    Function {
        name: String,
        stack: usize,
        code: CodeVec,
    },
    Ret,
    Mov {
        src: CodeRef,
        dst: CodeRef,
    },
    Not {
        dst: CodeRef,
    },
    Neg {
        dst: CodeRef,
    },
    Imm(i64),
    Var(usize),
    Reg(usize),
    Tmp,
    Stack(usize),
}

type CodeRef = Rc<RefCell<Code>>;
type CodeVec = Vec<CodeRef>;
type VarMap = HashMap<usize, CodeRef>;

static STACK_POS: AtomicUsize = AtomicUsize::new(0);

fn incr_stack_pos(by: usize) -> usize {
    STACK_POS.fetch_add(by, Ordering::SeqCst);
    STACK_POS.load(Ordering::SeqCst)
}

fn reset_stack_pos() {
    STACK_POS.store(0, Ordering::SeqCst)
}

fn create_var(idx: usize, var_map: &mut VarMap) -> CodeRef {
    var_map
        .entry(idx)
        .or_insert_with(|| code_rc!(Var(idx)))
        .clone()
}

fn create_mov(src: CodeRef, dst: CodeRef, code_vec: &mut CodeVec) {
    if matches!(*src.borrow(), Code::Reg(_))
        || matches!(*dst.borrow(), Code::Reg(_))
        || matches!(*src.borrow(), Code::Imm(_))
        || matches!(*dst.borrow(), Code::Imm(_))
    {
        let mov = code_rc!(Mov {
            src: src,
            dst: dst.clone(),
        });
        code_vec.push(mov);
    } else {
        let tmp = code_rc!(Tmp);
        let mut mov = code_rc!(Mov {
            src: src,
            dst: tmp.clone(),
        });
        code_vec.push(mov);
        mov = code_rc!(Mov { src: tmp, dst: dst });
        code_vec.push(mov);
    }
}

fn transform_expr(
    node: Rc<TAC>,
    var_map: &mut VarMap,
    code_vec: &mut CodeVec,
) -> CodeRef {
    match &*node {
        TAC::ConstInt(val) => {
            return code_rc!(Imm(*val));
        }

        TAC::Var(val) => {
            return create_var(*val, var_map);
        }

        TAC::Not { src, dst } => {
            let src = transform_expr(src.clone(), var_map, code_vec);
            let dst = transform_expr(dst.clone(), var_map, code_vec);
            create_mov(src, dst.clone(), code_vec);
            return code_rc!(Not { dst: dst.clone() });
        }

        TAC::Neg { src, dst } => {
            let src = transform_expr(src.clone(), var_map, code_vec);
            let dst = transform_expr(dst.clone(), var_map, code_vec);
            create_mov(src, dst.clone(), code_vec);
            return code_rc!(Neg { dst: dst.clone() });
        }

        _ => {
            unreachable!()
        }
    }
}

fn transform(node: Rc<TAC>, var_map: &mut VarMap, code_vec: &mut CodeVec) {
    match &*node {
        TAC::Function { name, code } => {
            let mut func_code_vec: CodeVec = vec![];

            for instr in code {
                transform(instr.clone(), var_map, &mut func_code_vec);
            }

            code_vec.push(code_rc!(Function {
                name: name.clone(),
                stack: 0, // we do not know this until fixup pass
                code: func_code_vec,
            }));
        }

        TAC::Not { src: _, dst: _ } => {
            let not = transform_expr(node.clone(), var_map, code_vec);
            code_vec.push(not);
        }

        TAC::Neg { src: _, dst: _ } => {
            let neg = transform_expr(node.clone(), var_map, code_vec);
            code_vec.push(neg);
        }

        TAC::Return(expr) => {
            let src = transform_expr(expr.clone(), var_map, code_vec);
            let dst = code_rc!(Reg(0));
            code_vec.push(code_rc!(Mov {
                src: src.clone(),
                dst: dst.clone(),
            }));
            code_vec.push(code_rc!(Ret));
        }

        _ => {
            unreachable!();
        }
    }
}

fn fixup(code: CodeRef, var_map: &mut VarMap) {
    let op = &mut *code.borrow_mut();
    match op {
        Code::Function {
            name: _,
            stack,
            code,
        } => {
            reset_stack_pos();

            for op in code {
                fixup(op.clone(), var_map);
            }

            *stack = incr_stack_pos(0);
        }

        Code::Mov { src, dst } => {
            fixup(src.clone(), var_map);
            fixup(dst.clone(), var_map);
        }

        Code::Not { dst } => {
            fixup(dst.clone(), var_map);
        }

        Code::Neg { dst } => {
            fixup(dst.clone(), var_map);
        }

        Code::Var(_idx) => {
            *op = Code::Stack(incr_stack_pos(4)).try_into().unwrap();
        }

        _ => {}
    }
}

pub fn generate(tac_code: Vec<Rc<TAC>>) -> CodeVec {
    let mut code_vec: CodeVec = vec![];
    let mut var_map = VarMap::new();

    for tac in tac_code {
        transform(tac.clone(), &mut var_map, &mut code_vec);
    }

    for asm in &code_vec {
        fixup(asm.clone(), &mut var_map);
    }

    return code_vec;
}
