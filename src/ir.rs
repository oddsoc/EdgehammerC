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
use std::sync::atomic::{AtomicUsize, Ordering};

use crate::parser::ASTRef;
use crate::parser::ASTVec;
use crate::parser::AST;

macro_rules! tac_rc {
    ($variant:ident) => {
        Rc::new(TAC::$variant)
    };

    ($variant:ident ( $($args:expr),* $(,)? )) => {
        Rc::new(TAC::$variant( $($args),* ))
    };

    ($variant:ident { $($field:ident : $value:expr),* $(,)? }) => {
        Rc::new(TAC::$variant {
            $($field: $value),*
        })
    };
}

#[derive(Debug, PartialEq, Clone)]
pub enum TAC {
    Function {
        name: String,
        code: TACVec,
    },
    ConstInt(i64),
    Var(usize, usize),
    Not {
        src: TACRef,
        dst: TACRef,
    },
    Neg {
        src: TACRef,
        dst: TACRef,
    },
    Mul {
        lhs: TACRef,
        rhs: TACRef,
        dst: TACRef,
    },
    IDiv {
        lhs: TACRef,
        rhs: TACRef,
        dst: TACRef,
    },
    Mod {
        lhs: TACRef,
        rhs: TACRef,
        dst: TACRef,
    },
    Add {
        lhs: TACRef,
        rhs: TACRef,
        dst: TACRef,
    },
    Sub {
        lhs: TACRef,
        rhs: TACRef,
        dst: TACRef,
    },
    LShift {
        lhs: TACRef,
        rhs: TACRef,
        dst: TACRef,
    },
    RShift {
        lhs: TACRef,
        rhs: TACRef,
        dst: TACRef,
    },
    And {
        lhs: TACRef,
        rhs: TACRef,
        dst: TACRef,
    },
    Or {
        lhs: TACRef,
        rhs: TACRef,
        dst: TACRef,
    },
    Xor {
        lhs: TACRef,
        rhs: TACRef,
        dst: TACRef,
    },
    Return(TACRef),
}

type TACRef = Rc<TAC>;
type TACVec = Vec<Rc<TAC>>;

static VAR_INDEX: AtomicUsize = AtomicUsize::new(0);

fn incr_var_index() -> usize {
    VAR_INDEX.fetch_add(1, Ordering::SeqCst)
}

fn reset_var_index() {
    VAR_INDEX.store(0, Ordering::SeqCst)
}

fn transform_expr(expr: ASTRef, tac_code: &mut TACVec) -> TACRef {
    match &*expr.borrow() {
        AST::ConstInt(val) => {
            return tac_rc!(ConstInt(*val));
        }

        AST::Not { expr } => {
            let src = transform_expr(expr.clone(), tac_code);
            let var_idx = incr_var_index();
            let dst = tac_rc!(Var(var_idx, 4));
            tac_code.push(tac_rc!(Not {
                src: src,
                dst: dst.clone(),
            }));
            return dst;
        }

        AST::Negate { expr } => {
            let src = transform_expr(expr.clone(), tac_code);
            let var_idx = incr_var_index();
            let dst = tac_rc!(Var(var_idx, 4));
            tac_code.push(tac_rc!(Neg {
                src: src,
                dst: dst.clone(),
            }));
            return dst;
        }

        AST::Multiply { left, right } => {
            let x = transform_expr(left.clone(), tac_code);
            let y = transform_expr(right.clone(), tac_code);
            let var_idx = incr_var_index();
            let dst = tac_rc!(Var(var_idx, 4));
            tac_code.push(tac_rc!(Mul {
                lhs: x,
                rhs: y,
                dst: dst.clone()
            }));
            return dst;
        }

        AST::Divide { left, right } => {
            let x = transform_expr(left.clone(), tac_code);
            let y = transform_expr(right.clone(), tac_code);
            let var_idx = incr_var_index();
            let dst = tac_rc!(Var(var_idx, 4));
            tac_code.push(tac_rc!(IDiv {
                lhs: x,
                rhs: y,
                dst: dst.clone()
            }));
            return dst;
        }

        AST::Modulo { left, right } => {
            let x = transform_expr(left.clone(), tac_code);
            let y = transform_expr(right.clone(), tac_code);
            let var_idx = incr_var_index();
            let dst = tac_rc!(Var(var_idx, 4));
            tac_code.push(tac_rc!(Mod {
                lhs: x,
                rhs: y,
                dst: dst.clone()
            }));
            return dst;
        }

        AST::Add { left, right } => {
            let x = transform_expr(left.clone(), tac_code);
            let y = transform_expr(right.clone(), tac_code);
            let var_idx = incr_var_index();
            let dst = tac_rc!(Var(var_idx, 4));
            tac_code.push(tac_rc!(Add {
                lhs: x,
                rhs: y,
                dst: dst.clone()
            }));
            return dst;
        }

        AST::Subtract { left, right } => {
            let x = transform_expr(left.clone(), tac_code);
            let y = transform_expr(right.clone(), tac_code);
            let var_idx = incr_var_index();
            let dst = tac_rc!(Var(var_idx, 4));
            tac_code.push(tac_rc!(Sub {
                lhs: x,
                rhs: y,
                dst: dst.clone()
            }));
            return dst;
        }

        AST::LShift { left, right } => {
            let x = transform_expr(left.clone(), tac_code);
            let y = transform_expr(right.clone(), tac_code);
            let var_idx = incr_var_index();
            let dst = tac_rc!(Var(var_idx, 4));
            tac_code.push(tac_rc!(LShift {
                lhs: x,
                rhs: y,
                dst: dst.clone()
            }));
            return dst;
        }

        AST::RShift { left, right } => {
            let x = transform_expr(left.clone(), tac_code);
            let y = transform_expr(right.clone(), tac_code);
            let var_idx = incr_var_index();
            let dst = tac_rc!(Var(var_idx, 4));
            tac_code.push(tac_rc!(RShift {
                lhs: x,
                rhs: y,
                dst: dst.clone()
            }));
            return dst;
        }

        AST::And { left, right } => {
            let x = transform_expr(left.clone(), tac_code);
            let y = transform_expr(right.clone(), tac_code);
            let var_idx = incr_var_index();
            let dst = tac_rc!(Var(var_idx, 4));
            tac_code.push(tac_rc!(And {
                lhs: x,
                rhs: y,
                dst: dst.clone()
            }));
            return dst;
        }

        AST::Or { left, right } => {
            let x = transform_expr(left.clone(), tac_code);
            let y = transform_expr(right.clone(), tac_code);
            let var_idx = incr_var_index();
            let dst = tac_rc!(Var(var_idx, 4));
            tac_code.push(tac_rc!(Or {
                lhs: x,
                rhs: y,
                dst: dst.clone()
            }));
            return dst;
        }

        AST::Xor { left, right } => {
            let x = transform_expr(left.clone(), tac_code);
            let y = transform_expr(right.clone(), tac_code);
            let var_idx = incr_var_index();
            let dst = tac_rc!(Var(var_idx, 4));
            tac_code.push(tac_rc!(Xor {
                lhs: x,
                rhs: y,
                dst: dst.clone()
            }));
            return dst;
        }

        _ => {
            unreachable!();
        }
    }
}

fn transform(node: ASTRef, tac_code: &mut TACVec) {
    match &*node.borrow() {
        AST::Function {
            name,
            params: _,
            stmts,
            rtype: _,
        } => {
            let mut func_code: TACVec = vec![];
            reset_var_index();

            for stmt in stmts {
                transform(stmt.clone(), &mut func_code);
            }

            tac_code.push(tac_rc!(Function {
                name: name.clone(),
                code: func_code,
            }));
        }

        AST::Return { expr } => {
            let e = transform_expr(expr.clone(), tac_code);
            tac_code.push(tac_rc!(Return(e)));
        }

        _ => {
            unreachable!();
        }
    }
}

pub fn generate(ast: ASTVec) -> TACVec {
    let mut code: TACVec = vec![];

    for node in ast {
        transform(node, &mut code);
    }

    return code;
}
