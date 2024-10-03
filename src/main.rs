use std::env;
use std::fs::File;
use std::io::prelude::*;
use std::collections::HashSet;

// use prettydiff::text::LineChangeset;
use sexp::Atom::*;
use sexp::*;

use im::HashMap;

#[derive(PartialEq)]
#[derive(Clone)]
enum Type {
    Int,
    Bool,
}

#[derive(Debug)]
enum Val {
    Reg(Reg),
    Imm(i32),
    RegOffset(Reg, i32),
}

// ADDED: extra register
#[derive(Debug)]
enum Reg {
    RAX,
    RSP,
    RBX,
    RDI,
}

// ADDED: added more enums for diff assembly instrs
// need to add jump and stuff for like if/else  
#[derive(Debug)]
enum Instr {
    IMov(Val, Val),
    IAdd(Val, Val),
    ISub(Val, Val),
    IMul(Val, Val),
    ICmp(Val,Val),
    Cmove(Val,Val),
    Cmovl(Val,Val),
    Cmovg(Val,Val),
    Cmovle(Val,Val),
    Cmovge(Val,Val),
    Jmp(String),      
    Jne(String),   
    Jo(String),
    Je(String),
    Label(String),

}

enum Op1 { Add1, Sub1 }

enum Op2 { Plus, Minus, Times, Equal, Greater, GreaterEqual, Less, LessEqual, }

enum Expr {
    Input,
    Number(i32),
    Boolean(bool),
    Id(String),
    Let(Vec<(String, Expr)>, Box<Expr>),
    UnOp(Op1, Box<Expr>),
    BinOp(Op2, Box<Expr>, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    RepeatUntil(Box<Expr>, Box<Expr>),
    Set(String, Box<Expr>),
    Block(Vec<Expr>),
}

fn is_reserved_word(s: &str) -> bool {
    let mut reserved_words: HashSet<&str> = HashSet::new();
    reserved_words.insert("let");
    reserved_words.insert("add1");
    reserved_words.insert("sub1");
    reserved_words.insert("+");
    reserved_words.insert("-");
    reserved_words.insert("*");
    reserved_words.insert("input");
    reserved_words.contains(s)
}

fn parse_bind(b: &Sexp, body: &Sexp) -> Expr {
    let mut bindings: Vec<(String, Expr)> = Vec::new();
    let mut scope: HashSet<&str> = HashSet::new();
    match b {
        Sexp::List(binds) => { 
            for bind in binds {
                match &bind {
                    Sexp::List(vec_) => {
                        match &vec_[..] {
                            [Sexp::Atom(S(name)), s] => {
                                if scope.contains(name.as_str()) { panic!("Duplicate binding") }
                                scope.insert(name);
                                bindings.push((name.to_string(), parse_expr(s)));
                                }, 
                            _ => panic!("invalid let expression")
                        }
                    }
                _ => panic!("invalid let expression")
                }
            }
            Expr::Let(bindings, Box::new(parse_expr(body)))
        },
        _ => panic!("invalid let expression")
    }
}

//ADDED: function for parsing if then else statements
fn parse_conditional(conditional:&Sexp, then_branch:&Sexp, else_branch:&Sexp) -> Expr {
    let condition = Box::new(parse_expr(conditional));
    let then_expr = Box::new(parse_expr(then_branch));
    let else_expr = Box::new(parse_expr(else_branch));

    Expr::If(condition, then_expr, else_expr)

}

fn parse_set(name : &String, body : &Sexp) -> Expr{
    let body = Box::new(parse_expr(body));
    Expr::Set(name.to_string(), body)
}

// ADDED: parsing booleans and if else statements
fn parse_expr(s: &Sexp) -> Expr {
    match s {
        Sexp::Atom(I(n)) => match i32::try_from(*n) {
            Ok(num) => Expr::Number(num),
            Err(_) => panic!("Invalid number literal bigger than i32")
        },

        Sexp::Atom(S(s)) if s == "true" => Expr::Boolean(true),
        Sexp::Atom(S(s)) if s == "false" => Expr::Boolean(false),

        // Handle only strings that aren't booleans
        Sexp::Atom(S(s)) => if s == "input" {Expr::Input} else {Expr::Id(s.to_string())},

        Sexp::Atom(F(_)) => panic!("Float operations not supported"),
        Sexp::List(vec) => {
            match &vec[..] {
                [Sexp::Atom(S(op)), s] if op == "add1" => Expr::UnOp(Op1::Add1, Box::new(parse_expr(s))),
                [Sexp::Atom(S(op)), s] if op == "sub1" => Expr::UnOp(Op1::Sub1, Box::new(parse_expr(s))),

                [Sexp::Atom(S(op)), bindings, body] if op == "let" => parse_bind(bindings, body),
                [Sexp::Atom(S(op)), conditional, then_branch, else_branch] if op == "if" => parse_conditional(conditional, then_branch, else_branch),
                [Sexp::Atom(S(op)), Sexp::Atom(S(name)), body] if op == "set!" => parse_set(name, body),
                [Sexp::Atom(S(op)), l, r] if op == "=" => Expr::BinOp(Op2::Equal,Box::new(parse_expr(l)), Box::new(parse_expr(r))),

                [Sexp::Atom(S(op)), l, r] if op == "<" => Expr::BinOp(Op2::Less,Box::new(parse_expr(l)), Box::new(parse_expr(r))),
                [Sexp::Atom(S(op)), l, r] if op == ">" => Expr::BinOp(Op2::Greater,Box::new(parse_expr(l)), Box::new(parse_expr(r))),
                [Sexp::Atom(S(op)), l, r] if op == ">=" => Expr::BinOp(Op2::GreaterEqual,Box::new(parse_expr(l)), Box::new(parse_expr(r))),
                [Sexp::Atom(S(op)), l, r] if op == "<=" => Expr::BinOp(Op2::LessEqual,Box::new(parse_expr(l)), Box::new(parse_expr(r))),
                [Sexp::Atom(S(op)), l, r] if op == "+" => Expr::BinOp(Op2::Plus,Box::new(parse_expr(l)), Box::new(parse_expr(r))),
                [Sexp::Atom(S(op)), l, r] if op == "-" => Expr::BinOp(Op2::Minus,Box::new(parse_expr(l)), Box::new(parse_expr(r))),
                [Sexp::Atom(S(op)), l, r] if op == "*" => Expr::BinOp(Op2::Times,Box::new(parse_expr(l)), Box::new(parse_expr(r))),
                [Sexp::Atom(S(op)), l, r] if op == "*" => Expr::BinOp(Op2::Times,Box::new(parse_expr(l)), Box::new(parse_expr(r))),
                [Sexp::Atom(S(op)), e1, e2] if op == "repeat-until" => Expr::RepeatUntil(Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), exprs @ ..] if op == "block" => { 
                        if exprs.is_empty() {panic!("Block must have at least one expression");} 
                        Expr::Block(exprs.iter().map(parse_expr).collect())
                    },
                _ => panic!("invalid parse sequence"),
            }
        },
    }
}

fn find_max_offset(allocs: &HashMap<String, i32>) -> i32{
  let mut max_offset = 0;
  for (_, offset) in allocs.into_iter() {
     if offset < &max_offset {
          max_offset = *offset;
     }
  }
  max_offset
}

fn allocate_space(name: &str, allocs: &mut HashMap<String, i32>) -> i32{
  let offset = find_max_offset(allocs);
  allocs.insert(name.to_string(), offset-8);
  // println!("allocated space at {}", offset-8);
  offset-8
}

// ADDED: binops for > < >= <= =, and if case
fn compile_to_instrs(e: &Expr, allocs: &mut HashMap<String, i32>, label_idx: &mut i32) -> Vec<Instr> {
  match e {
      Expr::Number(n) => vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(*n))],
      // if b is true, b=1 else b=0 
      Expr::Boolean(b) => vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(if *b { 1} else { 0 }))],
      Expr::Input => vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Reg(Reg::RDI))],
      Expr::Id(name) => match allocs.get(name) {
            Some(&offset) => vec![Instr::IMov(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, offset))],
            None => panic!("Unbound variable identifier {name}")
        },
      
      Expr::UnOp(Op1::Add1, e_) => {
          let mut ins = compile_to_instrs(e_, allocs,label_idx);
          ins.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::Imm(1)));
          ins.push(Instr::Jo("error".to_string()));
          ins
      },
      Expr::UnOp(Op1::Sub1, e_) => {
          let mut ins = compile_to_instrs(e_, allocs,label_idx);
          ins.push(Instr::ISub(Val::Reg(Reg::RAX), Val::Imm(1)));
          ins.push(Instr::Jo("error".to_string()));
          ins
      },
      Expr::BinOp(Op2::Plus, l, r) => {
          let mut ins = compile_to_instrs(l, allocs,label_idx);
          let offset_left = allocate_space(&format!("temp{}", allocs.len()), allocs);
          ins.push(Instr::IMov(Val::RegOffset(Reg::RSP, offset_left), Val::Reg(Reg::RAX)));
          ins.extend(compile_to_instrs(r, allocs,label_idx));
          ins.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, offset_left)));
          ins.push(Instr::Jo("error".to_string()));
          ins
      },
      Expr::BinOp(Op2::Minus, l, r) => {
          let mut ins = compile_to_instrs(r, allocs,label_idx);
          let offset_right = allocate_space(&format!("temp{}", allocs.len()), allocs);
          ins.push(Instr::IMov(Val::RegOffset(Reg::RSP, offset_right), Val::Reg(Reg::RAX)));
          ins.extend(compile_to_instrs(l, allocs,label_idx));
          ins.push(Instr::ISub(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, offset_right)));
          ins.push(Instr::Jo("error".to_string()));
          ins
      },
      Expr::BinOp(Op2::Times, l, r) => {
          let mut ins = compile_to_instrs(l, allocs,label_idx);
          let offset_left = allocate_space(&format!("temp{}", allocs.len()), allocs);
          ins.push(Instr::IMov(Val::RegOffset(Reg::RSP, offset_left), Val::Reg(Reg::RAX)));
          ins.extend(compile_to_instrs(r, allocs,label_idx));
          ins.push(Instr::IMul(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, offset_left)));
          ins.push(Instr::Jo("error".to_string()));
          ins
      },
      Expr::Let(bindings, body) => {
        let mut keys: Vec<String> = Vec::new();
        let mut ins: Vec<Instr> = Vec::new();
        for (name, e) in bindings {
            if is_reserved_word(name) {panic!("trying to bind reserved variable name: {name}")}
            ins.extend(compile_to_instrs(e, allocs,label_idx));
            keys.push(name.to_string());
            let offset = allocate_space(name, allocs);
            ins.push(Instr::IMov(Val::RegOffset(Reg::RSP, offset), Val::Reg(Reg::RAX)));
        }
        ins.extend(compile_to_instrs(body, allocs,label_idx));
        for key in keys {
            allocs.remove(&key);
        }
        ins
    },
      Expr::BinOp(Op2::Equal, l, r) => {
        let mut ins = compile_to_instrs(l, allocs,label_idx);
        let offset_left = allocate_space(&format!("temp{}", allocs.len()), allocs);
        ins.push(Instr::IMov(Val::RegOffset(Reg::RSP, offset_left), Val::Reg(Reg::RAX)));
        ins.extend(compile_to_instrs(r, allocs,label_idx));
        ins.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, offset_left)));
        ins.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(1))); // set rbx to 1 (true)
        ins.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(0)));   //set rax to 0 (false)
        ins.push(Instr::Cmove(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));  // move 1 into rax if less than
        ins
    },

    Expr::BinOp(Op2::Less, l, r) => {
        let mut ins = compile_to_instrs(l, allocs,label_idx);
        let offset_left = allocate_space(&format!("temp{}", allocs.len()), allocs);
        ins.push(Instr::IMov(Val::RegOffset(Reg::RSP, offset_left), Val::Reg(Reg::RAX)));
        ins.extend(compile_to_instrs(r, allocs,label_idx));
        ins.push(Instr::ICmp(Val::RegOffset(Reg::RSP, offset_left), Val::Reg(Reg::RAX)));
        ins.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(1))); // set rbx to 1 (true)
        ins.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(0)));   //set rax to 0 (false)
        ins.push(Instr::Cmovl(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));  // move 1 into rax if less than
        
        ins
    },

    Expr::BinOp(Op2::Greater, l, r) => {
        let mut ins = compile_to_instrs(l, allocs,label_idx);
        let offset_left = allocate_space(&format!("temp{}", allocs.len()), allocs);
        ins.push(Instr::IMov(Val::RegOffset(Reg::RSP, offset_left), Val::Reg(Reg::RAX)));
        ins.extend(compile_to_instrs(r, allocs,label_idx));
        ins.push(Instr::ICmp(Val::RegOffset(Reg::RSP, offset_left), Val::Reg(Reg::RAX)));
        ins.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(1))); // set rbx to 1 (true)
        ins.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(0)));   //set rax to 0 (false)
        ins.push(Instr::Cmovg(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));  // move 1 into rax if greater than
        ins
    },

    Expr::BinOp(Op2::LessEqual, l, r) => {
        let mut ins = compile_to_instrs(l, allocs,label_idx);
        let offset_left = allocate_space(&format!("temp{}", allocs.len()), allocs);
        ins.push(Instr::IMov(Val::RegOffset(Reg::RSP, offset_left), Val::Reg(Reg::RAX)));
        ins.extend(compile_to_instrs(r, allocs,label_idx));
        ins.push(Instr::ICmp(Val::RegOffset(Reg::RSP, offset_left), Val::Reg(Reg::RAX)));
        ins.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(1))); // set rbx to 1 (true)
        ins.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(0)));   // set rax to 0 (false)
        ins.push(Instr::Cmovle(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));  // move 1 into rax if less or equal
        ins
    },
    
    Expr::BinOp(Op2::GreaterEqual, l, r) => {
        let mut ins = compile_to_instrs(l, allocs,label_idx);
        let offset_left = allocate_space(&format!("temp{}", allocs.len()), allocs);
        ins.push(Instr::IMov(Val::RegOffset(Reg::RSP, offset_left), Val::Reg(Reg::RAX)));
        ins.extend(compile_to_instrs(r, allocs,label_idx));
        ins.push(Instr::ICmp(Val::RegOffset(Reg::RSP, offset_left), Val::Reg(Reg::RAX)));
        ins.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(1))); // set rbx to 1 (true)
        ins.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(0)));   // set rax to 0 (false)
        ins.push(Instr::Cmovge(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));  // move 1 into rax if greater or equal
        ins
    },

    Expr::Block(exprs) => {
        let mut ins: Vec<Instr> = Vec::new();
        for e in exprs {
            ins.extend(compile_to_instrs(e, allocs,label_idx));
        }
        ins
    },
    
    Expr::If(cond, then_branch, else_branch) => {
        let mut ins = compile_to_instrs(cond, allocs, label_idx); 
        let offset_left = allocate_space(&format!("temp{}", allocs.len()), allocs); 
        ins.push(Instr::IMov(Val::RegOffset(Reg::RSP, offset_left), Val::Reg(Reg::RAX))); 
        // ins.extend(compile_to_instrs(then_branch, allocs, label_idx)); 
    
        // FIX: need to add indicies to else and end because of nested if statements 
        let else_label = format!("else{}", label_idx); // create a label for the else branch
        let end_label = format!("end{}", label_idx); // create a label for the end of the if statement
        *label_idx += 1;

        ins.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::Imm(0)));
        ins.push(Instr::Je(else_label.clone())); 
    
        ins.extend(compile_to_instrs(then_branch, allocs,label_idx)); 
        ins.push(Instr::Jmp(end_label.clone()));
    
        // else label
        ins.push(Instr::Label(else_label));
        ins.extend(compile_to_instrs(else_branch, allocs,label_idx)); 
    
        // end label
        ins.push(Instr::Label(end_label));
    
        ins
    },

      Expr::Set(name, body) => {
            if !(allocs.contains_key(name)) {panic!("trying to bind undeclared variable {name}")}
            let mut ins = compile_to_instrs(body, allocs,label_idx); 
            let offset = allocs.get(name).unwrap();
            ins.push(Instr::IMov(Val::RegOffset(Reg::RSP, *offset), Val::Reg(Reg::RAX)));
            ins
      },
      
    Expr::RepeatUntil(body,condition) => {
        let offset_left = allocate_space(&format!("temp{}", allocs.len()), allocs); 
        let loop_label = format!("loop{}", label_idx);
        // let end_label = format!("end{}", label_idx);
        *label_idx += 1;

        let mut ins = Vec::new();
        
        // loop0:
        ins.push(Instr::Label(loop_label.clone()));
        // {e}
        ins.extend(compile_to_instrs(body, allocs,label_idx)); 
        // move the updated values into rsp
        ins.push(Instr::IMov(Val::RegOffset(Reg::RSP, offset_left), Val::Reg(Reg::RAX))); 
        // {condition}
        ins.extend(compile_to_instrs(condition, allocs,label_idx)); 
        // compare rax (the condition) with the current value
        // ins.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, offset_left)));
        // check if condition is true by comparing rax to 1
        ins.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::Imm(1)));
        // jump to the start if the condition hasn't been met yet
        ins.push(Instr::Jne(loop_label));
        // if condition is met, move result to rax
        ins.push(Instr::IMov(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, offset_left)));
        // add the end label to the end of the loop - idk if this is needed
        // ins.push(Instr::Label(end_label)); // i don't think this is needed
        ins
    }
  }
}

// ADDED: rbx to string
fn val_to_str(v: &Val) -> String {
  match v {
      Val::Imm(n) => n.to_string(),
      Val::Reg(Reg::RAX) => "rax".to_string(),
      Val::Reg(Reg::RSP) => "rsp".to_string(),
      Val::Reg(Reg::RBX) => "rbx".to_string(),
      Val::Reg(Reg::RDI) => "rdi".to_string(),

      Val::RegOffset(Reg::RSP, n) => format!("[rsp{n}]"),
      _ => panic!("not yet implemented"),
  }
}

// ADDED: intrs to strings for new instrs
fn instr_to_stri(i: &Instr) -> String {
  match i {
      Instr::IMov(a, b) => {
          let str_a = val_to_str(a);
          let str_b = val_to_str(b);
          format!("mov {str_a}, {str_b}")
      }
      Instr::IAdd(a, b) => {
          let str_a = val_to_str(a);
          let str_b = val_to_str(b);
          format!("add {str_a}, {str_b}")
      }
      Instr::ISub(a, b) => {
          let str_a = val_to_str(a);
          let str_b = val_to_str(b);
          format!("sub {str_a}, {str_b}")
      }
      Instr::IMul(a, b) => {
          let str_a = val_to_str(a);
          let str_b = val_to_str(b);
          format!("imul {str_a}, {str_b}")
      }
      Instr::ICmp(a, b) => {
                let str_a = val_to_str(a);
                let str_b = val_to_str(b);
                format!("cmp {str_a}, {str_b}")
      }
      Instr::Cmove(a, b) => {
                let str_a = val_to_str(a);
                let str_b = val_to_str(b);
                format!("cmove {str_a}, {str_b}")
    }
      Instr::Cmovl(a, b) => {
                let str_a = val_to_str(a);
                let str_b = val_to_str(b);
                format!("cmovl {str_a}, {str_b}")
     }
      Instr::Cmovg(a, b) => {
                let str_a = val_to_str(a);
                let str_b = val_to_str(b);
                format!("cmovg {str_a}, {str_b}")
      }
      Instr::Cmovle(a, b) => {
                let str_a = val_to_str(a);
                let str_b = val_to_str(b);
                format!("cmovle {str_a}, {str_b}")
      }
      Instr::Cmovge(a, b) => {
                let str_a = val_to_str(a);
                let str_b = val_to_str(b);
                format!("cmovge {str_a}, {str_b}")
      }
        
      Instr::Label(name) => format!{"{}:", name},
      Instr::Jmp(label) => format!("jmp {}", label),
      Instr::Jne(label) => format!("jne {}", label),
      Instr::Jo(label) => format!("jo {}", label),
      Instr::Je(label) => format!("je {label}"),


  }
}


fn compile(e: &Expr) -> String {
  let mut allocations: HashMap<String, i32> = HashMap::new();
  let mut label_index = 0;
  let instructions: Vec<Instr> = compile_to_instrs(e, &mut allocations, &mut label_index);
  let assembly: Vec<String> = instructions.iter().map(|x| instr_to_stri(x)).collect();
  assembly.join("\n")
}

fn typecheck(e: &Expr, ctx:&HashMap<String, Type>) -> Type {
  match e {
    Expr::Input => {
        Type::Int
    },
    Expr::Number(_) => {
        Type::Int
    },
    Expr::Boolean(_) => {
        Type::Bool
    },
    Expr::UnOp(_, e) => {
        let e_type = typecheck(e, ctx);
        if e_type != Type::Int {
            panic!("type mismatch: expression must be an integer");
        }
        e_type
    }
    Expr::BinOp(op2,lhs, rhs) => {
        let l_type = typecheck(lhs, ctx);
        let r_type = typecheck(rhs, ctx);
       match op2 {
        Op2::Equal => {
            if l_type != r_type {
                panic!("type mismatch: equal operation expects two of the same type");
            } 
            Type::Bool
        },
        Op2::Minus => {
            if l_type != Type::Int || r_type != Type::Int {
                panic!("type mismatch: binary operator expected two integers");
            }
            l_type
        },
        Op2::Plus => {
            if l_type != Type::Int || r_type != Type::Int {
                panic!("type mismatch: binary operator expected two integers");
            }
            l_type
        },
        Op2::Times => {
            if l_type != Type::Int || r_type != Type::Int {
                panic!("type mismatch: binary operator expected two integers");
            }
            l_type
        },
        _ => {
            if l_type != Type::Int || r_type != Type::Int {
                panic!("type mismatch: binary operator expected two integers");
            }
            Type::Bool
        }
       } 
    },
    Expr::Id(name) => {
      match ctx.get(name) {
        Some(ty) => ty.clone(),
        None => panic!("panic"),
      }
    },
    Expr::Let(bindings, body) => {
        let mut new_ctx = ctx.clone();
        for vec in bindings.iter() {
            let name = vec.0.clone();
            let rhs = &vec.1;
            if is_reserved_word(&name) { 
                panic!("variable name is a keyword: {name}"); 
            }
            let ty1 = typecheck(rhs, &new_ctx);
            new_ctx = new_ctx.update(name.clone(), ty1);
        }
        typecheck(body, &new_ctx)
    },
    Expr::If(cond, then_branch, else_branch) => {
        let cond_type = typecheck(cond, ctx);
        if cond_type != Type::Bool {
            panic!("type mismatch: condition must be boolean");
        }
        let then_type = typecheck(then_branch, ctx);
        let else_type = typecheck(else_branch, ctx);
        if then_type != else_type {
            panic!("type mismatch: branches must return the same type");
        }
        then_type
    }
    Expr::Set(name, body) => {
        let name_type = ctx.get(name).unwrap().clone();
        let body_type = typecheck(body, ctx);
        if name_type != body_type {
            panic!("type mismatch: set variable must have same type as set body")
        }
        name_type
    }
    Expr::Block(exps) => {
        let mut e_type: Option<Type> = None;
        for exp in exps.iter() {
           e_type = Some(typecheck(exp, ctx));
        }
        if e_type == None {
            panic!("block expression must contain at least one expression")
        }
        e_type.unwrap()
    }
    Expr::RepeatUntil(body, condition) => {
        let condition_type = typecheck(condition, ctx);
        if condition_type != Type::Bool {
            panic!("type mismatch: repeat until condition condition must be type bool");
        }
        typecheck(body, ctx)
    }
    //ADDED curly bracket
  }
}

fn main() -> std::io::Result<()> {
    // ADDED reverted changes to main since input is collected in runtime/start.rs
    let args: Vec<String> = env::args().collect();

    let in_name = &args[1];
    let out_name = &args[2];

    let mut in_file = File::open(in_name)?;
    let mut in_contents = String::new();
    in_file.read_to_string(&mut in_contents)?;

    let expr = parse_expr(&parse(&in_contents).unwrap());
    let ctx: HashMap<String, Type> = HashMap::new(); 
    let expr_type = typecheck(&expr, &ctx);
    let result = compile(&expr);
    let type_flag = if expr_type == Type::Int {0} else {1};
    let asm_program = format!(
        "
section .text
extern snek_error
extern snek_print
global our_code_starts_here
our_code_starts_here:
{}
mov rdi, rax
mov rsi, {}
call snek_print
ret
error:
mov rdi, 0
call snek_error
     ",
       result,
       type_flag
      );


    let mut out_file = File::create(out_name)?;
    out_file.write_all(asm_program.as_bytes())?;

    Ok(())
}



