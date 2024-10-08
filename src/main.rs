use std::env;
use std::fmt::format;
use std::fs::File;
use std::io::prelude::*;
use std::collections::HashSet;

// use prettydiff::text::LineChangeset;
use sexp::Atom::*;
use sexp::*;

use im::HashMap;

#[derive(PartialEq)]
#[derive(Clone)]

// ADDED: type int and bool 

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

// ADDED: extra registers for function args
#[derive(Debug)]
enum Reg {
    RAX,
    RSP,
    RBX,
    RDI,
    RSI,
    RDX,
    RCX,
    RBP,
    R8,
    R9,

}

// ADDED: added more enums for call and ret ADDED: push and pop
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
    Call(String),
    Push(Val),
    Pop(Val),
    Ret,


}
// ADDED: print

enum Op1 { Add1, Sub1, Print}

enum Op2 { Plus, Minus, Times, Equal, Greater, GreaterEqual, Less, LessEqual, }

// ADDED: struct for function defn

struct Defn {
    name: String,
    params: Vec<(String, Type)>, // (parameter name, type)
    return_type: Type,
    body: Expr,
}

// ADDED: struct for program defn 

struct Prog {
    functions: Vec<Defn>,
    main_expr: Expr,
}

// ADDED: function call

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
    FunCall {
        name: String,
        args: Vec<Expr>, // arguments to the function call
    },

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

// check for duplicates in function parameter list
fn check_duplicates(params: &Vec<(String, Type)>) {
    let mut seen = HashSet::new();
    for (name, _) in params {
        if seen.contains(name) {
            eprintln!("Duplicate parameter name: {}", name);
        }
        seen.insert(name.clone());
    }
}
fn check_duplicates_function_names(functs: &Vec<Defn>) {
    let mut seen = HashSet::new();
    for funct in functs.iter() {
        if seen.contains(&funct.name) {
            eprintln!("Duplicate parameter name: {}", &funct.name);
        }
        seen.insert(funct.name.clone());
    }
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

//ADDED: function for parsing types
fn parse_type(s: &Sexp) -> Type {
    match s {
        Sexp::Atom(S(t)) if t == "int" => Type::Int,
        Sexp::Atom(S(t)) if t == "bool" => Type::Bool,
        _ => panic!("Unknown type"),
    }
}

//ADDED: function for parsing function definitions
fn parse_defn(s: &Sexp) -> Defn {
    match s {
        Sexp::List(vec) => match &vec[..] {
            [Sexp::Atom(S(fun)), Sexp::Atom(S(name)), Sexp::List(params), return_type, body] if fun == "fun" => {
                let param_list: Vec<(String, Type)> = params
                    .iter()
                    .map(|param| match param {
                        Sexp::List(vec) => match &vec[..] {
                            [Sexp::Atom(S(param_name)), param_type] => {
                                (param_name.clone(), parse_type(param_type))
                            }
                            _ => panic!("Invalid parameter format"),
                        },
                        _ => panic!("Invalid parameter format"),
                    })
                    .collect();

                check_duplicates(&param_list); // Check for duplicate parameters
                let ret_type = parse_type(return_type);
                let expr_body = parse_expr(body);

                Defn {
                    name: name.clone(),
                    params: param_list,
                    return_type: ret_type,
                    body: expr_body,
                }
            }
            _ => panic!("Invalid function definition"),
        },
        _ => panic!("Invalid function syntax"),
    }
}

fn parse_prog(s: &Sexp) -> Prog {
    match s {
        Sexp::List(exprs) => {
            let mut functions = Vec::new();
            let mut main_expr: Option<Expr> = None;

            for expr in exprs.iter() {
                match expr {
                    Sexp::List(list) => {
                        match &list[..] {
                            [Sexp::Atom(S(fun)), Sexp::Atom(S(name)), Sexp::List(params), return_type, body] if fun == "fun" => {
                                functions.push(parse_defn(expr));
                            }
                            _ => {
                                if main_expr.is_none() {
                                    main_expr = Some(parse_expr(expr));
                                } else {
                                    panic!("Multiple main expressions found");
                                }
                            }
                        }
                    }
                    _ => panic!("Invalid program structure"),
                }
            }
            check_duplicates_function_names(&functions);

            Prog {
                functions,
                main_expr: main_expr.expect("Program must have a main expression"),
            }
        },
        _ => panic!("Invalid program syntax"),
    }
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
                [Sexp::Atom(S(op)), s] if op == "print" => Expr::UnOp(Op1::Print, Box::new(parse_expr(s))),

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

                //ADDED: parsing a function call
                [Sexp::Atom(S(fun_name)), exprs @ ..] => {
                        Expr::FunCall {
                            name: fun_name.clone(),
                            args: exprs.iter().map(parse_expr).collect(),
                        }
                }
        
                _ => panic!("invalid parse sequence"),
            }
        },
    }
}

fn find_max_offset(allocs: &HashMap<String, i32>) -> i32{
  let mut max_offset = 0;
  for (name, offset) in allocs.into_iter() {
     if offset < &max_offset && !name.ends_with("LOCAL_VAR") {
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
fn compile_to_instrs(e: &Expr, allocs: &mut HashMap<String, i32>, label_idx: &mut i32, functs: &Vec<Defn>, ctx:&HashMap<String, Type>) -> Vec<Instr> {
  match e {
      Expr::Number(n) => vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(*n))],

      // if b is true, b=1 else b=0 
      Expr::Boolean(b) => vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(if *b { 1} else { 0 }))],
      Expr::Input => vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Reg(Reg::RDI))],
      Expr::Id(name) => match allocs.get(name) {
            Some(&offset) =>  vec![Instr::IMov(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, offset))],
            None => {
                let mut clone = name.clone();
                clone.push_str("LOCAL_VAR");
                match allocs.get(&clone) {
                    Some(&offset) =>  vec![Instr::IMov(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RBP, offset*8))],
                    None => panic!("Unbound variable identifier {name}")
                }
            }
            
        },
      
      Expr::UnOp(Op1::Add1, e_) => {
          let mut ins = compile_to_instrs(e_, allocs,label_idx, functs, ctx);
          ins.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::Imm(1)));
          ins.push(Instr::Jo("error".to_string()));
          ins
      },
      Expr::UnOp(Op1::Sub1, e_) => {
          let mut ins = compile_to_instrs(e_, allocs,label_idx, functs, ctx);
          ins.push(Instr::ISub(Val::Reg(Reg::RAX), Val::Imm(1)));
          ins.push(Instr::Jo("error".to_string()));
          ins
      },
      //ADDED: starting implementing print
      // FIX: i don't know if we are supposed to call snek_print or some other function
      // on piazza: 
      // The behavior of the print statement will depend on the type of its argument, so you'll need to find a way for typechecking to impact code generation.

      Expr::UnOp(Op1::Print, e) => {
        let type_e = typecheck(e, functs, ctx);
        let type_flag = if type_e == Type::Int {0} else {1};
        let mut ins = compile_to_instrs(e, allocs, label_idx, functs, ctx); 
        ins.push(Instr::IMov(Val::Reg(Reg::RDI), Val::Reg(Reg::RAX))); 
        ins.push(Instr::IMov(Val::Reg(Reg::RSI), Val::Imm(type_flag))); 
        ins.push(Instr::Call("snek_print".to_string())); 
        ins
        },
    
      Expr::BinOp(Op2::Plus, l, r) => {
          let mut ins = compile_to_instrs(l, allocs,label_idx, functs, ctx);
          let offset_left = allocate_space(&format!("temp{}", allocs.len()), allocs);
          ins.push(Instr::IMov(Val::RegOffset(Reg::RSP, offset_left), Val::Reg(Reg::RAX)));
          ins.extend(compile_to_instrs(r, allocs,label_idx, functs, ctx));
          ins.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, offset_left)));
          ins.push(Instr::Jo("error".to_string()));
          ins
      },
      Expr::BinOp(Op2::Minus, l, r) => {
          let mut ins = compile_to_instrs(r, allocs,label_idx, functs, ctx);
          let offset_right = allocate_space(&format!("temp{}", allocs.len()), allocs);
          ins.push(Instr::IMov(Val::RegOffset(Reg::RSP, offset_right), Val::Reg(Reg::RAX)));
          ins.extend(compile_to_instrs(l, allocs,label_idx, functs, ctx));
          ins.push(Instr::ISub(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, offset_right)));
          ins.push(Instr::Jo("error".to_string()));
          ins
      },
      Expr::BinOp(Op2::Times, l, r) => {
          let mut ins = compile_to_instrs(l, allocs,label_idx, functs, ctx);
          let offset_left = allocate_space(&format!("temp{}", allocs.len()), allocs);
          ins.push(Instr::IMov(Val::RegOffset(Reg::RSP, offset_left), Val::Reg(Reg::RAX)));
          ins.extend(compile_to_instrs(r, allocs,label_idx, functs, ctx));
          ins.push(Instr::IMul(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, offset_left)));
          ins.push(Instr::Jo("error".to_string()));
          ins
      },
      Expr::Let(bindings, body) => {
        let mut keys: Vec<String> = Vec::new();
        let mut ins: Vec<Instr> = Vec::new();
        for (name, e) in bindings {
            if is_reserved_word(name) {panic!("trying to bind reserved variable name: {name}")}
            ins.extend(compile_to_instrs(e, allocs,label_idx, functs, ctx));
            keys.push(name.to_string());
            let offset = allocate_space(name, allocs);
            ins.push(Instr::IMov(Val::RegOffset(Reg::RSP, offset), Val::Reg(Reg::RAX)));
        }
        ins.extend(compile_to_instrs(body, allocs,label_idx, functs, ctx));
        for key in keys {
            allocs.remove(&key);
        }
        ins
    },
      Expr::BinOp(Op2::Equal, l, r) => {
        let mut ins = compile_to_instrs(l, allocs,label_idx, functs, ctx);
        let offset_left = allocate_space(&format!("temp{}", allocs.len()), allocs);
        ins.push(Instr::IMov(Val::RegOffset(Reg::RSP, offset_left), Val::Reg(Reg::RAX)));
        ins.extend(compile_to_instrs(r, allocs,label_idx, functs, ctx));
        ins.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, offset_left)));
        ins.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(1))); // set rbx to 1 (true)
        ins.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(0)));   //set rax to 0 (false)
        ins.push(Instr::Cmove(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));  // move 1 into rax if less than
        ins
    },

    Expr::BinOp(Op2::Less, l, r) => {
        let mut ins = compile_to_instrs(l, allocs,label_idx, functs, ctx);
        let offset_left = allocate_space(&format!("temp{}", allocs.len()), allocs);
        ins.push(Instr::IMov(Val::RegOffset(Reg::RSP, offset_left), Val::Reg(Reg::RAX)));
        ins.extend(compile_to_instrs(r, allocs,label_idx, functs, ctx));
        ins.push(Instr::ICmp(Val::RegOffset(Reg::RSP, offset_left), Val::Reg(Reg::RAX)));
        ins.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(1))); // set rbx to 1 (true)
        ins.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(0)));   //set rax to 0 (false)
        ins.push(Instr::Cmovl(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));  // move 1 into rax if less than
        
        ins
    },

    Expr::BinOp(Op2::Greater, l, r) => {
        let mut ins = compile_to_instrs(l, allocs,label_idx, functs, ctx);
        let offset_left = allocate_space(&format!("temp{}", allocs.len()), allocs);
        ins.push(Instr::IMov(Val::RegOffset(Reg::RSP, offset_left), Val::Reg(Reg::RAX)));
        ins.extend(compile_to_instrs(r, allocs,label_idx, functs, ctx));
        ins.push(Instr::ICmp(Val::RegOffset(Reg::RSP, offset_left), Val::Reg(Reg::RAX)));
        ins.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(1))); // set rbx to 1 (true)
        ins.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(0)));   //set rax to 0 (false)
        ins.push(Instr::Cmovg(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));  // move 1 into rax if greater than
        ins
    },

    Expr::BinOp(Op2::LessEqual, l, r) => {
        let mut ins = compile_to_instrs(l, allocs,label_idx, functs, ctx);
        let offset_left = allocate_space(&format!("temp{}", allocs.len()), allocs);
        ins.push(Instr::IMov(Val::RegOffset(Reg::RSP, offset_left), Val::Reg(Reg::RAX)));
        ins.extend(compile_to_instrs(r, allocs,label_idx, functs, ctx));
        ins.push(Instr::ICmp(Val::RegOffset(Reg::RSP, offset_left), Val::Reg(Reg::RAX)));
        ins.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(1))); // set rbx to 1 (true)
        ins.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(0)));   // set rax to 0 (false)
        ins.push(Instr::Cmovle(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));  // move 1 into rax if less or equal
        ins
    },
    
    Expr::BinOp(Op2::GreaterEqual, l, r) => {
        let mut ins = compile_to_instrs(l, allocs,label_idx, functs, ctx);
        let offset_left = allocate_space(&format!("temp{}", allocs.len()), allocs);
        ins.push(Instr::IMov(Val::RegOffset(Reg::RSP, offset_left), Val::Reg(Reg::RAX)));
        ins.extend(compile_to_instrs(r, allocs,label_idx, functs, ctx));
        ins.push(Instr::ICmp(Val::RegOffset(Reg::RSP, offset_left), Val::Reg(Reg::RAX)));
        ins.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(1))); // set rbx to 1 (true)
        ins.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(0)));   // set rax to 0 (false)
        ins.push(Instr::Cmovge(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));  // move 1 into rax if greater or equal
        ins
    },

    Expr::Block(exprs) => {
        let mut ins: Vec<Instr> = Vec::new();
        for e in exprs {
            ins.extend(compile_to_instrs(e, allocs,label_idx, functs, ctx));
        }
        ins
    },
    
    Expr::If(cond, then_branch, else_branch) => {
        let mut ins = compile_to_instrs(cond, allocs, label_idx, functs, ctx); 
        let offset_left = allocate_space(&format!("temp{}", allocs.len()), allocs); 
        ins.push(Instr::IMov(Val::RegOffset(Reg::RSP, offset_left), Val::Reg(Reg::RAX))); 
        // ins.extend(compile_to_instrs(then_branch, allocs, label_idx)); 
    
        // FIX: need to add indicies to else and end because of nested if statements 
        let else_label = format!("else{}", label_idx); // create a label for the else branch
        let end_label = format!("end{}", label_idx); // create a label for the end of the if statement
        *label_idx += 1;

        ins.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::Imm(0)));
        ins.push(Instr::Je(else_label.clone())); 
    
        ins.extend(compile_to_instrs(then_branch, allocs,label_idx, functs, ctx)); 
        ins.push(Instr::Jmp(end_label.clone()));
    
        // else label
        ins.push(Instr::Label(else_label));
        ins.extend(compile_to_instrs(else_branch, allocs,label_idx, functs, ctx)); 
    
        // end label
        ins.push(Instr::Label(end_label));
    
        ins
    },

      Expr::Set(name, body) => {
            if !(allocs.contains_key(name)) {panic!("trying to bind undeclared variable {name}")}
            let mut ins = compile_to_instrs(body, allocs,label_idx, functs, ctx); 
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
        ins.extend(compile_to_instrs(body, allocs,label_idx, functs, ctx)); 
        // move the updated values into rsp
        ins.push(Instr::IMov(Val::RegOffset(Reg::RSP, offset_left), Val::Reg(Reg::RAX))); 
        // {condition}
        ins.extend(compile_to_instrs(condition, allocs,label_idx, functs, ctx)); 
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
    },
        //ADDED: function call handling

    Expr::FunCall { name, args } => {
        let mut ins: Vec<Instr> = Vec::new();
        for arg in args.iter().rev() {
            ins.extend(compile_to_instrs(arg, allocs, label_idx, functs, ctx));
            ins.push(Instr::Push(Val::Reg(Reg::RAX))); // save previous caller's rbp
        }
        ins.push(Instr::Call(name.to_string()));
        ins.push(Instr::IAdd(Val::Reg(Reg::RSP), Val::Imm((8*args.len()) as i32)));
        ins
    }
    }
}

fn compile_defns(defns: &[Defn], allocs: &mut HashMap<String, i32>, label_idx: &mut i32, functs: &Vec<Defn>, ctx:&HashMap<String, Type>) -> Vec<Instr> {
    let mut ins = Vec::new();
    for defn in defns {
        // ADDED changes to stack based approach instead of allocating in heap
        // Set up function label
        ins.push(Instr::Label(defn.name.clone()));

        // Set up frame pointer (prologue)
        ins.push(Instr::Push(Val::Reg(Reg::RBP))); // save previous caller's rbp
        ins.push(Instr::IMov(Val::Reg(Reg::RBP), Val::Reg(Reg::RSP)));
        // Allocate stack space for parameters
        ins.push(Instr::ISub(Val::Reg(Reg::RSP), Val::Imm((8*defn.params.len()) as i32))); 

        // Space has already been allocated for local parameters
        // We just need to know where to access them on the stack
        let mut keys: Vec<String> = Vec::new();
        for (i, (param_name, _)) in defn.params.iter().enumerate() {
            if is_reserved_word(&param_name) {panic!("trying to bind reserved variable name: {param_name}")}

            let mut name_clone = param_name.clone();
            name_clone.push_str("LOCAL_VAR");
            allocs.insert(name_clone.to_string(), ((i+1) as i32)*-1);
            keys.push(name_clone.to_string());
        }
        // Compile the body of the function
        ins.extend(compile_to_instrs(&defn.body, allocs, label_idx, functs, ctx));

        // Restore frame pointer (epilogue) and return
        ins.push(Instr::IMov(Val::Reg(Reg::RSP), Val::Reg(Reg::RBP)));
        ins.push(Instr::Pop(Val::Reg(Reg::RBP))); // save previous caller's rbp
        ins.push(Instr::Ret);
        for key in keys {
            allocs.remove(&key);
        }
    }
    ins
}


// ADDED: more registers to strings // ADDED: RBP
fn val_to_str(v: &Val) -> String {
  match v {
      Val::Imm(n) => n.to_string(),
      Val::Reg(Reg::RAX) => "rax".to_string(),
      Val::Reg(Reg::RSP) => "rsp".to_string(),
      Val::Reg(Reg::RBX) => "rbx".to_string(),
      Val::Reg(Reg::RDI) => "rdi".to_string(),
      Val::Reg(Reg::RSI) => "rsi".to_string(),
      Val::Reg(Reg::RDX) => "rdx".to_string(),
      Val::Reg(Reg::RCX) => "rcx".to_string(),
      Val::Reg(Reg::RBP) => "rbp".to_string(),
      Val::Reg(Reg::R8) => "r8".to_string(),
      Val::Reg(Reg::R9) => "r9".to_string(),
      Val::RegOffset(Reg::RSP, n) => format!("[rsp{n}]"),
      Val::RegOffset(Reg::RBP, n) => format!("[rbp{n}]"),
      _ => panic!("not yet implemented"),
  }
}

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
      // ADDED pop push
      Instr::Pop(a) => {
                let str_a = val_to_str(a);
                format!("pop {str_a}")
      }
      Instr::Push(a) => {
                let str_a = val_to_str(a);
                format!("push {str_a}")
      }
        
      Instr::Label(name) => format!{"{}:", name},
      Instr::Jmp(label) => format!("jmp {}", label),
      Instr::Jne(label) => format!("jne {}", label),
      Instr::Jo(label) => format!("jo {}", label),
      Instr::Je(label) => format!("je {label}"),
      // ADDED: intrs to strings call and ret

      Instr::Call(name) => format!("call {}", name),
      Instr::Ret => "ret".to_string(),

  }
}



fn compile(prog: &Prog, ctx:&HashMap<String, Type>) -> String {
  let mut allocations: HashMap<String, i32> = HashMap::new();
  let mut label_index = 0;
  // let instructions: Vec<Instr> = compile_to_instrs(e, &mut allocations, &mut label_index);
  // compile function definitions
  // compile main instructions 
  let mut instructions: Vec<Instr> = compile_to_instrs(&prog.main_expr, &mut allocations, &mut label_index, &prog.functions, ctx);
  instructions.push(Instr::Jmp("done".to_string()));
  instructions.extend(compile_defns(&prog.functions, &mut allocations, &mut label_index, &prog.functions, ctx));

  let assembly: Vec<String> = instructions.iter().map(|x| instr_to_stri(x)).collect();
  assembly.join("\n")
}

fn typecheck(e: &Expr, functs: &Vec<Defn>,  ctx:&HashMap<String, Type>) -> Type {
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
    // ADDED typechecking for print
    Expr::UnOp(op1, e) => {
        match op1 {
            Op1::Print => typecheck(e, functs, ctx),
            _ => {
                let e_type = typecheck(e, functs, ctx);
                if e_type != Type::Int {
                    panic!("type mismatch: expression must be an integer");
                }
                e_type
            }
        }
    },
    Expr::BinOp(op2,lhs, rhs) => {
        let l_type = typecheck(lhs, functs, ctx);
        let r_type = typecheck(rhs, functs, ctx);
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
            let ty1 = typecheck(rhs, functs,&new_ctx);
            new_ctx = new_ctx.update(name.clone(), ty1);
        }
        typecheck(body, functs, &new_ctx)
    },
    Expr::If(cond, then_branch, else_branch) => {
        let cond_type = typecheck(cond, functs, ctx);
        if cond_type != Type::Bool {
            panic!("type mismatch: condition must be boolean");
        }
        let then_type = typecheck(then_branch, functs, ctx);
        let else_type = typecheck(else_branch, functs,ctx);
        if then_type != else_type {
            panic!("type mismatch: branches must return the same type");
        }
        then_type
    }
    Expr::Set(name, body) => {
        let name_type = ctx.get(name).unwrap().clone();
        let body_type = typecheck(body, functs, ctx);
        if name_type != body_type {
            panic!("type mismatch: set variable must have same type as set body")
        }
        name_type
    }
    Expr::Block(exps) => {
        let mut e_type: Option<Type> = None;
        for exp in exps.iter() {
           e_type = Some(typecheck(exp, functs, ctx));
        }
        if e_type == None {
            panic!("block expression must contain at least one expression")
        }
        e_type.unwrap()
    }
    Expr::RepeatUntil(body, condition) => {
        let condition_type = typecheck(condition, functs, ctx);
        if condition_type != Type::Bool {
            panic!("type mismatch: repeat until condition condition must be type bool");
        }
        typecheck(body, functs, ctx)
    }
    // ADDED type check for function calls
    Expr::FunCall { name, args } => {
        for funct in functs.iter() {
            if *name == funct.name {
                if args.len() != funct.params.len() {
                    panic!("function call {} expected a different number of inputs", name);
                }
                for (i, arg )in args.iter().enumerate() {
                    let arg_type = typecheck(arg, functs, ctx);
                    let (_, expected_type) = &funct.params[i];
                    if arg_type != *expected_type {
                        panic!("type mismatch in arguments to function call: {}", name);
                    }
                }
                return funct.return_type.clone();
            }
        }
        panic!("undeclared function with name: {}", name)
    },
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
    
    // ADDED need to wrap input in parenthesis to make valid SExpr as posted on piazza
    let wrapped = format!("({})", in_contents);
    let expr = parse_prog(&parse(&wrapped).unwrap());
    let ctx: HashMap<String, Type> = HashMap::new(); 
    let expr_type = typecheck(&expr.main_expr, &expr.functions, &ctx);
    let result = compile(&expr, &ctx);

    let type_flag = if expr_type == Type::Int {0} else {1};
    let asm_program = format!(
        "
section .text
extern snek_error
extern snek_print
global our_code_starts_here
our_code_starts_here:
{}
done:
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



