use std::{
  cell::Cell,
  fmt,
  mem::discriminant,
  rc::Rc,
};
use crate::{
  chunk::Chunk,
  interner::Symbol,
  scanner::Token,
  utils::{next_boundary, previous_boundary},
  value::Value,
};

#[derive(Debug, Clone)]
pub struct Obj<'a> {
  pub next: Cell<Option<&'a Obj<'a>>>,
  pub value: ObjValue<'a>,
}

#[derive(Debug, Clone)]
pub enum ObjValue<'a> {
  String(Symbol),
  Fun(Rc<Fun<'a>>),
  Native(Native<'a>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Fun<'a> {
  pub arity: u16,
  pub chunk: Chunk<'a>,
  pub name: Option<String>,
}

impl<'a> Default for Fun<'a> {
  fn default() -> Self {
    Self { arity: 0, chunk: Chunk::default(), name: None }
  }
}

#[derive(Clone)]
pub struct Native<'a> {
  pub name: String,
  pub arity: u8,
  pub fun: Rc<dyn Fn(&[Value<'a>]) -> Result<Value<'a>, String> + 'a>,
}

impl <'a> fmt::Debug for Native<'a> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "Native {}({})", self.name, self.arity)
  }
}

impl <'a> PartialEq for Native<'a> {
  fn eq(&self, other: &Self) -> bool {
    self.name == other.name
  }
}

impl <'a> Native<'a> {
  pub fn new(name: String, arity: u8, fun: Rc<dyn Fn(&[Value<'a>]) -> Result<Value<'a>, String> + 'a>) -> Self {
    Self {
      name,
      arity,
      fun,
    }
  }
}

#[derive(PartialEq)]
pub enum FunKind {
  Fun,
  Script,
}

impl fmt::Display for Obj<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match &self.value {
      ObjValue::String(store) => write!(f, "{}", store),
      ObjValue::Fun(fun) => match &fun.name {
        Some(name) => write!(f, "<fn {}>", name),
        None => write!(f, "<script>"),
      },
      ObjValue::Native(native) => write!(f, "<native {}>", native.name),
    }
  }
}

impl<'a> PartialEq for Obj<'a> {
  fn eq(&self, other: &Obj<'a>) -> bool {
    if discriminant(&self.value) != discriminant(&other.value) {
      return false
    }

    match &self.value {
      ObjValue::String(str1) => match &other.value {
        ObjValue::String(str2) => str1 == str2,
        _ => false,
      },
      ObjValue::Fun(fun1) => match &other.value {
        ObjValue::Fun(fun2) => fun1 == fun2,
        _ => false,
      },
      ObjValue::Native(native1) => match &other.value {
        ObjValue::Native(native2) => native1 == native2,
        _ => false,
      },
    }
  }
}
impl Eq for Obj<'_> {}

impl<'a> Obj<'a> {
  pub fn new(value: ObjValue<'a>) -> Self {
    Self { next: Cell::new(Option::None), value }
  }

  pub fn get_symbol(&self) -> Symbol {
    match self.value {
      ObjValue::String(symbol) => symbol,
      _ => panic!("Expected ObjValue::String."),
    }
  }

  pub fn get_fun_ref(&self) -> Rc<Fun<'a>> {
    match &self.value {
      ObjValue::Fun(fun) => fun.clone(),
      _ => panic!("Expected ObjValue::Fun"),
    }
  }
}

impl<'a> Fun<'a> {
  pub fn new () -> Self {
    Self { arity: 0, name: None, chunk: Chunk::new() }
  }
}

pub fn copy_string(token: &Token) -> String {
  let start = next_boundary(&token.lexeme, 0);
  let end = previous_boundary(&token.lexeme, token.lexeme.len());

  token.lexeme[start..end].to_string()
}
