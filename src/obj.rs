use std::cell::Cell;
use std::fmt;
use std::mem::discriminant;
use crate::scanner::Token;
use crate::utils::{next_boundary, previous_boundary};

#[derive(Debug, Clone)]
pub struct Obj<'a> {
  pub next: Cell<Option<&'a Obj<'a>>>,
  pub value: ObjValue,
}

#[derive(Debug, Clone)]
pub enum ObjValue {
  String(String),
}

impl fmt::Display for Obj<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match &self.value {
      ObjValue::String(store) => write!(f, "{}", store),
    }
  }
}

impl PartialEq for Obj<'_> {
  fn eq(&self, other: &Obj) -> bool {
    if discriminant(&self.value) != discriminant(&other.value) {
      return false
    }

    match &self.value {
      ObjValue::String(str1) => match &other.value {
        ObjValue::String(str2) => str1 == str2,
      }
    }
  }
}

impl Obj<'_> {
  pub fn new(value: ObjValue) -> Self {
    Self { next: Cell::new(Option::None), value }
  }
}

pub fn copy_string(token: &Token) -> String {
  let start = next_boundary(&token.lexeme, 0);
  let end = previous_boundary(&token.lexeme, token.lexeme.len());

  token.lexeme[start..end].to_string()
}
