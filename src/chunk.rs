use std::{fmt, mem};
use crate::value::Value;

#[repr(u8)]
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Op {
  Illegal = 0,

  // Literals
  Constant,
  Nil,
  True,
  False,

  // Binary
  Add,
  Divide,
  Equal,
  Greater,
  Less,
  Multiply,
  Subtract,

  // Unary
  Negate,
  Not,

  Pop,
  Print,
  DefineGlobal,
  SetGlobal,
  GetGlobal,
  SetLocal,
  GetLocal,
  Return,
}

impl fmt::Display for Op {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", *self as u8)
  }
}

impl From<Op> for u8 {
  fn from(op: Op) -> Self {
    op as Self
  }
}

impl From<u8> for Op {
  fn from(u: u8) -> Self {
    if u > 21 {
      return Op::Illegal
    }

    // Safe because Op is repr(u8) and we guarded against out of bounds
    unsafe { mem::transmute::<u8, Self>(u) }
  }
}

#[derive(Debug, Default)]
pub struct Chunk<'a> {
  pub code: Vec<u8>,
  pub constants: Vec<Value<'a>>, // TODO - hashmap with symbols?
  pub lines: Vec<i32>, // TODO - more efficient lines implementation
}

impl<'a> Chunk<'a> {
  pub fn new() -> Self {
    Self::default()
  }

  pub fn write<T>(&mut self, byte: T, line: i32)
  where T: Into<u8> {
    self.code.push(byte.into());
    self.lines.push(line);
  }

  // TODO - OP_CONSTANT_LONG w/ write_constant and add_consant_long
  pub fn add_constant(&mut self, value: Value<'a>) -> usize {
    self.constants.push(value);
    self.constants.len() - 1
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn it_converts_to_op_return() {
    let op: Op = 21u8.into();
    assert_eq!(Op::Return, op);
  }

  #[test]
  fn it_converts_out_of_bounds_to_illegal() {
    let op: Op = 200u8.into();
    assert_eq!(Op::Illegal, op);
  }
}
