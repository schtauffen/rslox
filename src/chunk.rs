use crate::value::Value;

pub mod opcode {
  // pub const ILLEGAL: u8 = 0;

  // Literals
  pub const CONSTANT: u8 = 1;
  pub const NIL: u8 = 2;
  pub const TRUE: u8 = 3;
  pub const FALSE: u8 = 4;

  // Binary
  pub const ADD: u8 = 5;
  pub const DIVIDE: u8 = 6;
  pub const EQUAL: u8 = 7;    // TODO - NOT_EQUAL
  pub const GREATER: u8 = 8;  // TODO - GREATER_EQUAL
  pub const LESS: u8 = 9;     // TODO - LESS_EQUAL
  pub const MULTIPLY: u8 = 10;
  pub const SUBTRACT: u8 = 11;

  // Unary
  pub const NEGATE: u8 = 12;
  pub const NOT: u8 = 13;

  pub const RETURN: u8 = 14;
}

#[derive(Debug, Default)]
pub struct Chunk<'a> {
  pub code: Vec<u8>,
  pub constants: Vec<Value<'a>>,
  pub lines: Vec<i32>, // TODO - more efficient lines implementation
}

impl<'a> Chunk<'a> {
  pub fn new() -> Self {
    Self::default()
  }

  pub fn write(&mut self, byte: u8, line: i32) {
    self.code.push(byte);
    self.lines.push(line);
  }

  // TODO - OP_CONSTANT_LONG w/ write_constant and add_consant_long
  pub fn add_constant(&mut self, value: Value<'a>) -> usize {
    self.constants.push(value);
    self.constants.len() - 1
  }
}
