use std::cell::RefCell;
use std::rc::Rc;
use crate::chunk::{opcode, Chunk};
use crate::debug;
use crate::interner::StringInterner;
use crate::obj::{copy_string, Obj, ObjValue};
use crate::parser::Parser;
use crate::scanner::TokenKind;
use crate::value::Value;

#[derive(PartialEq, PartialOrd)]
enum Precedence {
  None,
  Assignment, // =
  Or,         // or
  And,        // and
  Equality,   // == !=
  Comparison, // < > <= >=
  Term,       // + -
  Factor,     // * /
  Unary,      // ! -
  Call,       // . ()
  Primary,
}

impl Precedence {
  fn higher(&self) -> Precedence {
    match self {
      Precedence::None => Precedence::Assignment,
      Precedence::Assignment => Precedence::Or,
      Precedence::Or => Precedence::And,
      Precedence::And => Precedence::Equality,
      Precedence::Equality => Precedence::Comparison,
      Precedence::Comparison => Precedence::Term,
      Precedence::Term => Precedence::Factor,
      Precedence::Factor => Precedence::Unary,
      Precedence::Unary => Precedence::Call,
      Precedence::Call => Precedence::Primary,
      Precedence::Primary => panic!("Primary is highest precedence"),
    }
  }
}

struct ParseRule {
  prefix: Option<Act>,
  infix: Option<Act>,
  precedence: Precedence,
}

impl ParseRule {
  const fn new(prefix: Option<Act>, infix: Option<Act>,
    precedence: Precedence) -> Self {
    Self { prefix, infix, precedence }
  }

  const fn get_rule(kind: TokenKind) -> &'static Self {
    &RULES_TABLE[kind as usize]
  }
}

#[derive(Clone)]
enum Act {
  Binary,
  Grouping,
  Unary,
  Literal,
  Number,
  String,
}

pub struct Compiler<'a, 'c: 'a> {
  allocate: &'a dyn Fn(ObjValue) -> Obj<'c>,
  interner: Rc<RefCell<StringInterner>>,
  parser: Parser,
  compiling_chunk: Chunk<'c>,
}

impl<'a, 'c: 'a> Compiler<'a, 'c> {
  pub fn new(
    source: String,
    allocate: &'a dyn Fn(ObjValue) -> Obj<'c>,
    interner: Rc<RefCell<StringInterner>>,
  ) -> Self {
    let compiling_chunk = Chunk::default();
    let parser = Parser::new(source);
    Self { allocate, interner, parser, compiling_chunk }
  }

  pub fn compile(mut self) -> Result<Chunk<'c>, ()> {
    self.parser.advance();
    self.expression();
    self.parser.consume(TokenKind::EOF, "Expected end of expression.");

    self.end_compiler();
    if self.parser.had_error {
      Err(())
    } else {
      Ok(self.compiling_chunk)
    }
  }

  fn emit_byte(&mut self, byte: u8) {
    let line = self.parser.previous.line; // current?
    self.current_chunk().write(byte, line);
  }

  fn emit_bytes(&mut self, byte1: u8, byte2: u8) {
    self.emit_byte(byte1);
    self.emit_byte(byte2);
  }

  fn emit_return(&mut self) {
    self.emit_byte(opcode::RETURN);
  }

  fn make_constant(&mut self, value: Value<'c>) -> u8 {
    let constant = self
      .current_chunk()
      .add_constant(value);
    
    if constant > std::u8::MAX as usize {
      self.parser.error("Too many constants in one chunk.");
      return 0;
    }

    return constant as u8;
  }

  fn emit_constant(&mut self, value: Value<'c>) {
    let constant = self.make_constant(value);
    self.emit_bytes(opcode::CONSTANT, constant);
  }

  fn end_compiler(&mut self) {
    self.emit_return();

    #[cfg(debug_assertions)]
    {
      if !self.parser.had_error {
        debug::disassemble_chunk(self.current_chunk(), "code");
      }
    }
  }

  fn execute_action(&mut self, action: Act) {
    match action {
      Act::Binary => self.binary(),
      Act::Grouping => self.grouping(),
      Act::Literal => self.literal(),
      Act::Number => self.number(),
      Act::String => self.string(),
      Act::Unary => self.unary(),
    }
  }

  fn binary(&mut self) {
    let kind = self.parser.previous.kind.clone();
    let precedence = ParseRule::get_rule(kind.clone()).precedence.higher();
    self.parse_precedence(precedence);

    match kind {
      TokenKind::BangEqual => self.emit_bytes(opcode::EQUAL, opcode::NOT),
      TokenKind::EqualEqual => self.emit_byte(opcode::EQUAL),
      TokenKind::Greater => self.emit_byte(opcode::GREATER),
      TokenKind::GreaterEqual => self.emit_bytes(opcode::LESS, opcode::NOT),
      TokenKind::Less => self.emit_byte(opcode::LESS),
      TokenKind::LessEqual => self.emit_bytes(opcode::GREATER, opcode::NOT),
      TokenKind::Plus => self.emit_byte(opcode::ADD),
      TokenKind::Minus => self.emit_byte(opcode::SUBTRACT),
      TokenKind::Star => self.emit_byte(opcode::MULTIPLY),
      TokenKind::Slash => self.emit_byte(opcode::DIVIDE),
      _ => unreachable!(),
    }
  }

  fn literal(&mut self) {
    match self.parser.previous.kind {
      TokenKind::False => self.emit_byte(opcode::FALSE),
      TokenKind::Nil => self.emit_byte(opcode::NIL),
      TokenKind::True => self.emit_byte(opcode::TRUE),
      _ => unreachable!(),
    }
  }

  fn grouping(&mut self) {
    self.expression();
    self.parser.consume(TokenKind::RightParen, "Expect ')' after expression.");
  }

  fn number(&mut self) {
    let value = self
      .parser
      .previous
      .lexeme
      .parse::<f64>()
      .expect("Unable to parse float");

    self.emit_constant(Value::Number(value));
  }

  fn string(&mut self) {
    let symbol = {
      let mut interner = self.interner.borrow_mut();
      interner.get_or_intern(copy_string(&self.parser.previous))
    };
    let obj = (self.allocate)(ObjValue::String(symbol));
    let value = Value::Obj(obj);
    self.emit_constant(value);
  }

  fn unary(&mut self) {
    let kind = self.parser.previous.kind.clone();

    self.parse_precedence(Precedence::Unary);

    match kind {
      TokenKind::Bang => self.emit_byte(opcode::NOT),
      TokenKind::Minus => self.emit_byte(opcode::NEGATE),
      _ => unreachable!()
    }
  }

  fn parse_precedence(&mut self, precedence: Precedence) {
    self.parser.advance();

    let prefix_fn = ParseRule::get_rule(self.parser.previous.kind.clone()).prefix.clone();
    if let Some(action) = prefix_fn {
      self.execute_action(action)
    } else {
      self.parser.error("Expect expression.");
      return;
    }

    while precedence <= ParseRule::get_rule(self.parser.current.kind.clone()).precedence {
      self.parser.advance();
      let infix_fn = ParseRule::get_rule(self.parser.previous.kind.clone()).infix.clone().unwrap();
      self.execute_action(infix_fn)
    }
  }

  fn expression(&mut self) {
    self.parse_precedence(Precedence::Assignment);
  }

  fn current_chunk(&mut self) -> &mut Chunk<'c> {
    &mut self.compiling_chunk
  }
}

// TODO - mixfix: ternary ?
const RULES_TABLE: [ParseRule; 40] = [
  ParseRule::new(Some(Act::Grouping), None, Precedence::None),  // LeftParen
  ParseRule::new(None, None, Precedence::None),                 // RightParen
  ParseRule::new(None, None, Precedence::None),                 // LeftBrace
  ParseRule::new(None, None, Precedence::None),                 // RightBrace
  ParseRule::new(None, None, Precedence::None),                 // Comma
  ParseRule::new(None, None, Precedence::None),                 // Dot
  ParseRule::new(Some(Act::Unary), Some(Act::Binary), Precedence::Term),// Minus
  ParseRule::new(None, Some(Act::Binary), Precedence::Term),    // Plus
  ParseRule::new(None, None, Precedence::None),                 // Semicolon
  ParseRule::new(None, Some(Act::Binary), Precedence::Factor),  // Slash
  ParseRule::new(None, Some(Act::Binary), Precedence::Factor),  // Star
  ParseRule::new(Some(Act::Unary), None, Precedence::None),     // Bang
  ParseRule::new(None, Some(Act::Binary), Precedence::Equality),// BangEqual
  ParseRule::new(None, None, Precedence::None),                 // Equal
  ParseRule::new(None, Some(Act::Binary), Precedence::Equality),// EqualEqual
  ParseRule::new(None, Some(Act::Binary), Precedence::Comparison),// Greater
  ParseRule::new(None, Some(Act::Binary), Precedence::Comparison),// GreaterEqual
  ParseRule::new(None, Some(Act::Binary), Precedence::Comparison),// Less
  ParseRule::new(None, Some(Act::Binary), Precedence::Comparison),// LessEqual
  ParseRule::new(None, None, Precedence::None),                 // Identifier
  ParseRule::new(Some(Act::String), None, Precedence::None),    // String
  ParseRule::new(Some(Act::Number), None, Precedence::None),    // Number
  ParseRule::new(None, None, Precedence::None),                 // And
  ParseRule::new(None, None, Precedence::None),                 // Class
  ParseRule::new(None, None, Precedence::None),                 // Else
  ParseRule::new(Some(Act::Literal), None, Precedence::None),   // False
  ParseRule::new(None, None, Precedence::None),                 // For
  ParseRule::new(None, None, Precedence::None),                 // Fun
  ParseRule::new(None, None, Precedence::None),                 // If
  ParseRule::new(Some(Act::Literal), None, Precedence::None),   // Nil
  ParseRule::new(None, None, Precedence::None),                 // Or
  ParseRule::new(None, None, Precedence::None),                 // Print
  ParseRule::new(None, None, Precedence::None),                 // Return
  ParseRule::new(None, None, Precedence::None),                 // Super
  ParseRule::new(None, None, Precedence::None),                 // This
  ParseRule::new(Some(Act::Literal), None, Precedence::None),   // True
  ParseRule::new(None, None, Precedence::None),                 // Var
  ParseRule::new(None, None, Precedence::None),                 // While
  ParseRule::new(None, None, Precedence::None),                 // Error
  ParseRule::new(None, None, Precedence::None),                 // EOF
];
