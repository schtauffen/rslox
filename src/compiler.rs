use std::cell::RefCell;
use std::rc::Rc;
use crate::chunk::{Op, Chunk};
use crate::debug;
use crate::interner::StringInterner;
use crate::obj::{copy_string, Obj, ObjValue};
use crate::parser::Parser;
use crate::scanner::{Token, TokenKind};
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
  And,
  Binary,
  Grouping,
  Unary,
  Literal,
  Number,
  Or,
  String,
  Variable,
}

struct Local {
  name: Token,
  depth: Option<u16>,
}

impl Local {
  pub fn new(name: Token, depth: Option<u16>) -> Self {
    Self { name, depth }
  }
}

pub struct Compiler<'a, 'c: 'a> {
  allocate: &'a dyn Fn(ObjValue) -> Obj<'c>,
  interner: Rc<RefCell<StringInterner>>,
  parser: Parser,
  compiling_chunk: Chunk<'c>,
  locals: Vec<Local>, // TODO - allow more locals
  scope_depth: u16,
}

impl<'a, 'c: 'a> Compiler<'a, 'c> {
  pub fn new(
    source: String,
    allocate: &'a dyn Fn(ObjValue) -> Obj<'c>,
    interner: Rc<RefCell<StringInterner>>,
  ) -> Self {
    Self {
      allocate,
      interner,
      parser: Parser::new(source),
      compiling_chunk: Chunk::default(),
      locals: Vec::new(),
      scope_depth: 0,
    }
  }

  pub fn compile(mut self) -> Result<Chunk<'c>, ()> {
    self.parser.advance();

    while !self.parser.match_token(TokenKind::EOF) {
      self.declaration();
    }

    self.end_compiler();
    if self.parser.had_error {
      Err(())
    } else {
      Ok(self.compiling_chunk)
    }
  }

  fn emit_byte<T>(&mut self, byte: T)
  where T: Into<u8> {
    let line = self.parser.previous.line; // current?
    self.current_chunk().write(byte, line);
  }

  fn emit_bytes<T, U>(&mut self, byte1: T, byte2: U)
  where T: Into<u8>, U: Into<u8> {
    self.emit_byte(byte1);
    self.emit_byte(byte2);
  }

  fn emit_loop(&mut self, loop_start: usize) {
    self.emit_byte(Op::Loop);

    let offset = self.current_chunk().code.len() - loop_start + 2;
    if offset > std::u16::MAX as usize {
      self.parser.error("Loop body too large.");
    }

    // TODO : helper of u16 -> (u8, u8) && (u8, u8) -> u16 
    self.emit_byte((offset >> 8) as u8);
    self.emit_byte(offset as u8);
  }

  fn emit_jump(&mut self, op: Op) -> usize { // TODO - JUMP vs LONG_JUMP?
    self.emit_byte(op);
    self.emit_byte(0u8);
    self.emit_byte(0u8);
    self.current_chunk().code.len() - 2
  }

  fn emit_return(&mut self) {
    self.emit_byte(Op::Return);
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
    self.emit_bytes(Op::Constant, constant);
  }

  fn patch_jump(&mut self, offset: usize) {
    // -2 due to size of jump offset itself
    let jump = self.current_chunk().code.len() - offset - 2;

    if jump > std::u16::MAX as usize {
      self.parser.error("Too much code to jump over.");
    }

    self.current_chunk().code[offset] = (jump >> 8) as u8;
    self.current_chunk().code[offset + 1] = jump as u8; 
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

  fn begin_scope(&mut self) {
    self.scope_depth += 1;
  }

  fn end_scope(&mut self) {
    self.scope_depth -= 1;

    while let Some(local) = self.locals.last() {
      match local.depth {
        Some(depth) => if depth <= self.scope_depth  {
          break;
        },
        None => unreachable!("Local depths should be defined."),
      }

      self.emit_byte(Op::Pop);
      self.locals.pop();
    }
  }

  fn execute_action(&mut self, action: Act, can_assign: bool) {
    match action {
      Act::And => self.and(),
      Act::Binary => self.binary(),
      Act::Grouping => self.grouping(),
      Act::Literal => self.literal(),
      Act::Number => self.number(),
      Act::Or => self.or(),
      Act::String => self.string(),
      Act::Unary => self.unary(),
      Act::Variable => self.variable(can_assign),
    }
  }

  fn binary(&mut self) {
    let kind = self.parser.previous.kind.clone();
    let precedence = ParseRule::get_rule(kind.clone()).precedence.higher();
    self.parse_precedence(precedence);

    match kind {
      TokenKind::BangEqual => self.emit_bytes(Op::Equal, Op::Not),
      TokenKind::EqualEqual => self.emit_byte(Op::Equal),
      TokenKind::Greater => self.emit_byte(Op::Greater),
      TokenKind::GreaterEqual => self.emit_bytes(Op::Less, Op::Not),
      TokenKind::Less => self.emit_byte(Op::Less),
      TokenKind::LessEqual => self.emit_bytes(Op::Greater, Op::Not),
      TokenKind::Plus => self.emit_byte(Op::Add),
      TokenKind::Minus => self.emit_byte(Op::Subtract),
      TokenKind::Star => self.emit_byte(Op::Multiply),
      TokenKind::Slash => self.emit_byte(Op::Divide),
      _ => unreachable!(),
    }
  }

  fn literal(&mut self) {
    match self.parser.previous.kind {
      TokenKind::False => self.emit_byte(Op::False),
      TokenKind::Nil => self.emit_byte(Op::Nil),
      TokenKind::True => self.emit_byte(Op::True),
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

  fn or(&mut self) { // TODO - JumpIfTrue would make this as fast as 'and'
    let else_jump = self.emit_jump(Op::JumpIfFalse);
    let end_jump = self.emit_jump(Op::Jump);

    self.patch_jump(else_jump);
    self.emit_byte(Op::Pop);

    self.parse_precedence(Precedence::Or);
    self.patch_jump(end_jump);
  }

  fn string(&mut self) {
    let string = copy_string(&self.parser.previous.clone());
    let value = self.string_object(string);
    self.emit_constant(value);
  }

  fn string_object<T>(&mut self, string: T) -> Value<'c>
  where T: Into<String> + AsRef<str> {
    let symbol = {
      let mut interner = self.interner.borrow_mut();
      interner.get_or_intern(string)
    };

    let obj = (self.allocate)(ObjValue::String(symbol));

    Value::Obj(obj)
  }

  fn resolve_local(&mut self, token: &Token) -> Option<u8> {
    for (index, local) in self.locals.iter().rev().enumerate() {
      if token.lexeme == local.name.lexeme {
        match local.depth {
          Some(_) => return Some(index as u8),
          None => self
            .parser
            .error("Cannot read local variable in its own initializer."),
        }
      } 
    }

    None
  }

  fn named_variable(&mut self, token: &Token, can_assign: bool) {
   let (get_op, set_op, arg) = match self.resolve_local(&token) {
      Some(local) => (
        Op::GetLocal,
        Op::SetLocal,
        local
      ),
      None => (
        Op::GetGlobal,
        Op::SetGlobal,
        self.identifier_constant(&token)
      ),
    };

    if can_assign && self.parser.match_token(TokenKind::Equal) {
      self.expression();
      self.emit_bytes(set_op, arg);
    } else {
      self.emit_bytes(get_op, arg);
    }
  }

  fn variable(&mut self, can_assign: bool) {
    self.named_variable(&self.parser.previous.clone(), can_assign);
  }

  fn unary(&mut self) {
    let kind = self.parser.previous.kind.clone();

    self.parse_precedence(Precedence::Unary);

    match kind {
      TokenKind::Bang => self.emit_byte(Op::Not),
      TokenKind::Minus => self.emit_byte(Op::Negate),
      _ => unreachable!()
    }
  }

  fn expression(&mut self) {
    self.parse_precedence(Precedence::Assignment);
  }

  fn parse_variable(&mut self, error_message: &str) -> u8 {
    self.parser.consume(TokenKind::Identifier, error_message);

    self.declare_variable();
    if self.scope_depth > 0 {
      return 0
    }

    self.identifier_constant(&self.parser.previous.clone())
  }

  fn mark_initialized(&mut self) {
    // TODO - need to hold reference for GC?
    let mut local = self.locals.pop().unwrap();
    local.depth = Some(self.scope_depth);
    self.locals.push(local);
  }

  fn add_local(&mut self, name: Token) {
    if self.locals.len() > std::u8::MAX as usize {
      self.parser.error("Too many local variables in function.");
      return
    }

    let local = Local::new(name, None);
    self.locals.push(local);
  }

  fn declare_variable(&mut self) {
    if self.scope_depth == 0 {
      return
    }

    let name = self.parser.previous.clone();

    for local in self.locals.iter().rev() {
      match local.depth {
        Some(depth) => if depth < self.scope_depth {
          break;
        },
        None => (),
      }

      if name.lexeme == local.name.lexeme {
        self
          .parser
          .error("Variable with this name already declared in this scope.");
      }
    }

    self.add_local(name);
  }

  fn define_variable(&mut self, global: u8) {
    if self.scope_depth > 0 {
      self.mark_initialized();
      return
    }

    self.emit_bytes(Op::DefineGlobal, global);
  }

  fn and(&mut self) {
    let end_jump = self.emit_jump(Op::JumpIfFalse);

    self.emit_byte(Op::Pop);
    self.parse_precedence(Precedence::And);

    self.patch_jump(end_jump);
  }

  fn identifier_constant(&mut self, token: &Token) -> u8 {
    let value = self.string_object(&token.lexeme);
    self.make_constant(value)
  }

  fn var_declaration(&mut self) {
    let global = self.parse_variable("Expect variable name.");

    if self.parser.match_token(TokenKind::Equal) {
      self.expression();
    } else {
      self.emit_byte(Op::Nil);
    }
    self.parser.consume(TokenKind::Semicolon, "Expect ';' after variable declaration.");

    self.define_variable(global);
  }

  fn expression_statement(&mut self) {
    self.expression();
    self.parser.consume(TokenKind::Semicolon, "Expect ';' after expression.");
    self.emit_byte(Op::Pop);
  }

  fn if_statement(&mut self) {
    self.parser.consume(TokenKind::LeftParen, "Expect '(' after 'if'.");
    self.expression();
    self.parser.consume(TokenKind::RightParen, "Expect ')' after condition.");

    let then_jump = self.emit_jump(Op::JumpIfFalse);
    self.emit_byte(Op::Pop);
    self.statement();

    let else_jump = self.emit_jump(Op::Jump);
    self.patch_jump(then_jump);
    self.emit_byte(Op::Pop);

    if self.parser.match_token(TokenKind::Else) {
      self.statement();
    }
    self.patch_jump(else_jump);
  }

  fn print_statement(&mut self) {
    self.expression();
    self.parser.consume(TokenKind::Semicolon, "Expect ';' after value.");
    self.emit_byte(Op::Print);
  }

  fn while_statement(&mut self) {
    let loop_start = self.current_chunk().code.len();

    self.parser.consume(TokenKind::LeftParen, "Expect '(' after 'while'.");
    self.expression();
    self.parser.consume(TokenKind::RightParen, "Expect ')' after condition.");

    let exit_jump = self.emit_jump(Op::JumpIfFalse);

    self.emit_byte(Op::Pop);
    self.statement();

    self.emit_loop(loop_start);

    self.patch_jump(exit_jump);
    self.emit_byte(Op::Pop);
  }

  fn synchronize(&mut self) {
    self.parser.panic_mode = false;

    while self.parser.current.kind != TokenKind::EOF {
      if self.parser.previous.kind == TokenKind::Semicolon {
        return;
      }

      match self.parser.current.kind {
        TokenKind::Class => return,
        TokenKind::Fun => return,
        TokenKind::Var => return,
        TokenKind::For => return,
        TokenKind::If => return,
        TokenKind::While => return,
        TokenKind::Print => return,
        TokenKind::Return => return,
        _ => (),
      }

      self.parser.advance();
    }
  }

  fn declaration(&mut self) {
    if self.parser.match_token(TokenKind::Var) {
      self.var_declaration();
    } else {
      self.statement();
    }

    if self.parser.panic_mode {
      self.synchronize();
    }
  }

  fn block (&mut self) {
    while
      !self.parser.check(TokenKind::RightBrace) &&
      !self.parser.check(TokenKind::EOF)
    {
      self.declaration();
    }

    self.parser.consume(TokenKind::RightBrace, "Expect '}' after block.");
  }

  fn statement(&mut self) {
    if self.parser.match_token(TokenKind::Print) {
      self.print_statement();
    } else if self.parser.match_token(TokenKind::If) {
      self.if_statement();
    } else if self.parser.match_token(TokenKind::While) {
      self.while_statement();
    } else if self.parser.match_token(TokenKind::LeftBrace) {
      self.begin_scope();
      self.block();
      self.end_scope();
    } else {
      self.expression_statement();
    }
  }

  fn parse_precedence(&mut self, precedence: Precedence) {
    self.parser.advance();

    let prefix_fn = ParseRule::get_rule(self.parser.previous.kind.clone()).prefix.clone();
    let can_assign = precedence <= Precedence::Assignment;

    if let Some(action) = prefix_fn {
      self.execute_action(action, can_assign);
    } else {
      self.parser.error("Expect expression.");
      return;
    }

    while precedence <= ParseRule::get_rule(self.parser.current.kind.clone()).precedence {
      self.parser.advance();
      let infix_fn = ParseRule::get_rule(self.parser.previous.kind.clone()).infix.clone().unwrap();
      self.execute_action(infix_fn, can_assign);
    }

    if can_assign && self.parser.match_token(TokenKind::Equal) {
      self.parser.error("Invalid assignment target.");
    }
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
  ParseRule::new(Some(Act::Variable), None, Precedence::None),  // Identifier
  ParseRule::new(Some(Act::String), None, Precedence::None),    // String
  ParseRule::new(Some(Act::Number), None, Precedence::None),    // Number
  ParseRule::new(None, Some(Act::And), Precedence::And),        // And
  ParseRule::new(None, None, Precedence::None),                 // Class
  ParseRule::new(None, None, Precedence::None),                 // Else
  ParseRule::new(Some(Act::Literal), None, Precedence::None),   // False
  ParseRule::new(None, None, Precedence::None),                 // For
  ParseRule::new(None, None, Precedence::None),                 // Fun
  ParseRule::new(None, None, Precedence::None),                 // If
  ParseRule::new(Some(Act::Literal), None, Precedence::None),   // Nil
  ParseRule::new(None, Some(Act::Or), Precedence::Or),          // Or
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
