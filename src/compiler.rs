use std::{
  cell::RefCell,
  rc::Rc,
};
use crate::{
  chunk::{Op, Chunk},
  interner::StringInterner,
  obj::{copy_string, Obj, ObjValue, Fun, FunKind},
  parser::Parser,
  scanner::{Token, TokenKind},
  value::Value,
};
#[cfg(debug_assertions)]
use crate::debug;

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
  Call,
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
  // enclosing: Compiler<?>, // TODO - refCell ? unsafe pointer?
  fun: Fun<'c>,
  fun_kind: FunKind,
  allocate: &'a dyn Fn(ObjValue<'c>) -> Obj<'c>,
  interner: Rc<RefCell<StringInterner>>,
  parser: Rc<RefCell<Parser>>, // TODO - faster (unsafe?) way to share ?
  locals: Vec<Local>, // TODO - allow more locals
  scope_depth: u16,
}

impl<'a, 'c: 'a> Compiler<'a, 'c> {
  pub fn new(
    parser: Rc<RefCell<Parser>>,
    allocate: &'a dyn Fn(ObjValue<'c>) -> Obj<'c>,
    interner: Rc<RefCell<StringInterner>>,
    fun_kind: FunKind,
  ) -> Self {
    let mut fun = Fun::new();
    fun.name = match fun_kind {
      FunKind::Script => None,
      _ => Some(parser.borrow_mut().previous.lexeme.to_string()),
    };

    Self {
      fun,
      fun_kind,
      allocate,
      interner,
      parser,
      locals: vec![
        Local::new(
          Token::new(TokenKind::Identifier, "", 0),
          Some(0),
        )
      ],
      scope_depth: 0,
    }
  }

  pub fn compile(mut self) -> Result<Rc<Fun<'c>>, ()> {
    self.parser.borrow_mut().advance();

    while !self.parser.borrow_mut().match_token(TokenKind::EOF) {
      self.declaration();
    }

    self.end_compiler();
    if self.parser.borrow().had_error {
      Err(())
    } else {
      Ok(Rc::new(self.fun))
    }
  }

  fn emit_byte<T>(&mut self, byte: T)
  where T: Into<u8> {
    let line = self.parser.borrow().previous.line; // current?
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
      self.parser.borrow_mut().error("Loop body too large.");
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
    self.emit_byte(Op::Nil);
    self.emit_byte(Op::Return);
  }

  fn make_constant(&mut self, value: Value<'c>) -> u8 {
    let constant = self
      .current_chunk()
      .add_constant(value);
    
    if constant > std::u8::MAX as usize {
      self.parser.borrow_mut().error("Too many constants in one chunk.");
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
      self.parser.borrow_mut().error("Too much code to jump over.");
    }

    self.current_chunk().code[offset] = (jump >> 8) as u8;
    self.current_chunk().code[offset + 1] = jump as u8; 
  }

  fn end_compiler(&mut self) {
    self.emit_return();

    #[cfg(debug_assertions)]
    {
      if !self.parser.borrow().had_error {
        let name = match &self.fun.name {
          Some(name) => name.clone(),
          None => "<script>".into(),
        };

        debug::disassemble_chunk(&self.current_chunk(), name.as_ref());
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
      Act::Call => self.call(),
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
    let kind = self.parser.borrow().previous.kind.clone();
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

  fn call(&mut self) {
    let arg_count = self.argument_list();
    self.emit_bytes(Op::Call, arg_count)
  }

  fn literal(&mut self) {
    let kind = { self.parser.borrow().previous.kind.clone() };
    match kind {
      TokenKind::False => self.emit_byte(Op::False),
      TokenKind::Nil => self.emit_byte(Op::Nil),
      TokenKind::True => self.emit_byte(Op::True),
      _ => unreachable!(),
    }
  }

  fn grouping(&mut self) {
    self.expression();
    self.parser.borrow_mut().consume(TokenKind::RightParen, "Expect ')' after expression.");
  }

  fn number(&mut self) {
    let value = self
      .parser
      .borrow()
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
    let string = copy_string(&self.parser.borrow().previous.clone());
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
            .borrow_mut()
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

    if can_assign && self.parser.borrow_mut().match_token(TokenKind::Equal) {
      self.expression();
      self.emit_bytes(set_op, arg);
    } else {
      self.emit_bytes(get_op, arg);
    }
  }

  fn variable(&mut self, can_assign: bool) {
    let previous = { self.parser.borrow().previous.clone() };
    self.named_variable(&previous, can_assign);
  }

  fn unary(&mut self) {
    let kind = self.parser.borrow().previous.kind.clone();

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
    self.parser.borrow_mut().consume(TokenKind::Identifier, error_message);

    self.declare_variable();
    if self.scope_depth > 0 {
      return 0
    }

    let previous = self.parser.borrow().previous.clone();
    self.identifier_constant(&previous)
  }

  fn mark_initialized(&mut self) {
    if self.scope_depth == 0 {
      return
    }
    // TODO - need to hold reference for GC?
    let mut local = self.locals.pop().unwrap();
    local.depth = Some(self.scope_depth);
    self.locals.push(local);
  }

  fn add_local(&mut self, name: Token) {
    if self.locals.len() == std::u8::MAX as usize {
      self.parser.borrow_mut().error("Too many local variables in function.");
      return
    }

    let local = Local::new(name, Some(self.scope_depth));
    self.locals.push(local);
  }

  fn declare_variable(&mut self) {
    if self.scope_depth == 0 {
      return
    }

    let name = self.parser.borrow().previous.clone();

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
          .borrow_mut()
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

  fn argument_list(&mut self) -> u8 {
    let mut arg_count = 0;
    if !self.parser.borrow().check(TokenKind::RightParen) {
      loop {
        self.expression();
        if arg_count == 255 {
          self.parser.borrow_mut().error("Cannot have more than 255 arguments.");
        }
        arg_count += 1;

        if !self.parser.borrow_mut().match_token(TokenKind::Comma) {
          break;
        }
      }
    }

    self.parser.borrow_mut().consume(TokenKind::RightParen, "Expect ')' after arguments.");
    arg_count
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

  fn fun_declaration(&mut self) {
    let global = self.parse_variable("Expected function name.");
    self.mark_initialized();
    self.function(FunKind::Fun);
    self.define_variable(global);
  }

  fn var_declaration(&mut self) {
    let global = self.parse_variable("Expect variable name.");

    if self.parser.borrow_mut().match_token(TokenKind::Equal) {
      self.expression();
    } else {
      self.emit_byte(Op::Nil);
    }
    self.parser.borrow_mut().consume(TokenKind::Semicolon, "Expect ';' after variable declaration.");

    self.define_variable(global);
  }

  fn expression_statement(&mut self) {
    self.expression();
    self.parser.borrow_mut().consume(TokenKind::Semicolon, "Expect ';' after expression.");
    self.emit_byte(Op::Pop);
  }

  fn for_statement(&mut self) {
    self.begin_scope();
    self.parser.borrow_mut().consume(TokenKind::LeftParen, "Expect '(' after 'for'.");

    // Initializer clause
    if self.parser.borrow_mut().match_token(TokenKind::Semicolon) {
      // empty so do nothing
    } else if self.parser.borrow_mut().match_token(TokenKind::Var) {
      self.var_declaration();
    } else {
      self.expression_statement();
    }

    let mut loop_start = self.current_chunk().code.len();

    // Condition clause
    let mut exit_jump = None;
    if !self.parser.borrow_mut().match_token(TokenKind::Semicolon) {
      self.expression();
      self.parser.borrow_mut().consume(TokenKind::Semicolon, "Expect ';' after loop condition.");

      exit_jump = Some(self.emit_jump(Op::JumpIfFalse));
      self.emit_byte(Op::Pop);
    }

    // Increment clause
    if !self.parser.borrow_mut().match_token(TokenKind::RightParen) {
      let body_jump = self.emit_jump(Op::Jump);

      let increment_start = self.current_chunk().code.len();
      self.expression();
      self.emit_byte(Op::Pop);
      self.parser.borrow_mut().consume(TokenKind::RightParen, "Expect ')' after for clauses.");

      self.emit_loop(loop_start);
      loop_start = increment_start;
      self.patch_jump(body_jump);
    }

    self.statement();

    self.emit_loop(loop_start);

    // if there was a condition clause, patch its exit jump
    if let Some(jump) = exit_jump {
      self.patch_jump(jump);
      self.emit_byte(Op::Pop);
    }

    self.end_scope();
  }

  fn if_statement(&mut self) {
    self.parser.borrow_mut().consume(TokenKind::LeftParen, "Expect '(' after 'if'.");
    self.expression();
    self.parser.borrow_mut().consume(TokenKind::RightParen, "Expect ')' after condition.");

    let then_jump = self.emit_jump(Op::JumpIfFalse);
    self.emit_byte(Op::Pop);
    self.statement();

    let else_jump = self.emit_jump(Op::Jump);
    self.patch_jump(then_jump);
    self.emit_byte(Op::Pop);

    if self.parser.borrow_mut().match_token(TokenKind::Else) {
      self.statement();
    }
    self.patch_jump(else_jump);
  }

  fn print_statement(&mut self) {
    self.expression();
    self.parser.borrow_mut().consume(TokenKind::Semicolon, "Expect ';' after value.");
    self.emit_byte(Op::Print);
  }

  fn return_statement(&mut self) {
    if self.fun_kind == FunKind::Script {
      self.parser.borrow_mut().error("Cannot return from top-level code.");
    }

    if self.parser.borrow_mut().match_token(TokenKind::Semicolon) {
      self.emit_return();
    } else {
      self.expression();
      self.parser.borrow_mut().consume(TokenKind::Semicolon, "Expect ';' after return value.");
      self.emit_byte(Op::Return);
    }
  }

  fn while_statement(&mut self) {
    let loop_start = self.current_chunk().code.len();

    self.parser.borrow_mut().consume(TokenKind::LeftParen, "Expect '(' after 'while'.");
    self.expression();
    self.parser.borrow_mut().consume(TokenKind::RightParen, "Expect ')' after condition.");

    let exit_jump = self.emit_jump(Op::JumpIfFalse);

    self.emit_byte(Op::Pop);
    self.statement();

    self.emit_loop(loop_start);

    self.patch_jump(exit_jump);
    self.emit_byte(Op::Pop);
  }

  fn synchronize(&mut self) {
    self.parser.borrow_mut().panic_mode = false;

    while self.parser.borrow().current.kind != TokenKind::EOF {
      if self.parser.borrow().previous.kind == TokenKind::Semicolon {
        return;
      }

      match self.parser.borrow().current.kind {
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

      self.parser.borrow_mut().advance();
    }
  }

  fn declaration(&mut self) {
    if self.parser.borrow_mut().match_token(TokenKind::Fun) {
      self.fun_declaration();
    } else if self.parser.borrow_mut().match_token(TokenKind::Var) {
      self.var_declaration();
    } else {
      self.statement();
    }

    if self.parser.borrow().panic_mode {
      self.synchronize();
    }
  }

  fn block (&mut self) {
    while
      !self.parser.borrow().check(TokenKind::RightBrace) &&
      !self.parser.borrow().check(TokenKind::EOF)
    {
      self.declaration();
    }

    self.parser.borrow_mut().consume(TokenKind::RightBrace, "Expect '}' after block.");
  }

  fn compile_function(mut self) -> Fun<'c> {
    self.begin_scope();

    // Parameter list
    self.parser
      .borrow_mut()
      .consume(TokenKind::LeftParen, "Expect '(' after function name.");
    if !self.parser.borrow().check(TokenKind::RightParen) {
      loop {
        self.fun.arity += 1;
        if self.fun.arity > 255 {
          self.parser.borrow_mut().error_at_current("Cannot have more than 255 params.");
        }

        let param_const = self.parse_variable("Expect param name.");
        self.define_variable(param_const);

        if !self.parser.borrow_mut().match_token(TokenKind::Comma) {
          break;
        }
      }
    }
    self.parser
      .borrow_mut()
      .consume(TokenKind::RightParen, "Expect ')' after parameters.");

    // Body
    self.parser
      .borrow_mut()
      .consume(TokenKind::LeftBrace, "Expect '{' before function body.");
    self.block();

    self.end_compiler();
    self.fun
  }

  fn function(&mut self, fun_kind: FunKind) {
    let fun_compiler = Compiler::new(
      self.parser.clone(),
      &self.allocate,
      self.interner.clone(),
      fun_kind
    );
    let fun = fun_compiler.compile_function();
    self.emit_constant(Value::Obj(Obj::new(ObjValue::Fun(Rc::new(fun)))));
  }

  fn statement(&mut self) {
    if self.parser.borrow_mut().match_token(TokenKind::Print) {
      self.print_statement();
    } else if self.parser.borrow_mut().match_token(TokenKind::For) {
      self.for_statement();
    } else if self.parser.borrow_mut().match_token(TokenKind::If) {
      self.if_statement();
    } else if self.parser.borrow_mut().match_token(TokenKind::Return) {
      self.return_statement();
    } else if self.parser.borrow_mut().match_token(TokenKind::While) {
      self.while_statement();
    } else if self.parser.borrow_mut().match_token(TokenKind::LeftBrace) {
      self.begin_scope();
      self.block();
      self.end_scope();
    } else {
      self.expression_statement();
    }
  }

  fn parse_precedence(&mut self, precedence: Precedence) {
    self.parser.borrow_mut().advance();

    let prefix_fn = ParseRule::get_rule(self.parser.borrow().previous.kind.clone()).prefix.clone();
    let can_assign = precedence <= Precedence::Assignment;

    if let Some(action) = prefix_fn {
      self.execute_action(action, can_assign);
    } else {
      self.parser.borrow_mut().error("Expect expression.");
      return;
    }

    while precedence <= ParseRule::get_rule(self.parser.borrow().current.kind.clone()).precedence {
      self.parser.borrow_mut().advance();
      let infix_fn = ParseRule::get_rule(self.parser.borrow().previous.kind.clone()).infix.clone().unwrap();
      self.execute_action(infix_fn, can_assign);
    }

    if can_assign && self.parser.borrow_mut().match_token(TokenKind::Equal) {
      self.parser.borrow_mut().error("Invalid assignment target.");
    }
  }

  fn current_chunk(&mut self) -> &mut Chunk<'c> {
    &mut self.fun.chunk
  }
}

// TODO - mixfix: ternary ?
const RULES_TABLE: [ParseRule; 40] = [
  ParseRule::new(Some(Act::Grouping), Some(Act::Call), Precedence::Call),// LeftParen
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
