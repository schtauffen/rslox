use crate::scanner::{Scanner, Token, TokenKind};

pub struct Parser {
  pub current: Token,
  pub previous: Token,
  pub had_error: bool,
  pub panic_mode: bool,
  scanner: Scanner,
}

impl Parser {
  pub fn new(source: String) -> Self {
    Self {
      scanner: Scanner::new(source),
      current: Token::new(TokenKind::Nil, "nil", -1),
      previous: Token::new(TokenKind::Nil, "nil", -1),
      had_error: false,
      panic_mode: false,
    }
  }

  pub fn advance(&mut self) {
    self.previous = self.current.clone();

    loop {
      self.current = self.scanner.scan();
      if let TokenKind::Error = self.current.kind {
        self.error_at_current("Encountered error");
      } else {
        break;
      }
    }
  }

  pub fn consume(&mut self, kind: TokenKind, message: &str) {
    if self.check(kind) {
      self.advance();
      return;
    }

    self.error_at_current(message);
  }

  fn check (&self, kind: TokenKind) -> bool {
    self.current.kind == kind
  }

  pub fn match_token(&mut self, kind: TokenKind) -> bool {
    if !self.check(kind) {
      return false
    }

    self.advance();

    true
  }

  pub fn error(&mut self, message: &str) {
    let token = self.previous.clone();
    self.error_at(token, message);
  }

  pub fn error_at_current(&mut self, message: &str) {
    let token = self.current.clone();
    self.error_at(token, message);
  }

  fn error_at(&mut self, token: Token, message: &str) {
    if self.panic_mode {
      return;
    }

    self.panic_mode = true;
    eprint!("[line {}] Error", token.line);

    match token.kind {
      TokenKind::EOF => eprint!(" at end"),
      TokenKind::Error => (),
      _ => eprint!(" at '{}'", token.lexeme),
    }

    eprintln!(": {}", message);
    self.had_error = true;
  }
}