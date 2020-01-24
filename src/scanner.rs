use crate::utils::{next_boundary, previous_boundary};

#[derive(Clone)]
pub struct Token {
  pub kind: TokenKind,
  pub lexeme: String,
  pub line: i32,
}

impl Token {
  pub fn new(kind: TokenKind, raw: &str, line: i32) -> Token {
    let lexeme = raw.to_string();
    Token { kind, lexeme, line }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
  // Single-character
  LeftParen,
  RightParen,
  LeftBrace,
  RightBrace,
  Comma,
  Dot,
  Minus,
  Plus,
  Semicolon,
  Slash,
  Star,

  // One or two character(s)
  Bang,
  BangEqual,
  Equal,
  EqualEqual,
  Greater,
  GreaterEqual,
  Less,
  LessEqual,

  // Literals
  Identifier,
  String,
  Number,

  // Keywords
  And,
  Class,
  Else,
  False,
  For,
  Fun,
  If,
  Nil,
  Or,
  Print,
  Return,
  Super,
  This,
  True,
  Var,
  While,

  Error,
  EOF,
}

pub struct Scanner {
  source: String,

  char_start: usize,
  current: usize,
  start: usize,

  line: i32,
}

const END_OF_FILE: &str = "";
const UNTERMINATED_STRING: &str = "Unterminated string.";
const UNKNOWN_CHARACTER: &str = "Unknown token.";

impl<'a> Scanner {
  pub fn new(source: String) -> Scanner {
    let current = next_boundary(&source, 0);

    Scanner {
      source,

      char_start: 0,
      current,
      start: 0,

      line: 1,
    }
  }

  pub fn scan(&mut self) -> Token {
    self.skip_white_space();

    self.start = previous_boundary(&self.source, self.current);
    self.char_start = self.start;

    if self.is_at_end() {
      return Token::new(TokenKind::EOF, END_OF_FILE, self.line);
    }

    let char_start = self.char_start;
    let current = self.current;

    self.advance_indices();
    match &self.source[char_start..current] {
      "(" => self.make_token(TokenKind::LeftParen),
      ")" => self.make_token(TokenKind::RightParen),
      "{" => self.make_token(TokenKind::LeftBrace),
      "}" => self.make_token(TokenKind::RightBrace),
      ";" => self.make_token(TokenKind::Semicolon),
      "," => self.make_token(TokenKind::Comma),
      "." => self.make_token(TokenKind::Dot),
      "-" => self.make_token(TokenKind::Minus),
      "+" => self.make_token(TokenKind::Plus),
      "/" => self.make_token(TokenKind::Slash),
      "*" => self.make_token(TokenKind::Star),
      "!" => {
        if self.match_char("=") {
          self.make_token(TokenKind::BangEqual)
        } else {
          self.make_token(TokenKind::Bang)
        }
      }
      "=" => {
        if self.match_char("=") {
          self.make_token(TokenKind::EqualEqual)
        } else {
          self.make_token(TokenKind::Equal)
        }
      }
      "<" => {
        if self.match_char("=") {
          self.make_token(TokenKind::LessEqual)
        } else {
          self.make_token(TokenKind::Less)
        }
      }
      ">" => {
        if self.match_char("=") {
          self.make_token(TokenKind::GreaterEqual)
        } else {
          self.make_token(TokenKind::Greater)
        }
      }
      "\"" => self.string(),
      c => {
        if is_digit(c) {
          return self.number();
        }

        if is_alpha(c) {
          return self.identifier();
        }

        self.error_token(UNKNOWN_CHARACTER)
      }
    }
  }

  fn skip_white_space(&mut self) {
    while !self.is_at_end() {
      let c = self.peek();

      match c {
        " " | "\r" | "\t" => {
          self.advance_indices();
        }
        "\n" => {
          self.line += 1;
          self.advance_indices();
        }
        "/" => match self.peek_next() {
          Some(next) => {
            if next == "/" {
              while self.peek() != "\n" && !self.is_at_end() {
                self.advance_indices();
              }
            } else {
              return;
            }
          }
          None => return,
        },
        _ => return,
      }
    }
  }

  fn string(&mut self) -> Token {
    while self.peek() != "\"" && !self.is_at_end() {
      if self.peek() == "\n" {
        self.line += 1;
      }
      self.advance_indices();
    }

    if self.is_at_end() {
      return self.error_token(UNTERMINATED_STRING);
    }

    self.advance_indices();
    self.make_token(TokenKind::String)
  }

  fn identifier_kind(&mut self) -> TokenKind {
    match self.nth_char_from(self.start, 0).unwrap() {
      "a" => self.check_keyword(1, "nd", TokenKind::And),
      "c" => self.check_keyword(1, "lass", TokenKind::Class),
      "e" => self.check_keyword(1, "lse", TokenKind::Else),
      "f" => match self.nth_char_from(self.start, 1) {
        Some(c) => match c {
          "a" => self.check_keyword(2, "lse", TokenKind::False),
          "o" => self.check_keyword(2, "r", TokenKind::For),
          "u" => self.check_keyword(2, "n", TokenKind::Fun),
          _ => TokenKind::Identifier,
        },
        None => TokenKind::Identifier,
      }
      "i" => self.check_keyword(1, "f", TokenKind::If),
      "n" => self.check_keyword(1, "il", TokenKind::Nil),
      "o" => self.check_keyword(1, "r", TokenKind::Or),
      "p" => self.check_keyword(1, "rint", TokenKind::Print),
      "r" => self.check_keyword(1, "eturn", TokenKind::Return),
      "s" => self.check_keyword(1, "uper", TokenKind::Super),
      "t" => match self.nth_char_from(self.start, 1) {
        Some(c) => match c {
          "h" => self.check_keyword(2, "is", TokenKind::This),
          "r" => self.check_keyword(2, "ue", TokenKind::True),
          _ => TokenKind::Identifier
        }
        None => TokenKind::Identifier
      }
      "v" => self.check_keyword(1, "ar", TokenKind::Var),
      "w" => self.check_keyword(1, "hile", TokenKind::While),
      _ => TokenKind::Identifier
    }
  }

  fn identifier(&mut self) -> Token {
    while is_alpha(self.peek()) || is_digit(self.peek()) {
      self.advance_indices();
    }

    let kind = self.identifier_kind();
    self.make_token(kind)
  }

  fn number(&mut self) -> Token {
    while !self.is_at_end() && is_digit(self.peek()) {
      self.advance_indices()
    }

    if !self.is_at_end() && self.peek() == "." {
      if let Some(next) = self.peek_next() {
        if is_digit(next) {
          self.advance_indices();

          while !self.is_at_end() && is_digit(self.peek()) {
            self.advance_indices();
          }
        }
      }
    }

    self.make_token(TokenKind::Number)
  }

  fn check_keyword(&self, start: usize, rest: &str, kind: TokenKind) -> TokenKind {
    let start_index = self.nth_next_boundary(self.start, start);
    let len = self.current - start_index - 1;

    if len == rest.len() && rest == &self.source[start_index..self.current - 1] {
      return kind;
    }

    TokenKind::Identifier
  }

  fn nth_char_from(&self, start: usize, n: u8) -> Option<&str> {
    let mut current_index = next_boundary(&self.source, start);
    let mut start_index = start;

    for _ in 0..n {
      start_index = current_index;
      current_index = next_boundary(&self.source, current_index);
    }

    if self.char_index_at_end(current_index) {
      None
    } else {
      Some(&self.source[start_index..current_index])
    }
  }

  fn nth_next_boundary(&self, start: usize, n: usize) -> usize {
    let mut current = start;
    for _ in 0..n {
      current = next_boundary(&self.source, current);
    }

    current
  }

  fn peek(&self) -> &str {
    &self.source[self.char_start..self.current]
  }

  fn peek_next(&self) -> Option<&str> {
    let start = self.current;
    let end = next_boundary(&self.source, self.current);

    if self.char_index_at_end(end) {
      return None;
    }

    Some(&self.source[start..end])
  }

  fn is_at_end(&self) -> bool {
    self.current > self.source.len()
  }

  fn char_index_at_end(&self, index: usize) -> bool {
    index > self.source.len()
  }

  fn match_char(&mut self, expected: &str) -> bool {
    if self.is_at_end() {
      return false;
    }

    if self.peek() != expected {
      return false;
    }

    self.advance_indices();
    true
  }

  fn advance_indices(&mut self) {
    self.char_start = self.current;
    self.current = next_boundary(&self.source, self.current);
  }

  fn current_slice(&self) -> &str {
    &self.source[self.start..self.current - 1]
  }

  fn make_token(&self, kind: TokenKind) -> Token {
    Token::new(kind, self.current_slice(), self.line)
  }

  fn error_token(&self, message: &str) -> Token {
    Token::new(TokenKind::Error, message, self.line)
  }
}

fn is_alpha(c: &str) -> bool {
  (c >= "a" && c <= "z") ||
  (c >= "A" && c <= "Z") ||
   c == "_"
}
  
fn is_digit(c: &str) -> bool {
  c >= "0" && c <= "9"
}
