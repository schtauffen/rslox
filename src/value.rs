use std::cell::RefCell;
use std::fmt;
use std::mem::discriminant;
use std::rc::Rc;
use crate::interner::StringInterner;
use crate::obj::{Obj, ObjValue};

#[derive(Debug, Clone)]
pub enum Value<'a> {
  Bool(bool),
  Nil,
  Number(f64),
  Obj(Obj<'a>),
}

impl<'a> Default for Value<'a> {
  fn default() -> Value<'a> {
    Value::Nil
  }
}

impl<'a> fmt::Display for Value<'a> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Value::Number(n) => write!(f, "{}", n),
      Value::Bool(b) => write!(f, "{}", b),
      Value::Nil => write!(f, "nil"),
      Value::Obj(obj) => write!(f, "{}", obj),
    }
  }
}

impl<'a> PartialEq for Value<'a> {
  fn eq(&self, other: &Value<'a>) -> bool {
    if discriminant(self) != discriminant(other) {
      return false
    }

    match self {
      Value::Number(a) => match other {
        Value::Number(b) => a == b,
        _ => unreachable!(),
      },
      Value::Bool(a) => match other {
        Value::Bool(b) => a == b,
        _ => unreachable!(),
      },
      Value::Nil => true,
      Value::Obj(a) => match other {
        Value::Obj(b) => a == b,
        _ => unreachable!(),
      },
    }
  }
}

impl<'a> Value<'a> {
  pub fn is_falsey(&self) -> bool {
    match self {
      Value::Nil => true,
      Value::Bool(b) => !b,
      _ => false,
    }
  }

  pub fn print(&self, interner: Rc<RefCell<StringInterner>>) {
    match self {
      Value::Obj(obj) => match obj.value {
        ObjValue::String(symbol) => {
          match interner.borrow().resolve(symbol) {
            Some(string) => print!("'{}'", string),
            None => unreachable!("Shouldn't be deleted"),
          }
        },
      },
      _ => print!("{}", self),
    }
  }

  // TODO - ?
  // to_int
  // to_bool
  // move_object
}
