// original source: https://github.com/Robbepop/string-interner
// adapted for needs of rslox for learning purposes

use std::{
  collections::HashMap,
  fmt,
  hash::{Hash, Hasher},
  num::NonZeroU32,
  u32,
};

/**
 * SYMBOL
 */
pub trait Sym: Copy + Ord + Eq {
  fn from_usize(val: usize) -> Self;

  fn to_usize(self) -> usize;
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Symbol(NonZeroU32);

impl Sym for Symbol {
  fn from_usize(val: usize) -> Self {
    assert!(val < u32::MAX as usize, "Symbol value {} must be < u32::MAX", val);

    Symbol(NonZeroU32::new((val + 1) as u32).unwrap_or_else(|| {
      unreachable!("0 < val + 1 <= u32::MAX")
    }))
  }

  fn to_usize(self) -> usize {
    (self.0.get() as usize) - 1
  }
}

impl fmt::Display for Symbol {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "Symbol {}", self.0.get())
  }
}

impl Sym for usize {
  fn from_usize(val: usize) -> Self {
    val
  }

  fn to_usize(self) -> usize {
    self
  }
}

/**
 * INTERNAL STRING REFERENCE
 */
#[derive(Debug, Eq)]
struct InternalStrRef(*const str);

impl InternalStrRef {
  fn from_str(val: &str) -> Self {
    Self(val as *const str)
  }

  fn as_str(&self) -> &str {
    unsafe { &*self.0 }
  }
}

impl<T> From<T> for InternalStrRef
where T: AsRef<str> {
  fn from(val: T) -> Self {
    InternalStrRef::from_str(val.as_ref())
  }
}

impl Hash for InternalStrRef {
  fn hash<H: Hasher>(&self, state: &mut H) {
    self.as_str().hash(state)
  }
}

impl PartialEq for InternalStrRef {
  fn eq(&self, other: &InternalStrRef) -> bool {
    self.as_str() == other.as_str()
  }
}

/**
 * STRING INTERNER
 */
#[derive(Debug)]
pub struct StringInterner {
  map: HashMap<InternalStrRef, Symbol>,
  recycled: Vec<Symbol>,
  values: Vec<Box<str>>,
}

impl StringInterner {
  pub fn new() -> Self {
    Self {
      map: HashMap::new(),
      recycled: Vec::new(),
      values: Vec::new(),
    }
  }

  pub fn get_or_intern<T>(&mut self, val: T) -> Symbol
  where T: Into<String> + AsRef<str> {
    match self.map.get(&val.as_ref().into()) {
      Some(&symbol) => symbol,
      None => {
        if self.recycled.len() > 0 {
          let symbol = self.recycled.remove(0);
          return self.overwrite(symbol, val)
        }
        self.intern(val)
      },
    }
  }

  fn intern<T>(&mut self, string: T) -> Symbol
  where T: Into<String> + AsRef<str> {
    let symbol = self.make_symbol();
    let boxed_str = string.into().into_boxed_str();
    let string_ref: InternalStrRef = boxed_str.as_ref().into();

    self.values.push(boxed_str);
    self.map.insert(string_ref, symbol);

    symbol
  }

  fn overwrite<T>(&mut self, symbol: Symbol, string: T) -> Symbol
  where T: Into<String> + AsRef<str> {
    let boxed_str = string.into().into_boxed_str();
    let string_ref: InternalStrRef = boxed_str.as_ref().into();

    self.values[symbol.to_usize()] = boxed_str;
    self.map.insert(string_ref, symbol);

    symbol
  }

  fn len(&self) -> usize {
    self.values.len()
  }

  fn make_symbol(&self) -> Symbol {
    Symbol::from_usize(self.len())
  }

  // DANGEROUS - can cause UB if you don't verify
  // that String is unused
  pub fn delete(&mut self, symbol: Symbol) {
    self.recycled.push(symbol);
    let boxed_str = self.resolve(symbol);

    match boxed_str {
      Some(string) => {
        let string_ref: InternalStrRef = string.into();
        self.map.remove(&string_ref);
      },
      _ => ()
    }
  }

  pub fn resolve(&self, symbol: Symbol) -> Option<&str> {
    self.values
      .get(symbol.to_usize())
      .map(|boxed_str| boxed_str.as_ref())
  }
}
