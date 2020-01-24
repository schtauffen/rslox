use std::cell::{Cell, RefCell};
use std::collections::HashMap;
use std::mem::replace;
use std::ops::Drop;
use std::rc::Rc;

use crate::chunk::{Chunk, opcode};
use crate::compiler::Compiler;
use crate::interner::{Symbol, StringInterner};
use crate::memory::free_objects;
use crate::obj::{Obj, ObjValue};
use crate::value::Value;

#[cfg(debug_assertions)]
use crate::debug::disassemble_instruction;

const DEFAULT_STACK_MAX: usize = 500;

pub struct Vm<'a> {
  chunk: Chunk<'a>,
  ip: usize,
  stack: Vec<Value<'a>>,
  stack_top: usize,
  objects: Cell<Option<&'a Obj<'a>>>,
  globals: HashMap<Symbol, Value<'a>>,
  interner: Rc<RefCell<StringInterner>>,
}

impl<'a> Drop for Vm<'a> {
  fn drop(&mut self) {
    if let Some(obj) = self.objects.get() {
      free_objects(obj);
    }
  }
}

pub enum InterpretResult {
  Success,
  CompileError,
  RuntimeError,
}

const ADD_OPERAND_MISMATCH_ERROR: &'static str = "Operands must be two numbers or two strings.";

impl<'a> Vm<'a> {
  pub fn new() -> Self {
    Self {
      chunk: Chunk::default(),
      ip: 0,
      stack_top: 0,
      stack: vec![Value::default(); DEFAULT_STACK_MAX],
      objects: Cell::new(Option::None),
      globals: HashMap::new(),
      interner: Rc::new(RefCell::new(StringInterner::new())),
    }
  }

  fn push(&mut self, value: Value<'a>) {
    self.stack[self.stack_top] = value;
    self.stack_top += 1;
  }

  fn pop(&mut self) -> Value<'a> {
    self.stack_top -= 1;
    replace(&mut self.stack[self.stack_top], Value::Nil)
  }

  fn peek(&self, distance: usize) -> &Value<'a> {
    &self.stack[self.stack_top - (distance + 1)]
  }

  fn read_byte(&mut self) -> u8 {
    let byte = self.chunk.code[self.ip];
    self.ip += 1;
    byte
  }

  fn read_constant(&mut self) -> Value<'a> {
    let index = self.read_byte() as usize;
    self.chunk.constants[index].clone()
  }

  fn allocate(&self, value: ObjValue) -> Obj<'a> {
    let obj = Obj::new(value);
    obj.next.set(self.objects.get());
    self.objects.set(obj.next.get());

    obj
  }

  fn reset_stack(&mut self) {
    self.stack_top = 0;
  }

  fn runtime_error(&mut self, message: &str) -> InterpretResult {
    eprintln!("{}", message);
    eprintln!("[line {}] in script", self.chunk.lines[self.ip]);

    self.reset_stack();

    InterpretResult::RuntimeError
  }

  pub fn interpret(&mut self, source: String) -> InterpretResult {
    let allocate = |value: ObjValue| self.allocate(value);
    let interner = Rc::clone(&self.interner);
    let compiler = Compiler::new(source, &allocate, interner);

    match compiler.compile() {
      Err(_) => InterpretResult::CompileError,
      Ok(chunk) => {
        self.chunk = chunk;
        self.ip = 0;
        self.run()
      }
    }
  }

  fn run(&mut self) -> InterpretResult {
    macro_rules! binary_op {
      ($op:tt,$return_kind:ident) => {{
        match self.pop() {
          Value::Number(b) => match self.pop() {
            Value::Number(a) => self.push(Value::$return_kind(a $op b)),
            _ => return self.runtime_error("Operands must be numbers."),
          },
          _ => return self.runtime_error("Operands must be numbers."),
        }
      }}
    }

    loop {
      #[cfg(debug_assertions)]
      {
        print!("          ");
        for slot in &self.stack[0..self.stack_top] {
          print!("[ ");
          slot.print(self.interner.clone());
          print!(" ]")
        }
        println!("");
        disassemble_instruction(&self.chunk, self.ip);
      }

      match self.read_byte() {
        opcode::ADD => {
          match self.pop() {
            Value::Number(b) => match self.pop() {
              Value::Number(a) => self.push(Value::Number(a + b)),
              _ => return self.runtime_error(&ADD_OPERAND_MISMATCH_ERROR),
            },
            Value::Obj(obj2) => match obj2.value {
              ObjValue::String(b) => match self.pop() {
                Value::Obj(obj1) => match obj1.value {
                  ObjValue::String(a) => {
                    let symbol = {
                      let mut interner = self.interner.borrow_mut();
                      let string_a = interner.resolve(a).unwrap();
                      let string_b = interner.resolve(b).unwrap();
                      let string = format!("{}{}", string_a, string_b);
                      interner.get_or_intern(string)
                    };
                    self.push(Value::Obj(Obj::new(ObjValue::String(symbol))));
                  },
                  // _ => return self.runtime_error(&ADD_OPERAND_MISMATCH_ERROR),
                }
                _ => return self.runtime_error(&ADD_OPERAND_MISMATCH_ERROR),
              },
              // _ => return self.runtime_error(&ADD_OPERAND_MISMATCH_ERROR),
            },
            _ => return self.runtime_error(&ADD_OPERAND_MISMATCH_ERROR),
          }
        },
        opcode::DIVIDE => binary_op!(/,Number),
        opcode::EQUAL => {
          let b = self.pop();
          let a = self.pop();
          self.push(Value::Bool(a == b));
        },
        opcode::GREATER => binary_op!(>,Bool),
        opcode::LESS => binary_op!(<,Bool),
        opcode::MULTIPLY => binary_op!(*,Number),
        opcode::SUBTRACT => binary_op!(-,Number),

        opcode::CONSTANT => {
          let constant = self.read_constant();
          self.push(constant);
        },
        opcode::NIL => self.push(Value::Nil),
        opcode::TRUE => self.push(Value::Bool(true)),
        opcode::FALSE => self.push(Value::Bool(false)),

        opcode::NEGATE => match self.pop() {
          Value::Number(num) => self.push(Value::Number(-num)),
          _ => return self.runtime_error("Operand must be a number."),
        },
        opcode::NOT => {
          let value = self.pop().is_falsey();
          self.push(Value::Bool(value));
        },

        opcode::POP => {
          self.pop();
        },
        opcode::DEFINE_GLOBAL => {
          let value = self.read_constant();
          let symbol = value.as_obj().get_symbol();
          self.globals.insert(symbol, self.peek(0).clone());
          self.pop();
        },
        opcode::PRINT => {
          &self.pop().print(self.interner.clone());
          println!();
        },
        opcode::RETURN => break,
        _ => panic!("Expected opcode"),
      }
    }

    InterpretResult::Success
  }
}
