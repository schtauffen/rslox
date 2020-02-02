use std::{
  cell::{Cell, RefCell},
  collections::HashMap,
  mem::replace,
  ops::Drop,
  rc::Rc,
};
use crate::{
  chunk::Op,
  compiler::Compiler,
  interner::{Symbol, StringInterner},
  memory::free_objects,
  obj::{Fun, FunKind, Obj, ObjValue},
  parser::Parser,
  value::Value,
};

#[cfg(debug_assertions)]
use crate::debug::disassemble_instruction;

const FRAMES_MAX: usize = 64;
const STACK_MAX: usize = (FRAMES_MAX * std::u8::MAX as usize);
const CALL_ONLY_FUNCTIONS_AND_CLASSES_ERROR: &str = "Can only call functions and classes";

struct CallFrame<'a> {
  fun: Rc<Fun<'a>>,
  ip: usize,
  slots: usize,
}

impl<'a> CallFrame<'a> {
  pub fn new(fun: &Rc<Fun<'a>>) -> Self {
    Self {
      fun: fun.clone(),
      ip: 0,
      slots: 0,
    }
  }
}

pub struct Vm<'a> {
  frames: Vec<CallFrame<'a>>,
  frame_count: usize,
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
      frames: vec![],
      frame_count: 0,
      stack_top: 0,
      stack: vec![Value::default(); STACK_MAX],
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

  fn call(&mut self, fun: &Rc<Fun<'a>>, arg_count: usize) -> InterpretResult {
    if arg_count != fun.arity as usize {
      let message = format!("Expected {} arguments but got {}.", fun.arity, arg_count);
      return self.runtime_error(&message)
    }

    if self.frame_count == FRAMES_MAX {
      return self.runtime_error("Stack overflow.")
    }

    let mut frame = CallFrame::new(fun);
    frame.slots = self.stack_top - arg_count - 1;
    self.frames.push(frame);
    self.frame_count += 1;
    InterpretResult::Success
  }

  fn call_value(&mut self, callee: Value<'a>, arg_count: usize) -> InterpretResult {
    match callee {
      Value::Obj(obj) => match &obj.value {
        ObjValue::Fun(fun) => self.call(fun, arg_count),
        _ => return self.runtime_error(CALL_ONLY_FUNCTIONS_AND_CLASSES_ERROR),
      },
      _ => return self.runtime_error(CALL_ONLY_FUNCTIONS_AND_CLASSES_ERROR),
    }
  }

  fn read_byte(&mut self) -> u8 {
    let frame = self.current_frame();
    let byte = frame.fun.chunk.code[frame.ip];
    frame.ip += 1;

    byte
  }

  fn read_short(&mut self) -> u16 {
    let frame = self.current_frame();
    let byte1 = frame.fun.chunk.code[frame.ip];
    let byte2 = frame.fun.chunk.code[frame.ip + 1];
    frame.ip +=2;
    (byte1 as u16) << 8 | byte2 as u16
  }

  fn read_constant(&mut self) -> Value<'a> {
    let index = self.read_byte() as usize;
    self.current_frame().fun.chunk.constants[index].clone()
  }

  fn allocate(&self, value: ObjValue<'a>) -> Obj<'a> {
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

    for frame in self.frames[0..self.frame_count].iter().rev() {
      eprint!("[line {}] in ", &frame.fun.chunk.lines[frame.ip - 1]);
      match &frame.fun.name {
        None => eprintln!("script"),
        Some(name) => eprintln!("{}", name),
      }
    }

    self.reset_stack();

    InterpretResult::RuntimeError
  }

  fn current_frame(&mut self) -> &mut CallFrame<'a> {
    &mut self.frames[self.frame_count - 1]
  }

  pub fn interpret(&mut self, source: String) -> InterpretResult {
    let parser = Rc::new(RefCell::new(Parser::new(source)));
    let allocate = |value: ObjValue<'a>| self.allocate(value);
    let interner = Rc::clone(&self.interner);
    let compiler = Compiler::new(parser.clone(), &allocate, interner, FunKind::Script);

    match compiler.compile() {
      Err(_) => InterpretResult::CompileError,
      Ok(fun) => {
        self.frames = vec![]; // TODO - use filled array instead of frames.push
        self.frame_count = 0;
        let script = Value::Obj(Obj::new(ObjValue::Fun(fun)));
        self.stack[0] = script.clone();
        self.stack_top = 1;
        self.call_value(script, 0);
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
          let string = slot.get_string(self.interner.clone());
          print!("[ {} ]", string);
        }
        println!("");
        let frame = self.current_frame();
        disassemble_instruction(&frame.fun.chunk, frame.ip);
      }

      match self.read_byte().into() {
        Op::Add => {
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
                  _ => return self.runtime_error(&ADD_OPERAND_MISMATCH_ERROR),
                }
                _ => return self.runtime_error(&ADD_OPERAND_MISMATCH_ERROR),
              },
              _ => return self.runtime_error(&ADD_OPERAND_MISMATCH_ERROR),
            },
            _ => return self.runtime_error(&ADD_OPERAND_MISMATCH_ERROR),
          }
        },
        Op::Divide => binary_op!(/,Number),
        Op::Equal => {
          let b = self.pop();
          let a = self.pop();
          self.push(Value::Bool(a == b));
        },
        Op::Greater => binary_op!(>,Bool),
        Op::Less => binary_op!(<,Bool),
        Op::Multiply => binary_op!(*,Number),
        Op::Subtract => binary_op!(-,Number),

        Op::Constant => {
          let constant = self.read_constant();
          self.push(constant);
        },
        Op::Nil => self.push(Value::Nil),
        Op::True => self.push(Value::Bool(true)),
        Op::False => self.push(Value::Bool(false)),

        Op::Negate => match self.pop() {
          Value::Number(num) => self.push(Value::Number(-num)),
          _ => return self.runtime_error("Operand must be a number."),
        },
        Op::Not => {
          let value = self.pop().is_falsey();
          self.push(Value::Bool(value));
        },

        Op::Call => {
          let arg_count: usize = self.read_byte().into();
          let value = self.peek(arg_count).clone();
          match self.call_value(value, arg_count) {
            InterpretResult::Success => (),
            _ => return InterpretResult::RuntimeError,
          }
        },
        Op::Pop => {
          self.pop();
        },
        Op::Jump => {
          let offset: usize = self.read_short().into();
          self.current_frame().ip += offset;
        },
        Op::JumpIfFalse => {
          let offset: usize = self.read_short().into();
          if self.peek(0).is_falsey() {
            self.current_frame().ip += offset;
          }
        },
        Op::DefineGlobal => {
          let value = self.read_constant();
          let symbol = value.as_obj().get_symbol();
          self.globals.insert(symbol, self.peek(0).clone());
          self.pop();
        },
        Op::SetGlobal => {
          let value = self.read_constant();
          let symbol = value.as_obj().get_symbol();
          match self.globals.insert(symbol, self.peek(0).clone()) {
            Some(_) => (),
            None => {
              self.globals.remove(&symbol);
              let name = value.get_string(self.interner.clone());
              let message = format!("Undefined variable '{}'.", name);
              return self.runtime_error(message.as_ref())
            }
          }
        },
        Op::GetGlobal => {
          let value = self.read_constant();
          let symbol = value.as_obj().get_symbol();
          match self.globals.get(&symbol) {
            Some(v) => {
              let result = v.clone();
              self.push(result);
            },
            None => {
              let name = value.get_string(self.interner.clone());
              let message = format!("Undefined variable '{}'.", name);
              return self.runtime_error(message.as_ref())
            }
          }
        },
        Op::SetLocal => {
          let slot: usize = self.read_byte().into();
          let slots = self.current_frame().slots;
          self.stack[1 + slots + slot] = self.peek(0).clone();
        },
        Op::GetLocal => {
          let slot: usize = self.read_byte().into();
          let slots = self.current_frame().slots;
          let value = self.stack[1 + slots + slot].clone();
          self.push(value);
        },
        Op::Loop => {
          let offset: usize = self.read_short().into();
          self.current_frame().ip -= offset;
        },
        Op::Print => {
          let value = self.pop();
          let message = value.get_string(self.interner.clone());
          println!("{}", message);
        },
        Op::Return => {
          let result = self.pop();
          self.frame_count -= 1;

          if self.frame_count == 0 {
            self.pop();
            return InterpretResult::Success
          }

          self.stack_top = self.frames[self.frame_count].slots;
          self.push(result);
        },
        _ => panic!("Expected Op"),
      }
    }
  }
}
