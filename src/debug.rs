use crate::chunk::{Chunk, opcode};

pub fn disassemble_chunk(chunk: &Chunk, name: &str) {
  println!("== {} ==", name);

  let mut offset: usize = 0;

  loop {
    match chunk.code.get(offset) {
      Some(_byte) => {
        offset = disassemble_instruction(chunk, offset)
      }
      _ => { break; }
    }
  }
}

// should be passing iterators around?
pub fn disassemble_instruction(chunk: &Chunk, offset: usize) -> usize {
  print!("{:04} ", offset);

  let line = chunk.lines.get(offset).expect("expected line number");
  if offset > 0 &&
    line ==chunk.lines.get(offset - 1).expect("expected previous line number")
  {
    print!("   | ");
  } else {
    print!("{:4} ", line);
  }

  // TODO remove get/expect since code is defaulted?
  let instruction: u8 = *chunk.code.get(offset).expect("expected instruction");

  match instruction {
    opcode::CONSTANT =>
      constant_instruction("CONSTANT", chunk, offset),
    opcode::FALSE =>
      simple_instruction("FALSE", offset),
    opcode::NIL =>
      simple_instruction("NIL", offset),
    opcode::TRUE =>
      simple_instruction("TRUE", offset),

    opcode::ADD =>
      simple_instruction("ADD", offset),
    opcode::DIVIDE =>
      simple_instruction("DIVIDE", offset),
    opcode::EQUAL =>
      simple_instruction("EQUAL", offset),
    opcode::GREATER =>
      simple_instruction("GREATER", offset),
    opcode::LESS =>
      simple_instruction("LESS", offset),
    opcode::MULTIPLY =>
      simple_instruction("MULTIPLY", offset),
    opcode::SUBTRACT =>
      simple_instruction("SUBTRACT", offset),

    opcode::NEGATE =>
      simple_instruction("NEGATE", offset),
    opcode::NOT =>
      simple_instruction("NOT", offset),

    opcode::DEFINE_GLOBAL =>
      constant_instruction("DEFINE_GLOBAL", chunk, offset),
    opcode::SET_GLOBAL =>
      constant_instruction("SET_GLOBAL", chunk, offset),
    opcode::GET_GLOBAL =>
      constant_instruction("GET_GLOBAL", chunk, offset),
    opcode::SET_LOCAL =>
      byte_instruction("SET_LOCAL", chunk, offset),
    opcode::GET_LOCAL =>
      byte_instruction("GET_LOCAL", chunk, offset),
    opcode::POP =>
      simple_instruction("POP", offset),
    opcode::PRINT =>
      simple_instruction("PRINT", offset),
    opcode::RETURN =>
      simple_instruction("RETURN", offset),

    _ => {
      println!("Unknown opcode {}", instruction);
      offset + 1
    }
  }
}

fn constant_instruction(name: &str, chunk: &Chunk, offset: usize) -> usize {
  let constant: u8 = *chunk
    .code
    .get(offset + 1)
    .expect("expected constant");

  let value = chunk
    .constants
    .get(constant as usize)
    .expect("expected constant value");

  println!("{:16} {:04} '{}'", name, constant, value);
  offset + 2
}

fn simple_instruction(name: &str, offset: usize) -> usize {
  println!("{}", name);
  offset + 1
}

fn byte_instruction(name: &str, chunk: &Chunk, offset: usize) -> usize {
  let slot = chunk.code[offset + 1];
  println!("{:16} {:04}", name, slot);
  offset + 2
}
