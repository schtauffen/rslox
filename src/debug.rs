use crate::chunk::{Chunk, Op};

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

  let byte: u8 = *chunk
    .code
    .get(offset)
    .expect("expected instruction");
  let instruction: Op = byte.into();

  match instruction {
    Op::Constant =>
      constant_instruction("CONSTANT", chunk, offset),
    Op::False =>
      simple_instruction("FALSE", offset),
    Op::Nil =>
      simple_instruction("NIL", offset),
    Op::True =>
      simple_instruction("TRUE", offset),

    Op::Add =>
      simple_instruction("ADD", offset),
    Op::Divide =>
      simple_instruction("DIVIDE", offset),
    Op::Equal =>
      simple_instruction("EQUAL", offset),
    Op::Greater =>
      simple_instruction("GREATER", offset),
    Op::Less =>
      simple_instruction("LESS", offset),
    Op::Multiply =>
      simple_instruction("MULTIPLY", offset),
    Op::Subtract =>
      simple_instruction("SUBTRACT", offset),

    Op::Negate =>
      simple_instruction("NEGATE", offset),
    Op::Not =>
      simple_instruction("NOT", offset),

    Op::Jump =>
      jump_instruction("JUMP", 1, chunk, offset),
    Op::JumpIfFalse =>
      jump_instruction("JUMP_IF_FALSE", 1, chunk, offset),
    Op::DefineGlobal =>
      constant_instruction("DEFINE_GLOBAL", chunk, offset),
    Op::SetGlobal =>
      constant_instruction("SET_GLOBAL", chunk, offset),
    Op::GetGlobal =>
      constant_instruction("GET_GLOBAL", chunk, offset),
    Op::SetLocal =>
      byte_instruction("SET_LOCAL", chunk, offset),
    Op::GetLocal =>
      byte_instruction("GET_LOCAL", chunk, offset),
    Op::Pop =>
      simple_instruction("POP", offset),
    Op::Print =>
      simple_instruction("PRINT", offset),
    Op::Return =>
      simple_instruction("RETURN", offset),

    _ => {
      println!("Unknown Op {}", instruction);
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

fn jump_instruction(
  name: &str,
  sign: i8,
  chunk: &Chunk,
  offset: usize
) -> usize {
  let mut jump = (chunk.code[offset + 1] as u16) << 8;
  jump |= chunk.code[offset + 2] as u16;
  let to_offset = if sign < 0 {
    offset + 3 - jump as usize
  } else {
    offset + 3 + jump as usize
  };
  println!("{:16} {:04} -> {}", name, offset, to_offset);
  offset + 3
}
