#[deriving(Show)]
#[deriving(PartialEq)]
enum FaultType {
  UnalignedMemoryAccess,
  InvalidInstruction,
  Syscall,
}

struct MipsCpu {
  regs: [u32, ..32],
  mem: [u32, ..1024],
  fault: Option<FaultType>,
  pc: u32,
  next_pc: u32,
  hi: u32,
  lo: u32,

}

impl MipsCpu {
  fn read_mem(&mut self, address: u32) -> u32 {
    if (address & 3) != 0 {
      self.fault = Some(UnalignedMemoryAccess);
      return 0;
    }
    self.mem[(address/4) as uint]
  }
  fn set_mem(&mut self, address: u32, val: u32) {
    if (address & 3) != 0 {
      self.fault = Some(UnalignedMemoryAccess);
    } else {
      self.mem[(address/4) as uint] = val;
    }
  }

  fn decode_r_type(instruction: u32) -> (uint, uint, uint, uint) {
    (
      ((instruction >> 21) & 0x1f) as uint,
      ((instruction >> 16) & 0x1f) as uint,
      ((instruction >> 11) & 0x1f) as uint,
      ((instruction >> 6) & 0x1f) as uint
    )
  }
  fn decode_i_type(instruction: u32) -> (uint, uint, u32) {
    (
      ((instruction >> 21) & 0x1f) as uint,
      ((instruction >> 16) & 0x1f) as uint,
      (((instruction & 0xffff) as i16) as i32) as u32
    )
  }
  fn decode_i_type_unsigned(instruction: u32) -> (uint, uint, u32) {
    (
      ((instruction >> 21) & 0x1f) as uint,
      ((instruction >> 16) & 0x1f) as uint,
      instruction & 0xffff
    )
  }
  fn decode_j_type(instruction: u32) -> u32 {
    instruction & 0x03ffffff
  }

  fn advance_pc(&mut self, delta: u32) {
    self.pc = self.next_pc;
    self.next_pc += delta;
  }

  fn exec_sll(&mut self, instruction: u32) {
    let (_, src, dst, amount) = MipsCpu::decode_r_type(instruction);
    self.regs[dst] = self.regs[src] << amount;
    self.advance_pc(4);
  }

  fn exec_srl(&mut self, instruction: u32) {
    let (_, src, dst, amount) = MipsCpu::decode_r_type(instruction);
    self.regs[dst] = self.regs[src] >> amount;
    self.advance_pc(4);
  }

  fn exec_sra(&mut self, instruction: u32) {
    let (_, src, dst, amount) = MipsCpu::decode_r_type(instruction);
    self.regs[dst] = ((self.regs[src] as i32) >> amount) as u32;
    self.advance_pc(4);
  }

  fn exec_sllv(&mut self, instruction: u32) {
    let (amount_reg, src, dst, _) = MipsCpu::decode_r_type(instruction);
    let amount = self.regs[amount_reg];
    self.regs[dst] = if amount < 32 {
        self.regs[src] << (amount as uint)
      } else {
        0
      };
    self.advance_pc(4);
  }

  fn exec_srlv(&mut self, instruction: u32) {
    let (amount_reg, src, dst, _) = MipsCpu::decode_r_type(instruction);
    let amount = self.regs[amount_reg];
    self.regs[dst] = if amount < 32 {
        self.regs[src] >> (amount as uint)
      } else {
        0
      };
    self.advance_pc(4);
  }

  fn exec_jr(&mut self, instruction: u32) {
    let (address_reg, _, _, _) = MipsCpu::decode_r_type(instruction);
    self.pc = self.next_pc;
    self.next_pc = self.regs[address_reg];
  }

  #[allow(unused_variable)]
  fn exec_syscall(&mut self, instruction: u32) {
    self.fault = Some(Syscall);
    self.advance_pc(4);
  }

  fn exec_mfhi(&mut self, instruction: u32) {
    let (_, _, dst, _) = MipsCpu::decode_r_type(instruction);
    self.regs[dst] = self.hi;
    self.advance_pc(4);
  }

  fn exec_mflo(&mut self, instruction: u32) {
    let (_, _, dst, _) = MipsCpu::decode_r_type(instruction);
    self.regs[dst] = self.lo;
    self.advance_pc(4);
  }

  fn exec_mult(&mut self, instruction: u32) {
    let (x_reg, y_reg, _, _) = MipsCpu::decode_r_type(instruction);
    self.lo = self.regs[x_reg] * self.regs[y_reg];
    self.advance_pc(4);
  }

  fn exec_multu(&mut self, instruction: u32) {
    let (x_reg, y_reg, _, _) = MipsCpu::decode_r_type(instruction);
    self.lo = self.regs[x_reg] * self.regs[y_reg];
    self.advance_pc(4);
  }

  fn exec_div(&mut self, instruction: u32) {
    let (x_reg, y_reg, _, _) = MipsCpu::decode_r_type(instruction);
    let x = self.regs[x_reg] as i32;
    let y = self.regs[y_reg] as i32;

    self.lo = (x / y) as u32;
    self.hi = (x % y) as u32;

    self.advance_pc(4);
  }

  fn exec_divu(&mut self, instruction: u32) {
    let (x_reg, y_reg, _, _) = MipsCpu::decode_r_type(instruction);
    let x = self.regs[x_reg];
    let y = self.regs[y_reg];

    self.lo = x / y;
    self.hi = x % y;

    self.advance_pc(4);
  }

  fn exec_add(&mut self, instruction: u32) {
    let (src1, src2, dest, _) = MipsCpu::decode_r_type(instruction);
    self.regs[dest] = self.regs[src1] + self.regs[src2];
    self.advance_pc(4);
  }

  fn exec_addu(&mut self, instruction: u32) {
    let (src1, src2, dest, _) = MipsCpu::decode_r_type(instruction);
    self.regs[dest] = self.regs[src1] + self.regs[src2];
    self.advance_pc(4);
  }

  fn exec_sub(&mut self, instruction: u32) {
    let (src1, src2, dest, _) = MipsCpu::decode_r_type(instruction);
    self.regs[dest] = self.regs[src1] - self.regs[src2];
    self.advance_pc(4);
  }

  fn exec_subu(&mut self, instruction: u32) {
    let (src1, src2, dest, _) = MipsCpu::decode_r_type(instruction);
    self.regs[dest] = self.regs[src1] - self.regs[src2];
    self.advance_pc(4);
  }

  fn exec_and(&mut self, instruction: u32) {
    let (src1, src2, dest, _) = MipsCpu::decode_r_type(instruction);
    self.regs[dest] = self.regs[src1] & self.regs[src2];
    self.advance_pc(4);
  }

  fn exec_or(&mut self, instruction: u32) {
    let (src1, src2, dest, _) = MipsCpu::decode_r_type(instruction);
    self.regs[dest] = self.regs[src1] | self.regs[src2];
    self.advance_pc(4);
  }

  fn exec_xor(&mut self, instruction: u32) {
    let (src1, src2, dest, _) = MipsCpu::decode_r_type(instruction);
    self.regs[dest] = self.regs[src1] ^ self.regs[src2];
    self.advance_pc(4);
  }

  fn exec_nor(&mut self, instruction: u32) {
    let (src1, src2, dest, _) = MipsCpu::decode_r_type(instruction);
    self.regs[dest] = (self.regs[src1] | self.regs[src2]).not();
    self.advance_pc(4);
  }

  fn exec_slt(&mut self, instruction: u32) {
    let (src1, src2, dest, _) = MipsCpu::decode_r_type(instruction);
    let a = self.regs[src1] as i32;
    let b = self.regs[src2] as i32;
    println!("Comparing (<) regs {}(={}) and {} (={}), storing into {}", src1, a, src2, b, dest);
    self.regs[dest] = if a < b { 1 } else { 0 };
    self.advance_pc(4);
  }

  fn exec_sltu(&mut self, instruction: u32) {
    let (src1, src2, dest, _) = MipsCpu::decode_r_type(instruction);
    let a = self.regs[src1];
    let b = self.regs[src2];
    self.regs[dest] = if a < b { 1 } else { 0 };
    self.advance_pc(4);
  }

  fn exec_bltz(&mut self, instruction: u32) {
    let (cmp_reg, _, offset) = MipsCpu::decode_i_type(instruction);
    if (self.regs[cmp_reg] as i32) < 0 {
      self.advance_pc(offset<<2);
    } else {
      self.advance_pc(4);
    }
  }

  fn exec_bgez(&mut self, instruction: u32) {
    let (cmp_reg, _, offset) = MipsCpu::decode_i_type(instruction);
    if (self.regs[cmp_reg] as i32) >= 0 {
      self.advance_pc(offset<<2);
    } else {
      self.advance_pc(4);
    }
  }

  fn exec_bltzal(&mut self, instruction: u32) {
    let (cmp_reg, _, offset) = MipsCpu::decode_i_type(instruction);
    if (self.regs[cmp_reg] as i32) < 0 {
      self.regs[31] = self.next_pc + 4;
      self.advance_pc(offset<<2);
    } else {
      self.advance_pc(4);
    }
  }

  fn exec_bgezal(&mut self, instruction: u32) {
    let (cmp_reg, _, offset) = MipsCpu::decode_i_type(instruction);
    if (self.regs[cmp_reg] as i32) >= 0 {
      self.regs[31] = self.next_pc + 4;
      self.advance_pc(offset<<2);
    } else {
      self.advance_pc(4);
    }
  }

  fn exec_j(&mut self, instruction: u32) {
    let target = MipsCpu::decode_j_type(instruction);
    self.pc = self.next_pc;
    self.next_pc = (self.pc & 0xf0000000) | (target << 2);
  }

  fn exec_jal(&mut self, instruction: u32) {
    let target = MipsCpu::decode_j_type(instruction);
    self.regs[31] = self.next_pc + 4;
    self.pc = self.next_pc;
    self.next_pc = (self.pc & 0xf0000000) | (target << 2);
  }

  fn exec_beq(&mut self, instruction: u32) {
    let (reg1, reg2, offset) = MipsCpu::decode_i_type(instruction);
    println!("Comparing registers {} and {}", reg1, reg2);
    if self.regs[reg1] == self.regs[reg2] {
      self.advance_pc(offset<<2);
    } else {
      self.advance_pc(4);
    }
  }

  fn exec_bne(&mut self, instruction: u32) {
    let (reg1, reg2, offset) = MipsCpu::decode_i_type(instruction);
    if self.regs[reg1] != self.regs[reg2] {
      self.advance_pc(offset<<2);
    } else {
      self.advance_pc(4);
    }
  }

  fn exec_blez(&mut self, instruction: u32) {
    let (cmp_reg, _, offset) = MipsCpu::decode_i_type(instruction);
    if (self.regs[cmp_reg] as i32) <= 0 {
      self.advance_pc(offset<<2);
    } else {
      self.advance_pc(4);
    }
  }

  fn exec_bgtz(&mut self, instruction: u32) {
    let (cmp_reg, _, offset) = MipsCpu::decode_i_type(instruction);
    if (self.regs[cmp_reg] as i32) > 0 {
      self.advance_pc(offset<<2);
    } else {
      self.advance_pc(4);
    }
  }

  fn exec_addi(&mut self, instruction: u32) {
    let (source, dest, value) = MipsCpu::decode_i_type(instruction);
    self.regs[dest] = self.regs[source] + value;
    self.advance_pc(4);
  }

  fn exec_addiu(&mut self, instruction: u32) {
    let (source, dest, value) = MipsCpu::decode_i_type(instruction);
    self.regs[dest] = self.regs[source] + value;
    self.advance_pc(4);
  }
  
  fn exec_slti(&mut self, instruction: u32) {
    let (compare_reg, output_reg, value) = MipsCpu::decode_i_type(instruction);
    self.regs[output_reg] = if (self.regs[compare_reg] as i32)<(value as i32) { 1 } else { 0 };
    self.advance_pc(4);
  }

  fn exec_sltiu(&mut self, instruction: u32) {
    let (compare_reg, output_reg, value) = MipsCpu::decode_i_type(instruction);
    let value_unsigned = (value & 0xffff) as u32; // TODO: Check if sign extension happens or not
    self.regs[output_reg] = if self.regs[compare_reg] < value_unsigned { 1 } else { 0 };
    self.advance_pc(4);
  }

  fn exec_andi(&mut self, instruction: u32) {
    let (source, dest, value) = MipsCpu::decode_i_type_unsigned(instruction);
    self.regs[dest] = self.regs[source] & value;
    self.advance_pc(4);
  }

  fn exec_ori(&mut self, instruction: u32) {
    let (source, dest, value) = MipsCpu::decode_i_type_unsigned(instruction);
    self.regs[dest] = self.regs[source] | value;
    self.advance_pc(4);
  }

  fn exec_xori(&mut self, instruction: u32) {
    let (source, dest, value) = MipsCpu::decode_i_type_unsigned(instruction);
    self.regs[dest] = self.regs[source] ^ value;
    self.advance_pc(4);
  }
  fn exec_lui(&mut self, instruction: u32) {
    let (_, dest, value) = MipsCpu::decode_i_type(instruction);
    self.regs[dest] = value << 16;
    self.advance_pc(4);
  }
  #[allow(unused_variable)]
  fn exec_lb(&mut self, instruction: u32) {
    self.fault = Some(InvalidInstruction);
    //let (source, dest, value) = MipsCpu::decode_i_type(instruction);
    //let address = self.regs[dest] + value;
    //self.regs[dest] = (self.read_mem[address & ~3] >> (24 - 8*(address&3)) ) & 0xff;
    //self.advance_pc(4);
  }
  fn exec_lw(&mut self, instruction: u32) {
    let (source, dest, value) = MipsCpu::decode_i_type(instruction);
    let address = self.regs[source] + value;
    self.regs[dest] = self.read_mem(address);
    self.advance_pc(4);
  }
  #[allow(unused_variable)]
  fn exec_sb(&mut self, instruction: u32) {
    self.fault = Some(InvalidInstruction);
//    let (source, dest, value) = MipsCpu::decode_i_type(instruction);
 //   let address = self.regs[dest] + value;
 //   self.regs[dest] = (self.read_mem[address & ~3] >> (24 - 8*(address&3)) ) & 0xff;
 //   self.advance_pc(4);
  }
  fn exec_sw(&mut self, instruction: u32) {
    let (source, dest, value) = MipsCpu::decode_i_type(instruction);
    let address = self.regs[dest] + value;
    let val = self.regs[source];
    self.set_mem(address, val);
    self.advance_pc(4);
  }

  
  fn step(&mut self) {
    let instruction_address = self.pc;
    let instruction = self.read_mem(instruction_address);
    let opcode = instruction >> 26;
    println!("Executing instruction {} ({})", instruction, opcode);

    match opcode {
      0x00 => {
        let function_code = instruction & 0x3f;
        match function_code {
          0x00 => self.exec_sll(instruction),
          0x02 => self.exec_srl(instruction),
          0x03 => self.exec_sra(instruction),
          0x04 => self.exec_sllv(instruction),
          0x06 => self.exec_srlv(instruction),
          0x08 => self.exec_jr(instruction),
          0x0c => self.exec_syscall(instruction),
          0x10 => self.exec_mfhi(instruction),
          0x12 => self.exec_mflo(instruction),
          0x18 => self.exec_mult(instruction),
          0x19 => self.exec_multu(instruction),
          0x1a => self.exec_div(instruction),
          0x1b => self.exec_divu(instruction),
          0x20 => self.exec_add(instruction),
          0x21 => self.exec_addu(instruction),
          0x22 => self.exec_sub(instruction),
          0x23 => self.exec_subu(instruction),
          0x24 => self.exec_and(instruction),
          0x25 => self.exec_or(instruction),
          0x26 => self.exec_xor(instruction),
          0x27 => self.exec_nor(instruction),
          0x2a => self.exec_slt(instruction),
          0x2b => self.exec_sltu(instruction),
          _ => self.fault = Some(InvalidInstruction)
        }

      }
      0x01 => {
        let subop = (instruction >> 11) & 0x1f;
        match subop {
          0x00 => self.exec_bltz(instruction),
          0x01 => self.exec_bgez(instruction),
          0x10 => self.exec_bltzal(instruction),
          0x11 => self.exec_bgezal(instruction),
          _ => self.fault = Some(InvalidInstruction)
        }
      }
      0x02 => self.exec_j(instruction),
      0x03 => self.exec_jal(instruction),
      0x04 => self.exec_beq(instruction),
      0x05 => self.exec_bne(instruction),
      0x06 => self.exec_blez(instruction),
      0x07 => self.exec_bgtz(instruction),
      0x08 => self.exec_addi(instruction),
      0x09 => self.exec_addiu(instruction),
      0x0a => self.exec_slti(instruction),
      0x0b => self.exec_sltiu(instruction),
      0x0c => self.exec_andi(instruction),
      0x0d => self.exec_ori(instruction),
      0x0e => self.exec_xori(instruction),
      0x0f => self.exec_lui(instruction),
      0x20 => self.exec_lb(instruction),
      0x23 => self.exec_lw(instruction),
      0x28 => self.exec_sb(instruction),
      0x2b => self.exec_sw(instruction),
      
      _ => self.fault = Some(InvalidInstruction)
    }
  }

  fn run(&mut self, max_steps: uint) -> Option<FaultType> {
    let mut step_count = 0;
    while step_count < max_steps {
      match self.fault {
        Some(f) => return Some(f),
        _ => self.step()
      }
      step_count += 1;
    }
    return None;
  }

}



fn run_int_fn(xs: &[u32], arg: u32) -> Result<u32, FaultType> {
  let mut cpu = MipsCpu {
    regs: [0, ..32],
    mem: [0, ..1024],
    fault: None,
    pc: 0,
    next_pc: 4,
    hi: 0,
    lo: 0,
  };

  // Set up a fake call site which will issue a SYSCALL as soon as the function returns
  let caller_address = 1000;
  cpu.regs[31] = caller_address;
  cpu.regs[4] = arg;
  cpu.set_mem(caller_address, 0x0c); // syscall
  cpu.set_mem(caller_address-4, 0x0c); // guard against stepping normally into the return site

  // Set execution at the beginning of the function under test
  cpu.pc = 32;
  cpu.next_pc = cpu.pc + 4;
  // Copy the function under test into memory
  for (idx, instruction) in xs.iter().enumerate() {
    let address = cpu.pc + (idx*4) as u32;
    cpu.set_mem(address, *instruction);
  }

  // Execute! But finitely. 
  return match cpu.run(5000) {
    None => fail!("Step count exceeded"),
    Some(Syscall) => {
      if cpu.pc == 1004 { // Syscall from the right spot
        Ok(cpu.regs[2]) // Calling convention puts the return int here (v0)
      } else {
        Err(Syscall)
      }
    }
    Some(f) => Err(f)
  }
}

#[test]
fn triangle_sum() {
  assert_eq!(run_int_fn([
    0x00002821, // move    a1,zero
    0x10000002, // b       18 <triangle+0x10>
    0x24030001, // li      v1,1
    0x24630001, // addiu   v1,v1,1
    0x0083102a, // slt     v0,a0,v1
    0x1040fffd, // beqz    v0,14 <triangle+0xc>
    0x00a32821, // addu    a1,a1,v1
    0x00a32823, // subu    a1,a1,v1
    0x03e00008, // jr      ra
    0x00a01021, // move    v0,a1
  ], 4), Ok(10));
}

#[test]
fn load_small_negative() {
  assert_eq!(run_int_fn([
    0x03e00008, // jr      ra
    0x2402fffa, // li      v0,-6
  ], 0), Ok(-6));
}

#[test]
fn load_large() {
  assert_eq!(run_int_fn([
    0x3c021000, // lui     v0,0x1000
    0x03e00008, // jr      ra
    0x3442fff8, // ori     v0,v0,0xfff8
  ], 0), Ok(0x1000fff8));
}

#[test]
fn is_prime() {
  let code = [
    0x10000007, // b       64 <is_prime+0x20>
    0x24030002, // li      v1,2
    0x14600002, // bnez    v1,58 <is_prime+0x14>
    0x0083001a, // div     zero,a0,v1
    0x0007000d, // break   0x7
    0x00001010, // mfhi    v0
    0x10400006, // beqz    v0,78 <is_prime+0x34>
    0x24630001, // addiu   v1,v1,1
    0x00630018, // mult    v1,v1
    0x00001012, // mflo    v0
    0x0082102a, // slt     v0,a0,v0
    0x1040fff6, // beqz    v0,4c <is_prime+0x8>
    0x24020001, // li      v0,1
    0x03e00008, // jr      ra
    0x00000000, // nop
  ];
  assert_eq!(run_int_fn(code, 5), Ok(1));
  assert_eq!(run_int_fn(code, 9), Ok(0));
  assert_eq!(run_int_fn(code, 29), Ok(1));
}

#[test]
fn chacha_round() {
  assert_eq!(run_int_fn([
    0x3c075678, // lui     a3,0x5678
    0x3c021020, // lui     v0,0x1020
    0x34e79abc, // ori     a3,a3,0x9abc
    0x34423040, // ori     v0,v0,0x3040
    0x3c03f0f0, // lui     v1,0xf0f0
    0x00873826, // xor     a3,a0,a3
    0x00821026, // xor     v0,a0,v0
    0x3463f0f0, // ori     v1,v1,0xf0f0
    0x00e21021, // addu    v0,a3,v0
    0x00831826, // xor     v1,a0,v1
    0x00621826, // xor     v1,v1,v0
    0x00032c00, // sll     a1,v1,0x10
    0x00031c02, // srl     v1,v1,0x10
    0x00a32825, // or      a1,a1,v1
    0x00042027, // nor     a0,zero,a0
    0x00a42021, // addu    a0,a1,a0
    0x00873826, // xor     a3,a0,a3
    0x00073300, // sll     a2,a3,0xc
    0x00073d02, // srl     a3,a3,0x14
    0x00c73025, // or      a2,a2,a3
    0x00c21021, // addu    v0,a2,v0
    0x00452826, // xor     a1,v0,a1
    0x00051a00, // sll     v1,a1,0x8
    0x00052e02, // srl     a1,a1,0x18
    0x00651825, // or      v1,v1,a1
    0x00642021, // addu    a0,v1,a0
    0x00863026, // xor     a2,a0,a2
    0x00621026, // xor     v0,v1,v0
    0x000631c0, // sll     a2,a2,0x7
    0x00031e42, // srl     v1,v1,0x19
    0x00441026, // xor     v0,v0,a0
    0x00c33025, // or      a2,a2,v1
    0x03e00008, // jr      ra
    0x00461026, // xor     v0,v0,a2
  ], 40), Ok(437050147));
}

