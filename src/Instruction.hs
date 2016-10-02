module Instruction where

import Data.Bits
import Data.Word

data AddrMode = Implied
              | Accumulator
              | Immediate
              | Relative
              | ZeroPage
              | ZeroPageX
              | ZeroPageY
              | Absolute
              | AbsoluteX
              | AbsoluteY
              | Indirect
              | IndexedIndirect
              | IndirectIndexed

data Op = Adc | And | Asl | Bcc | Bcs | Beq | Bit
        | Bmi | Bne | Bpl | Brk | Bvc | Bvs | Clc
        | Cld | Cli | Clv | Cmp | Cpx | Cpy | Dec
        | Dex | Dey | Eor | Inc | Inx | Iny | Jmp
        | Jsr | Lda | Ldx | Ldy | Lsr | Nop | Ora
        | Pha | Php | Pla | Plp | Rol | Ror | Rti
        | Rts | Sbc | Sec | Sed | Sei | Sta | Stx
        | Sty | Tax | Tay | Tsx | Txa | Txs | Tya

data Instruction = Instr Op AddrMode

decode :: Word8 -> Instruction
decode op = case op of
  0x69 -> Instr Adc Immediate
  0x65 -> Instr Adc ZeroPage
  0x75 -> Instr Adc ZeroPageX
  0x6d -> Instr Adc Absolute
  0x7d -> Instr Adc AbsoluteX
  0x79 -> Instr Adc AbsoluteY
  0x61 -> Instr Adc IndexedIndirect
  0x71 -> Instr Adc IndirectIndexed
  0x29 -> Instr And Immediate
  0x25 -> Instr And ZeroPage
  0x35 -> Instr And ZeroPageX
  0x2d -> Instr And Absolute
  0x3d -> Instr And AbsoluteX
  0x39 -> Instr And AbsoluteY
  0x21 -> Instr And IndexedIndirect
  0x31 -> Instr And IndirectIndexed
  0x0a -> Instr Asl Accumulator
  0x06 -> Instr Asl ZeroPage
  0x16 -> Instr Asl ZeroPageX
  0x0e -> Instr Asl Absolute
  0xea -> Instr Nop Implied
  0x00 -> Instr Brk Implied
  _ -> undefined
