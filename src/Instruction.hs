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
  0x1e -> Instr Asl AbsoluteX
  0x90 -> Instr Bcc Relative
  0xb0 -> Instr Bcs Relative
  0xf0 -> Instr Beq Relative
  0x24 -> Instr Bit ZeroPage
  0x2c -> Instr Bit Absolute
  0x30 -> Instr Bmi Relative
  0xd0 -> Instr Bne Relative
  0x10 -> Instr Bpl Relative
  0x00 -> Instr Brk Implied
  0x50 -> Instr Bvc Relative
  0x70 -> Instr Bvs Relative
  0x18 -> Instr Clc Implied
  0xd8 -> Instr Cld Implied
  0x58 -> Instr Cli Implied
  0xb8 -> Instr Clv Implied
  0xc9 -> Instr Cmp Immediate
  0xc5 -> Instr Cmp ZeroPage
  0xd5 -> Instr Cmp ZeroPageX
  0xcd -> Instr Cmp Absolute
  0xc1 -> Instr Cmp IndexedIndirect
  0xd1 -> Instr Cmp IndirectIndexed
  0xe0 -> Instr Cpx Immediate
  0xe4 -> Instr Cpx ZeroPage
  0xec -> Instr Cpx Absolute
  0xc0 -> Instr Cpy Immediate
  0xc4 -> Instr Cpy ZeroPage
  0xcc -> Instr Cpy Absolute
  0xc6 -> Instr Dec ZeroPage
  0xd6 -> Instr Dec ZeroPageX
  0xce -> Instr Dec Absolute
  0xde -> Instr Dec AbsoluteX
  0xca -> Instr Dex Implied
  0x88 -> Instr Dey Implied
  0x49 -> Instr Eor Immediate
  0x45 -> Instr Eor ZeroPage
  0x55 -> Instr Eor ZeroPageX
  0x4d -> Instr Eor Absolute
  0x5d -> Instr Eor AbsoluteX
  0x59 -> Instr Eor AbsoluteY
  0x41 -> Instr Eor IndexedIndirect
  0x51 -> Instr Eor IndirectIndexed
  0xe6 -> Instr Inc ZeroPage
  0xf6 -> Instr Inc ZeroPageX
  0xee -> Instr Inc Absolute
  0xfe -> Instr Inc AbsoluteX
  0xe8 -> Instr Inx Implied
  0xc8 -> Instr Iny Implied
  0x4c -> Instr Jmp Absolute
  0x6c -> Instr Jmp Indirect
  0x20 -> Instr Jsr Absolute
  0xa9 -> Instr Lda Immediate
  0xa5 -> Instr Lda ZeroPage
  0xb5 -> Instr Lda ZeroPageX
  0xad -> Instr Lda Absolute
  0xbd -> Instr Lda AbsoluteX
  0xb9 -> Instr Lda AbsoluteY
  0xa1 -> Instr Lda IndexedIndirect
  0xb1 -> Instr Lda IndirectIndexed
  0xa2 -> Instr Ldx Immediate
  0xa6 -> Instr Ldx ZeroPage
  0xb6 -> Instr Ldx ZeroPageY
  0xae -> Instr Ldx Absolute
  0xbe -> Instr Ldx AbsoluteY
  0xa0 -> Instr Ldy Immediate
  0xa4 -> Instr Ldy ZeroPage
  0xb4 -> Instr Ldy ZeroPageX
  0xac -> Instr Ldy Absolute
  0xbc -> Instr Ldy AbsoluteX
  0x4a -> Instr Lsr Accumulator
  0x46 -> Instr Lsr ZeroPage
  0x56 -> Instr Lsr ZeroPageX
  0x4e -> Instr Lsr Absolute
  0x5e -> Instr Lsr AbsoluteX
  0xea -> Instr Nop Implied
  0x09 -> Instr Ora Immediate
  0x05 -> Instr Ora ZeroPage
  0x15 -> Instr Ora ZeroPageX
  0x0d -> Instr Ora Absolute
  0x1d -> Instr Ora AbsoluteX
  0x19 -> Instr Ora AbsoluteY
  0x01 -> Instr Ora IndexedIndirect
  0x11 -> Instr Ora IndirectIndexed
  0x48 -> Instr Pha Implied
  0x08 -> Instr Php Implied
  0x68 -> Instr Pla Implied
  0x28 -> Instr Plp Implied
  0x2a -> Instr Rol Accumulator
  0x26 -> Instr Rol ZeroPage
  0x36 -> Instr Rol ZeroPageX
  0x2e -> Instr Rol Absolute
  0x3e -> Instr Rol AbsoluteX
  0x6a -> Instr Ror Accumulator
  0x66 -> Instr Ror ZeroPage
  0x76 -> Instr Ror ZeroPageX
  0x6e -> Instr Ror Absolute
  0x7e -> Instr Ror AbsoluteX
  0x40 -> Instr Rti Implied
  0x60 -> Instr Rts Implied
  0xe9 -> Instr Sbc Immediate
  0xe5 -> Instr Sbc ZeroPage
  0xf5 -> Instr Sbc ZeroPageX
  0xed -> Instr Sbc Absolute
  0xfd -> Instr Sbc AbsoluteX
  0xf9 -> Instr Sbc AbsoluteY
  0xe1 -> Instr Sbc IndexedIndirect
  0xf1 -> Instr Sbc IndirectIndexed
  0x38 -> Instr Sec Implied
  0xf8 -> Instr Sed Implied
  0x78 -> Instr Sei Implied
  0x85 -> Instr Sta ZeroPage
  0x95 -> Instr Sta ZeroPageX
  0x8d -> Instr Sta Absolute
  0x9d -> Instr Sta AbsoluteX
  0x99 -> Instr Sta AbsoluteY
  0x81 -> Instr Sta IndexedIndirect
  0x91 -> Instr Sta IndirectIndexed
  0x86 -> Instr Stx ZeroPage
  0x96 -> Instr Stx ZeroPageY
  0x8e -> Instr Stx Absolute
  0x84 -> Instr Sty ZeroPage
  0x94 -> Instr Sty ZeroPageX
  0x8c -> Instr Sty Absolute
  0xaa -> Instr Tax Implied
  0xa8 -> Instr Tay Implied
  0xba -> Instr Tsx Implied
  0x8a -> Instr Txa Implied
  0x9a -> Instr Txs Implied
  0x98 -> Instr Tya Implied
  _ -> undefined
