"""Z80 CPU core tailored for the TI-86.

The implementation focuses on clarity and debuggability over raw speed.  It implements
an extensible subset of the Z80 instruction set that is sufficient for the automated
unit tests in this repository and provides hooks for adding further opcodes as the
emulator matures.
"""

from __future__ import annotations

# ruff: noqa: E741

from dataclasses import dataclass
from typing import Callable, Dict

from .memory import Memory


FLAG_SIGN = 0x80
FLAG_ZERO = 0x40
FLAG_HALF_CARRY = 0x10
FLAG_PARITY = 0x04
FLAG_SUBTRACT = 0x02
FLAG_CARRY = 0x01


@dataclass
class ExecutionTrace:
    """Single instruction execution trace.

    The trace captures enough information to drive a lightweight debugger or to
    drive unit tests that assert on internal behaviour.
    """

    pc: int
    opcode: int
    mnemonic: str
    cycles: int


@dataclass
class Registers:
    """Z80 register file."""

    a: int = 0
    f: int = 0
    b: int = 0
    c: int = 0
    d: int = 0
    e: int = 0
    h: int = 0
    l: int = 0
    sp: int = 0xFFFE
    pc: int = 0
    ix: int = 0
    iy: int = 0

    @property
    def bc(self) -> int:
        return (self.b << 8) | self.c

    @bc.setter
    def bc(self, value: int) -> None:
        self.b = (value >> 8) & 0xFF
        self.c = value & 0xFF

    @property
    def de(self) -> int:
        return (self.d << 8) | self.e

    @de.setter
    def de(self, value: int) -> None:
        self.d = (value >> 8) & 0xFF
        self.e = value & 0xFF

    @property
    def hl(self) -> int:
        return (self.h << 8) | self.l

    @hl.setter
    def hl(self, value: int) -> None:
        self.h = (value >> 8) & 0xFF
        self.l = value & 0xFF

    @property
    def af(self) -> int:
        return (self.a << 8) | self.f

    @af.setter
    def af(self, value: int) -> None:
        self.a = (value >> 8) & 0xFF
        self.f = value & 0xFF

    def snapshot(self) -> Dict[str, int]:
        return {
            "a": self.a,
            "f": self.f,
            "b": self.b,
            "c": self.c,
            "d": self.d,
            "e": self.e,
            "h": self.h,
            "l": self.l,
            "sp": self.sp,
            "pc": self.pc,
            "ix": self.ix,
            "iy": self.iy,
            "bc": self.bc,
            "de": self.de,
            "hl": self.hl,
            "af": self.af,
        }


class Z80CPU:
    """A pragmatic Z80 CPU core.

    The class exposes helpers such as :meth:`step` and :meth:`set_breakpoint` so the
    rest of the emulator can offer stepping and debugging experiences without needing
    to re-implement CPU internals.
    """

    def __init__(self, memory: Memory) -> None:
        self.memory = memory
        self.reg = Registers()
        self._halted = False
        self.breakpoints: set[int] = set()
        self._decoder: Dict[int, Callable[[int], ExecutionTrace]] = {}
        self._build_decoder()

    # ------------------------------------------------------------------
    # Public API
    # ------------------------------------------------------------------
    @property
    def halted(self) -> bool:
        return self._halted

    def reset(self) -> None:
        self.reg = Registers()
        self._halted = False

    def set_breakpoint(self, address: int) -> None:
        self.breakpoints.add(address & 0xFFFF)

    def clear_breakpoint(self, address: int) -> None:
        self.breakpoints.discard(address & 0xFFFF)

    def clear_breakpoints(self) -> None:
        self.breakpoints.clear()

    def step(self) -> ExecutionTrace:
        """Execute a single instruction and return an :class:`ExecutionTrace`."""

        if self._halted:
            return ExecutionTrace(self.reg.pc, 0x00, "HALT", cycles=4)

        pc = self.reg.pc
        if pc in self.breakpoints:
            raise RuntimeError(f"Execution stopped at breakpoint 0x{pc:04X}")

        opcode = self._fetch_byte()
        handler = self._decoder.get(opcode)
        if handler is None:
            raise NotImplementedError(f"Opcode 0x{opcode:02X} not implemented")
        trace = handler(opcode)
        return trace

    # ------------------------------------------------------------------
    # Decoder construction
    # ------------------------------------------------------------------
    def _build_decoder(self) -> None:
        for opcode in range(0x40, 0x80):
            self._decoder[opcode] = self._op_ld_r_r
        for opcode in range(0x80, 0x88):
            self._decoder[opcode] = self._op_add_a_r
        for opcode in range(0x88, 0x90):
            self._decoder[opcode] = self._op_adc_a_r
        for opcode in range(0x90, 0x98):
            self._decoder[opcode] = self._op_sub_r
        for opcode in range(0x98, 0xA0):
            self._decoder[opcode] = self._op_sbc_a_r
        for opcode in range(0xA0, 0xA8):
            self._decoder[opcode] = self._op_and_r
        for opcode in range(0xA8, 0xB0):
            self._decoder[opcode] = self._op_xor_r
        for opcode in range(0xB0, 0xB8):
            self._decoder[opcode] = self._op_or_r
        for opcode in range(0xB8, 0xC0):
            self._decoder[opcode] = self._op_cp_r

        special_handlers: Dict[int, Callable[[int], ExecutionTrace]] = {
            0x00: self._op_nop,
            0x01: self._op_ld_rr_nn,
            0x02: self._op_ld_rr_indirect_a,
            0x03: self._op_inc_rr,
            0x04: self._op_inc_r,
            0x05: self._op_dec_r,
            0x06: self._op_ld_r_n,
            0x07: self._op_rlca,
            0x09: self._op_add_hl_rr,
            0x0A: self._op_ld_a_rr_indirect,
            0x0B: self._op_dec_rr,
            0x0C: self._op_inc_r,
            0x0D: self._op_dec_r,
            0x0E: self._op_ld_r_n,
            0x0F: self._op_rrca,
            0x11: self._op_ld_rr_nn,
            0x12: self._op_ld_rr_indirect_a,
            0x13: self._op_inc_rr,
            0x14: self._op_inc_r,
            0x15: self._op_dec_r,
            0x16: self._op_ld_r_n,
            0x17: self._op_rla,
            0x18: self._op_jr,
            0x19: self._op_add_hl_rr,
            0x1A: self._op_ld_a_rr_indirect,
            0x1B: self._op_dec_rr,
            0x1C: self._op_inc_r,
            0x1D: self._op_dec_r,
            0x1E: self._op_ld_r_n,
            0x1F: self._op_rra,
            0x20: self._op_jr_cond,
            0x21: self._op_ld_rr_nn,
            0x22: self._op_ld_indirect_nn_rr,
            0x23: self._op_inc_rr,
            0x24: self._op_inc_r,
            0x25: self._op_dec_r,
            0x26: self._op_ld_r_n,
            0x27: self._op_daa,
            0x28: self._op_jr_cond,
            0x29: self._op_add_hl_rr,
            0x2A: self._op_ld_rr_indirect_nn,
            0x2B: self._op_dec_rr,
            0x2C: self._op_inc_r,
            0x2D: self._op_dec_r,
            0x2E: self._op_ld_r_n,
            0x2F: self._op_cpl,
            0x31: self._op_ld_rr_nn,
            0x32: self._op_ld_indirect_nn_a,
            0x33: self._op_inc_rr,
            0x34: self._op_inc_mem_hl,
            0x35: self._op_dec_mem_hl,
            0x36: self._op_ld_mem_hl_n,
            0x37: self._op_scf,
            0x38: self._op_jr_cond,
            0x39: self._op_add_hl_rr,
            0x3A: self._op_ld_a_indirect_nn,
            0x3B: self._op_dec_rr,
            0x3C: self._op_inc_r,
            0x3D: self._op_dec_r,
            0x3E: self._op_ld_r_n,
            0x3F: self._op_ccf,
            0x76: self._op_halt,
            0xC1: self._op_pop_rr,
            0xC3: self._op_jp_nn,
            0xC5: self._op_push_rr,
            0xC6: self._op_add_a_n,
            0xC8: self._op_ret_cond,
            0xC9: self._op_ret,
            0xCA: self._op_jp_cond,
            0xCB: self._op_prefix_cb,
            0xCD: self._op_call_nn,
            0xCE: self._op_adc_a_n,
            0xD1: self._op_pop_rr,
            0xD3: self._op_out,  # TI-86 uses memory mapped IO; keep placeholder.
            0xD5: self._op_push_rr,
            0xD6: self._op_sub_n,
            0xD8: self._op_ret_cond,
            0xDA: self._op_jp_cond,
            0xDE: self._op_sbc_a_n,
            0xE1: self._op_pop_rr,
            0xE5: self._op_push_rr,
            0xE6: self._op_and_n,
            0xE9: self._op_jp_hl,
            0xEA: self._op_ld_indirect_nn_a,
            0xEE: self._op_xor_n,
            0xF1: self._op_pop_rr,
            0xF3: self._op_di,
            0xF5: self._op_push_rr,
            0xF6: self._op_or_n,
            0xF9: self._op_ld_sp_hl,
            0xFA: self._op_ld_a_indirect_nn,
            0xFB: self._op_ei,
            0xFE: self._op_cp_n,
        }
        self._decoder.update(special_handlers)

    # ------------------------------------------------------------------
    # Instruction helpers
    # ------------------------------------------------------------------
    def _fetch_byte(self) -> int:
        value = self.memory.read_byte(self.reg.pc)
        self.reg.pc = (self.reg.pc + 1) & 0xFFFF
        return value

    def _fetch_word(self) -> int:
        low = self._fetch_byte()
        high = self._fetch_byte()
        return (high << 8) | low

    def _read_reg_by_index(self, index: int) -> int:
        table = [
            self.reg.b,
            self.reg.c,
            self.reg.d,
            self.reg.e,
            self.reg.h,
            self.reg.l,
            None,
            self.reg.a,
        ]
        if index == 6:  # (HL)
            address = (self.reg.h << 8) | self.reg.l
            return self.memory.read_byte(address)
        return table[index]

    def _write_reg_by_index(self, index: int, value: int) -> None:
        value &= 0xFF
        if index == 0:
            self.reg.b = value
        elif index == 1:
            self.reg.c = value
        elif index == 2:
            self.reg.d = value
        elif index == 3:
            self.reg.e = value
        elif index == 4:
            self.reg.h = value
        elif index == 5:
            self.reg.l = value
        elif index == 6:
            address = (self.reg.h << 8) | self.reg.l
            self.memory.write_byte(address, value)
        elif index == 7:
            self.reg.a = value
        else:
            raise ValueError(index)

    def _set_reg_pair(self, name: str, value: int) -> None:
        value &= 0xFFFF
        if name == "bc":
            self.reg.bc = value
        elif name == "de":
            self.reg.de = value
        elif name == "hl":
            self.reg.hl = value
        elif name == "sp":
            self.reg.sp = value
        elif name == "af":
            self.reg.af = value
        else:
            raise ValueError(name)

    def _set_flags_szp(self, value: int) -> None:
        value &= 0xFF
        flags = 0
        if value & 0x80:
            flags |= FLAG_SIGN
        if value == 0:
            flags |= FLAG_ZERO
        if bin(value).count("1") % 2 == 0:
            flags |= FLAG_PARITY
        self.reg.f = (self.reg.f & (FLAG_HALF_CARRY | FLAG_CARRY)) | flags

    def _set_flags_add(self, a: int, b: int, carry: int = 0) -> int:
        total = a + b + carry
        result = total & 0xFF
        flags = 0
        if result & 0x80:
            flags |= FLAG_SIGN
        if result == 0:
            flags |= FLAG_ZERO
        if ((a & 0x0F) + (b & 0x0F) + carry) & 0x10:
            flags |= FLAG_HALF_CARRY
        if (~(a ^ b) & (a ^ result) & 0x80) != 0:
            flags |= FLAG_PARITY
        if total > 0xFF:
            flags |= FLAG_CARRY
        self.reg.f = flags
        return result

    def _set_flags_sub(self, a: int, b: int, carry: int = 0) -> int:
        diff = a - b - carry
        result = diff & 0xFF
        flags = FLAG_SUBTRACT
        if result & 0x80:
            flags |= FLAG_SIGN
        if result == 0:
            flags |= FLAG_ZERO
        if ((a & 0x0F) - (b & 0x0F) - carry) & 0x10:
            flags |= FLAG_HALF_CARRY
        if ((a ^ b) & (a ^ result) & 0x80) != 0:
            flags |= FLAG_PARITY
        if diff < 0:
            flags |= FLAG_CARRY
        self.reg.f = flags
        return result

    def _push(self, value: int) -> None:
        self.reg.sp = (self.reg.sp - 1) & 0xFFFF
        self.memory.write_byte(self.reg.sp, (value >> 8) & 0xFF)
        self.reg.sp = (self.reg.sp - 1) & 0xFFFF
        self.memory.write_byte(self.reg.sp, value & 0xFF)

    def _pop(self) -> int:
        low = self.memory.read_byte(self.reg.sp)
        self.reg.sp = (self.reg.sp + 1) & 0xFFFF
        high = self.memory.read_byte(self.reg.sp)
        self.reg.sp = (self.reg.sp + 1) & 0xFFFF
        return (high << 8) | low

    # ------------------------------------------------------------------
    # Opcode implementations
    # ------------------------------------------------------------------
    def _op_nop(self, opcode: int) -> ExecutionTrace:
        return ExecutionTrace(self.reg.pc - 1, opcode, "NOP", cycles=4)

    def _op_ld_r_r(self, opcode: int) -> ExecutionTrace:
        dest = (opcode >> 3) & 0x07
        src = opcode & 0x07
        value = self._read_reg_by_index(src)
        self._write_reg_by_index(dest, value)
        return ExecutionTrace(self.reg.pc - 1, opcode, "LD", cycles=4)

    def _op_ld_r_n(self, opcode: int) -> ExecutionTrace:
        dest = (opcode >> 3) & 0x07
        value = self._fetch_byte()
        self._write_reg_by_index(dest, value)
        return ExecutionTrace(self.reg.pc - 2, opcode, "LD", cycles=7)

    def _op_ld_mem_hl_n(self, opcode: int) -> ExecutionTrace:
        value = self._fetch_byte()
        address = (self.reg.h << 8) | self.reg.l
        self.memory.write_byte(address, value)
        return ExecutionTrace(self.reg.pc - 2, opcode, "LD (HL),n", cycles=10)

    def _op_inc_r(self, opcode: int) -> ExecutionTrace:
        dest = (opcode >> 3) & 0x07
        value = self._read_reg_by_index(dest)
        result = (value + 1) & 0xFF
        half = ((value & 0x0F) + 1) & 0x10
        flags = self.reg.f & FLAG_CARRY
        if result & 0x80:
            flags |= FLAG_SIGN
        if result == 0:
            flags |= FLAG_ZERO
        if half:
            flags |= FLAG_HALF_CARRY
        if result == 0x80:
            flags |= FLAG_PARITY
        self.reg.f = flags
        self._write_reg_by_index(dest, result)
        return ExecutionTrace(self.reg.pc - 1, opcode, "INC", cycles=4)

    def _op_dec_r(self, opcode: int) -> ExecutionTrace:
        dest = (opcode >> 3) & 0x07
        value = self._read_reg_by_index(dest)
        result = (value - 1) & 0xFF
        half = ((value & 0x0F) - 1) & 0x10
        flags = FLAG_SUBTRACT | (self.reg.f & FLAG_CARRY)
        if result & 0x80:
            flags |= FLAG_SIGN
        if result == 0:
            flags |= FLAG_ZERO
        if half:
            flags |= FLAG_HALF_CARRY
        if result == 0x7F:
            flags |= FLAG_PARITY
        self.reg.f = flags
        self._write_reg_by_index(dest, result)
        return ExecutionTrace(self.reg.pc - 1, opcode, "DEC", cycles=4)

    def _op_inc_mem_hl(self, opcode: int) -> ExecutionTrace:
        address = (self.reg.h << 8) | self.reg.l
        value = self.memory.read_byte(address)
        result = (value + 1) & 0xFF
        half = ((value & 0x0F) + 1) & 0x10
        flags = self.reg.f & FLAG_CARRY
        if result & 0x80:
            flags |= FLAG_SIGN
        if result == 0:
            flags |= FLAG_ZERO
        if half:
            flags |= FLAG_HALF_CARRY
        if result == 0x80:
            flags |= FLAG_PARITY
        self.reg.f = flags
        self.memory.write_byte(address, result)
        return ExecutionTrace(self.reg.pc - 1, opcode, "INC (HL)", cycles=11)

    def _op_dec_mem_hl(self, opcode: int) -> ExecutionTrace:
        address = (self.reg.h << 8) | self.reg.l
        value = self.memory.read_byte(address)
        result = (value - 1) & 0xFF
        half = ((value & 0x0F) - 1) & 0x10
        flags = FLAG_SUBTRACT | (self.reg.f & FLAG_CARRY)
        if result & 0x80:
            flags |= FLAG_SIGN
        if result == 0:
            flags |= FLAG_ZERO
        if half:
            flags |= FLAG_HALF_CARRY
        if result == 0x7F:
            flags |= FLAG_PARITY
        self.reg.f = flags
        self.memory.write_byte(address, result)
        return ExecutionTrace(self.reg.pc - 1, opcode, "DEC (HL)", cycles=11)

    def _op_add_a_r(self, opcode: int) -> ExecutionTrace:
        index = opcode & 0x07
        value = self._read_reg_by_index(index)
        self.reg.a = self._set_flags_add(self.reg.a, value)
        return ExecutionTrace(self.reg.pc - 1, opcode, "ADD", cycles=4)

    def _op_adc_a_r(self, opcode: int) -> ExecutionTrace:
        index = opcode & 0x07
        value = self._read_reg_by_index(index)
        carry = 1 if (self.reg.f & FLAG_CARRY) else 0
        self.reg.a = self._set_flags_add(self.reg.a, value, carry)
        return ExecutionTrace(self.reg.pc - 1, opcode, "ADC", cycles=4)

    def _op_sub_r(self, opcode: int) -> ExecutionTrace:
        index = opcode & 0x07
        value = self._read_reg_by_index(index)
        self.reg.a = self._set_flags_sub(self.reg.a, value)
        return ExecutionTrace(self.reg.pc - 1, opcode, "SUB", cycles=4)

    def _op_sbc_a_r(self, opcode: int) -> ExecutionTrace:
        index = opcode & 0x07
        value = self._read_reg_by_index(index)
        carry = 1 if (self.reg.f & FLAG_CARRY) else 0
        self.reg.a = self._set_flags_sub(self.reg.a, value, carry)
        return ExecutionTrace(self.reg.pc - 1, opcode, "SBC", cycles=4)

    def _op_and_r(self, opcode: int) -> ExecutionTrace:
        index = opcode & 0x07
        value = self._read_reg_by_index(index)
        self.reg.a &= value
        self.reg.a &= 0xFF
        self.reg.f = FLAG_HALF_CARRY
        self._set_flags_szp(self.reg.a)
        return ExecutionTrace(self.reg.pc - 1, opcode, "AND", cycles=4)

    def _op_or_r(self, opcode: int) -> ExecutionTrace:
        index = opcode & 0x07
        value = self._read_reg_by_index(index)
        self.reg.a |= value
        self.reg.a &= 0xFF
        self.reg.f = 0
        self._set_flags_szp(self.reg.a)
        return ExecutionTrace(self.reg.pc - 1, opcode, "OR", cycles=4)

    def _op_xor_r(self, opcode: int) -> ExecutionTrace:
        index = opcode & 0x07
        value = self._read_reg_by_index(index)
        self.reg.a ^= value
        self.reg.a &= 0xFF
        self.reg.f = 0
        self._set_flags_szp(self.reg.a)
        return ExecutionTrace(self.reg.pc - 1, opcode, "XOR", cycles=4)

    def _op_cp_r(self, opcode: int) -> ExecutionTrace:
        index = opcode & 0x07
        value = self._read_reg_by_index(index)
        self._set_flags_sub(self.reg.a, value)
        return ExecutionTrace(self.reg.pc - 1, opcode, "CP", cycles=4)

    def _op_add_a_n(self, opcode: int) -> ExecutionTrace:
        value = self._fetch_byte()
        self.reg.a = self._set_flags_add(self.reg.a, value)
        return ExecutionTrace(self.reg.pc - 2, opcode, "ADD n", cycles=7)

    def _op_adc_a_n(self, opcode: int) -> ExecutionTrace:
        value = self._fetch_byte()
        carry = 1 if (self.reg.f & FLAG_CARRY) else 0
        self.reg.a = self._set_flags_add(self.reg.a, value, carry)
        return ExecutionTrace(self.reg.pc - 2, opcode, "ADC n", cycles=7)

    def _op_sub_n(self, opcode: int) -> ExecutionTrace:
        value = self._fetch_byte()
        self.reg.a = self._set_flags_sub(self.reg.a, value)
        return ExecutionTrace(self.reg.pc - 2, opcode, "SUB n", cycles=7)

    def _op_sbc_a_n(self, opcode: int) -> ExecutionTrace:
        value = self._fetch_byte()
        carry = 1 if (self.reg.f & FLAG_CARRY) else 0
        self.reg.a = self._set_flags_sub(self.reg.a, value, carry)
        return ExecutionTrace(self.reg.pc - 2, opcode, "SBC n", cycles=7)

    def _op_and_n(self, opcode: int) -> ExecutionTrace:
        value = self._fetch_byte()
        self.reg.a &= value
        self.reg.a &= 0xFF
        self.reg.f = FLAG_HALF_CARRY
        self._set_flags_szp(self.reg.a)
        return ExecutionTrace(self.reg.pc - 2, opcode, "AND n", cycles=7)

    def _op_or_n(self, opcode: int) -> ExecutionTrace:
        value = self._fetch_byte()
        self.reg.a |= value
        self.reg.a &= 0xFF
        self.reg.f = 0
        self._set_flags_szp(self.reg.a)
        return ExecutionTrace(self.reg.pc - 2, opcode, "OR n", cycles=7)

    def _op_xor_n(self, opcode: int) -> ExecutionTrace:
        value = self._fetch_byte()
        self.reg.a ^= value
        self.reg.a &= 0xFF
        self.reg.f = 0
        self._set_flags_szp(self.reg.a)
        return ExecutionTrace(self.reg.pc - 2, opcode, "XOR n", cycles=7)

    def _op_cp_n(self, opcode: int) -> ExecutionTrace:
        value = self._fetch_byte()
        self._set_flags_sub(self.reg.a, value)
        return ExecutionTrace(self.reg.pc - 2, opcode, "CP n", cycles=7)

    def _op_ld_rr_nn(self, opcode: int) -> ExecutionTrace:
        pair_map = {0x01: "bc", 0x11: "de", 0x21: "hl", 0x31: "sp"}
        pair = pair_map.get(opcode)
        if pair is None:
            raise NotImplementedError(f"Opcode 0x{opcode:02X} not implemented")
        value = self._fetch_word()
        self._set_reg_pair(pair, value)
        return ExecutionTrace(
            self.reg.pc - 3, opcode, f"LD {pair.upper()},nn", cycles=10
        )

    def _op_ld_rr_indirect_a(self, opcode: int) -> ExecutionTrace:
        addr = self._get_indirect_address(opcode)
        self.memory.write_byte(addr, self.reg.a)
        return ExecutionTrace(self.reg.pc - 1, opcode, "LD (rr),A", cycles=7)

    def _op_ld_a_rr_indirect(self, opcode: int) -> ExecutionTrace:
        addr = self._get_indirect_address(opcode)
        self.reg.a = self.memory.read_byte(addr)
        return ExecutionTrace(self.reg.pc - 1, opcode, "LD A,(rr)", cycles=7)

    def _get_indirect_address(self, opcode: int) -> int:
        if opcode in (0x02, 0x0A):
            return (self.reg.b << 8) | self.reg.c
        if opcode in (0x12, 0x1A):
            return (self.reg.d << 8) | self.reg.e
        if opcode in (0x22, 0x2A, 0x32, 0x3A):
            return self._fetch_word()
        raise NotImplementedError(f"Indirect addressing for opcode 0x{opcode:02X}")

    def _op_inc_rr(self, opcode: int) -> ExecutionTrace:
        pair_map = {0x03: "bc", 0x13: "de", 0x23: "hl", 0x33: "sp"}
        pair = pair_map.get(opcode)
        if pair is None:
            raise NotImplementedError(f"Opcode 0x{opcode:02X} not implemented")
        value = (self._get_reg_pair_value(pair) + 1) & 0xFFFF
        self._set_reg_pair(pair, value)
        return ExecutionTrace(self.reg.pc - 1, opcode, f"INC {pair.upper()}", cycles=6)

    def _op_dec_rr(self, opcode: int) -> ExecutionTrace:
        pair_map = {0x0B: "bc", 0x1B: "de", 0x2B: "hl", 0x3B: "sp"}
        pair = pair_map.get(opcode)
        if pair is None:
            raise NotImplementedError(f"Opcode 0x{opcode:02X} not implemented")
        value = (self._get_reg_pair_value(pair) - 1) & 0xFFFF
        self._set_reg_pair(pair, value)
        return ExecutionTrace(self.reg.pc - 1, opcode, f"DEC {pair.upper()}", cycles=6)

    def _get_reg_pair_value(self, name: str) -> int:
        if name == "bc":
            return self.reg.bc
        if name == "de":
            return self.reg.de
        if name == "hl":
            return self.reg.hl
        if name == "sp":
            return self.reg.sp
        raise ValueError(name)

    def _op_add_hl_rr(self, opcode: int) -> ExecutionTrace:
        pair_map = {0x09: "bc", 0x19: "de", 0x29: "hl", 0x39: "sp"}
        src = pair_map.get(opcode)
        if src is None:
            raise NotImplementedError(f"Opcode 0x{opcode:02X} not implemented")
        hl = self._get_reg_pair_value("hl")
        value = self._get_reg_pair_value(src)
        total = hl + value
        result = total & 0xFFFF
        new_flags = self.reg.f & ~(FLAG_SUBTRACT | FLAG_HALF_CARRY | FLAG_CARRY)
        if ((hl & 0x0FFF) + (value & 0x0FFF)) > 0x0FFF:
            new_flags |= FLAG_HALF_CARRY
        if total > 0xFFFF:
            new_flags |= FLAG_CARRY
        self.reg.f = new_flags
        self._set_reg_pair("hl", result)
        return ExecutionTrace(self.reg.pc - 1, opcode, "ADD HL,rr", cycles=11)

    def _op_ld_indirect_nn_rr(self, opcode: int) -> ExecutionTrace:
        address = self._fetch_word()
        value = self._get_reg_pair_value("hl")
        self.memory.write_byte(address, value & 0xFF)
        self.memory.write_byte((address + 1) & 0xFFFF, (value >> 8) & 0xFF)
        return ExecutionTrace(self.reg.pc - 3, opcode, "LD (nn),HL", cycles=16)

    def _op_ld_rr_indirect_nn(self, opcode: int) -> ExecutionTrace:
        address = self._fetch_word()
        low = self.memory.read_byte(address)
        high = self.memory.read_byte((address + 1) & 0xFFFF)
        self._set_reg_pair("hl", (high << 8) | low)
        return ExecutionTrace(self.reg.pc - 3, opcode, "LD HL,(nn)", cycles=16)

    def _op_ld_indirect_nn_a(self, opcode: int) -> ExecutionTrace:
        address = self._fetch_word()
        self.memory.write_byte(address, self.reg.a)
        return ExecutionTrace(self.reg.pc - 3, opcode, "LD (nn),A", cycles=13)

    def _op_ld_a_indirect_nn(self, opcode: int) -> ExecutionTrace:
        address = self._fetch_word()
        self.reg.a = self.memory.read_byte(address)
        return ExecutionTrace(self.reg.pc - 3, opcode, "LD A,(nn)", cycles=13)

    def _op_add_hl_rr_placeholder(self, opcode: int) -> ExecutionTrace:
        raise NotImplementedError("Deprecated placeholder")

    def _op_rlca(self, opcode: int) -> ExecutionTrace:
        carry = (self.reg.a >> 7) & 0x01
        self.reg.a = ((self.reg.a << 1) | carry) & 0xFF
        self.reg.f = carry
        self._set_flags_szp(self.reg.a)
        return ExecutionTrace(self.reg.pc - 1, opcode, "RLCA", cycles=4)

    def _op_rrca(self, opcode: int) -> ExecutionTrace:
        carry = self.reg.a & 0x01
        self.reg.a = ((carry << 7) | (self.reg.a >> 1)) & 0xFF
        self.reg.f = carry
        self._set_flags_szp(self.reg.a)
        return ExecutionTrace(self.reg.pc - 1, opcode, "RRCA", cycles=4)

    def _op_rla(self, opcode: int) -> ExecutionTrace:
        carry = 1 if (self.reg.f & FLAG_CARRY) else 0
        new_carry = (self.reg.a >> 7) & 0x01
        self.reg.a = ((self.reg.a << 1) | carry) & 0xFF
        self.reg.f = new_carry
        self._set_flags_szp(self.reg.a)
        return ExecutionTrace(self.reg.pc - 1, opcode, "RLA", cycles=4)

    def _op_rra(self, opcode: int) -> ExecutionTrace:
        carry = 1 if (self.reg.f & FLAG_CARRY) else 0
        new_carry = self.reg.a & 0x01
        self.reg.a = ((carry << 7) | (self.reg.a >> 1)) & 0xFF
        self.reg.f = new_carry
        self._set_flags_szp(self.reg.a)
        return ExecutionTrace(self.reg.pc - 1, opcode, "RRA", cycles=4)

    def _op_daa(self, opcode: int) -> ExecutionTrace:
        a = self.reg.a
        adjust = 0
        carry = False
        if (self.reg.f & FLAG_HALF_CARRY) or ((a & 0x0F) > 9):
            adjust |= 0x06
        if (self.reg.f & FLAG_CARRY) or (a > 0x99):
            adjust |= 0x60
            carry = True
        if self.reg.f & FLAG_SUBTRACT:
            a = (a - adjust) & 0xFF
        else:
            a = (a + adjust) & 0xFF
        self.reg.a = a
        self.reg.f &= ~(
            FLAG_HALF_CARRY | FLAG_ZERO | FLAG_SIGN | FLAG_PARITY | FLAG_CARRY
        )
        if a == 0:
            self.reg.f |= FLAG_ZERO
        if a & 0x80:
            self.reg.f |= FLAG_SIGN
        if bin(a).count("1") % 2 == 0:
            self.reg.f |= FLAG_PARITY
        if carry:
            self.reg.f |= FLAG_CARRY
        return ExecutionTrace(self.reg.pc - 1, opcode, "DAA", cycles=4)

    def _op_cpl(self, opcode: int) -> ExecutionTrace:
        self.reg.a ^= 0xFF
        self.reg.f |= FLAG_HALF_CARRY | FLAG_SUBTRACT
        return ExecutionTrace(self.reg.pc - 1, opcode, "CPL", cycles=4)

    def _op_scf(self, opcode: int) -> ExecutionTrace:
        self.reg.f = (self.reg.f & ~(FLAG_SUBTRACT | FLAG_HALF_CARRY)) | FLAG_CARRY
        return ExecutionTrace(self.reg.pc - 1, opcode, "SCF", cycles=4)

    def _op_ccf(self, opcode: int) -> ExecutionTrace:
        carry = 1 if (self.reg.f & FLAG_CARRY) else 0
        new_flags = self.reg.f & ~(FLAG_SUBTRACT | FLAG_HALF_CARRY | FLAG_CARRY)
        if carry:
            new_flags |= FLAG_HALF_CARRY
        else:
            new_flags |= FLAG_CARRY
        self.reg.f = new_flags
        return ExecutionTrace(self.reg.pc - 1, opcode, "CCF", cycles=4)

    def _op_jr(self, opcode: int) -> ExecutionTrace:
        offset = self._fetch_byte()
        if offset & 0x80:
            offset -= 0x100
        old_pc = self.reg.pc
        self.reg.pc = (self.reg.pc + offset) & 0xFFFF
        return ExecutionTrace(old_pc - 2, opcode, "JR", cycles=12)

    def _op_jr_cond(self, opcode: int) -> ExecutionTrace:
        conditions = {
            0x20: lambda: not (self.reg.f & FLAG_ZERO),
            0x28: lambda: bool(self.reg.f & FLAG_ZERO),
            0x30: lambda: not (self.reg.f & FLAG_CARRY),
            0x38: lambda: bool(self.reg.f & FLAG_CARRY),
        }
        condition = conditions.get(opcode)
        if condition is None:
            raise NotImplementedError(f"JR condition for opcode 0x{opcode:02X}")
        offset = self._fetch_byte()
        if offset & 0x80:
            offset -= 0x100
        origin = self.reg.pc
        taken = condition()
        if taken:
            self.reg.pc = (self.reg.pc + offset) & 0xFFFF
            mnemonic = "JR taken"
        else:
            mnemonic = "JR not taken"
        return ExecutionTrace(origin - 2, opcode, mnemonic, cycles=12 if taken else 7)

    def _op_jp_nn(self, opcode: int) -> ExecutionTrace:
        address = self._fetch_word()
        self.reg.pc = address
        return ExecutionTrace(self.reg.pc, opcode, "JP", cycles=10)

    def _op_jp_hl(self, opcode: int) -> ExecutionTrace:
        self.reg.pc = (self.reg.h << 8) | self.reg.l
        return ExecutionTrace(self.reg.pc, opcode, "JP (HL)", cycles=4)

    def _op_jp_cond(self, opcode: int) -> ExecutionTrace:
        conditions = {
            0xCA: lambda: bool(self.reg.f & FLAG_ZERO),
            0xDA: lambda: bool(self.reg.f & FLAG_CARRY),
        }
        address = self._fetch_word()
        condition = conditions.get(opcode)
        if condition is None:
            raise NotImplementedError(f"JP condition for opcode 0x{opcode:02X}")
        if condition():
            self.reg.pc = address
            mnemonic = "JP taken"
        else:
            mnemonic = "JP not taken"
        return ExecutionTrace(self.reg.pc, opcode, mnemonic, cycles=10)

    def _op_call_nn(self, opcode: int) -> ExecutionTrace:
        address = self._fetch_word()
        self._push(self.reg.pc)
        self.reg.pc = address
        return ExecutionTrace(self.reg.pc, opcode, "CALL", cycles=17)

    def _op_ret(self, opcode: int) -> ExecutionTrace:
        self.reg.pc = self._pop()
        return ExecutionTrace(self.reg.pc, opcode, "RET", cycles=10)

    def _op_ret_cond(self, opcode: int) -> ExecutionTrace:
        conditions = {
            0xC8: lambda: bool(self.reg.f & FLAG_ZERO),
            0xD8: lambda: bool(self.reg.f & FLAG_CARRY),
        }
        condition = conditions.get(opcode)
        if condition is None:
            raise NotImplementedError(f"RET condition for opcode 0x{opcode:02X}")
        origin = self.reg.pc
        taken = condition()
        if taken:
            self.reg.pc = self._pop()
            mnemonic = "RET taken"
        else:
            mnemonic = "RET not taken"
        return ExecutionTrace(origin - 1, opcode, mnemonic, cycles=11 if taken else 5)

    def _op_push_rr(self, opcode: int) -> ExecutionTrace:
        pair_map = {0xC5: "bc", 0xD5: "de", 0xE5: "hl", 0xF5: "af"}
        pair = pair_map.get(opcode)
        if pair is None:
            raise NotImplementedError(f"PUSH pair for opcode 0x{opcode:02X}")
        if pair == "af":
            value = (self.reg.a << 8) | self.reg.f
        else:
            value = self._get_reg_pair_value(pair)
        self._push(value)
        return ExecutionTrace(
            self.reg.pc - 1, opcode, f"PUSH {pair.upper()}", cycles=11
        )

    def _op_pop_rr(self, opcode: int) -> ExecutionTrace:
        pair_map = {0xC1: "bc", 0xD1: "de", 0xE1: "hl", 0xF1: "af"}
        pair = pair_map.get(opcode)
        if pair is None:
            raise NotImplementedError(f"POP pair for opcode 0x{opcode:02X}")
        value = self._pop()
        if pair == "af":
            self.reg.a = (value >> 8) & 0xFF
            self.reg.f = value & 0xFF
        else:
            self._set_reg_pair(pair, value)
        return ExecutionTrace(self.reg.pc - 1, opcode, f"POP {pair.upper()}", cycles=10)

    def _op_halt(self, opcode: int) -> ExecutionTrace:
        self._halted = True
        self.reg.pc = (self.reg.pc - 1) & 0xFFFF
        return ExecutionTrace(self.reg.pc, opcode, "HALT", cycles=4)

    def _op_out(self, opcode: int) -> ExecutionTrace:
        # TI-86 hardware maps peripherals into memory.  We treat OUT as a NOP so
        # the CPU core remains usable for diagnostics without a peripheral bus.
        port = self._fetch_byte()
        value = self.reg.a
        _ = port, value
        return ExecutionTrace(self.reg.pc - 2, opcode, "OUT (stub)", cycles=11)

    def _op_di(self, opcode: int) -> ExecutionTrace:
        return ExecutionTrace(self.reg.pc - 1, opcode, "DI", cycles=4)

    def _op_ei(self, opcode: int) -> ExecutionTrace:
        return ExecutionTrace(self.reg.pc - 1, opcode, "EI", cycles=4)

    def _op_ld_sp_hl(self, opcode: int) -> ExecutionTrace:
        self.reg.sp = (self.reg.h << 8) | self.reg.l
        return ExecutionTrace(self.reg.pc - 1, opcode, "LD SP,HL", cycles=6)

    def _op_prefix_cb(self, opcode: int) -> ExecutionTrace:
        cb_opcode = self._fetch_byte()
        bit = (cb_opcode >> 3) & 0x07
        target_index = cb_opcode & 0x07
        if cb_opcode >= 0x40 and cb_opcode < 0x80:
            value = self._read_reg_by_index(target_index)
            mask = 1 << bit
            flags = (self.reg.f & FLAG_CARRY) | FLAG_HALF_CARRY
            if (value & mask) == 0:
                flags |= FLAG_ZERO
                flags |= FLAG_PARITY
            if mask & 0x80 and (value & mask):
                flags |= FLAG_SIGN
            self.reg.f = flags
            return ExecutionTrace(self.reg.pc - 2, 0xCB00 | cb_opcode, "BIT", cycles=8)
        raise NotImplementedError(
            f"CB-prefixed opcode 0x{cb_opcode:02X} not implemented"
        )

    # Utility wrappers -------------------------------------------------
    def load_state(self, snapshot: Dict[str, int]) -> None:
        for key, value in snapshot.items():
            if key in {"pc", "sp", "ix", "iy"}:
                setattr(self.reg, key, value & 0xFFFF)
            elif key in {"a", "b", "c", "d", "e", "h", "l", "f"}:
                setattr(self.reg, key, value & 0xFF)
            elif key in {"bc", "de", "hl", "af"}:
                setattr(self.reg, key, value & 0xFFFF)

    def export_state(self) -> Dict[str, int]:
        return self.reg.snapshot()
