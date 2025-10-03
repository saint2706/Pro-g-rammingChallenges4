"""Minimalist single-step debugger helpers."""

from __future__ import annotations

from dataclasses import dataclass
from typing import Callable, List, Optional

from .cpu import ExecutionTrace, Z80CPU


@dataclass
class Breakpoint:
    address: int
    enabled: bool = True


class Debugger:
    """Convenience wrapper used in tests and documentation examples."""

    def __init__(self, cpu: Z80CPU) -> None:
        self.cpu = cpu
        self.breakpoints: List[Breakpoint] = []
        self.on_step: Optional[Callable[[ExecutionTrace], None]] = None

    def add_breakpoint(self, address: int) -> None:
        bp = Breakpoint(address & 0xFFFF)
        self.breakpoints.append(bp)
        self.cpu.set_breakpoint(bp.address)

    def remove_breakpoint(self, address: int) -> None:
        address &= 0xFFFF
        self.breakpoints = [bp for bp in self.breakpoints if bp.address != address]
        self.cpu.clear_breakpoint(address)

    def clear(self) -> None:
        self.breakpoints.clear()
        self.cpu.clear_breakpoints()

    def step(self) -> ExecutionTrace:
        trace = self.cpu.step()
        if self.on_step is not None:
            self.on_step(trace)
        return trace
