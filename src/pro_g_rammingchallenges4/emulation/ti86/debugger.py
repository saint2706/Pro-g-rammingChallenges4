"""Minimalist single-step debugger helpers for the TI-86 emulator.

This module provides a simple debugger that wraps the Z80 CPU, allowing for
single-stepping and breakpoint management.
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import Callable, List, Optional

from .cpu import ExecutionTrace, Z80CPU


@dataclass
class Breakpoint:
    """Represents a breakpoint at a specific memory address.

    Attributes:
        address: The 16-bit memory address of the breakpoint.
        enabled: A boolean indicating if the breakpoint is currently active.
    """

    address: int
    enabled: bool = True


class Debugger:
    """A simple single-step debugger for the Z80 CPU.

    This class provides a convenient way to control the execution of the CPU,
    allowing for single-stepping and setting breakpoints.
    """

    def __init__(self, cpu: Z80CPU) -> None:
        """Initializes the debugger.

        Args:
            cpu: The Z80CPU instance to debug.
        """
        self.cpu = cpu
        self.breakpoints: List[Breakpoint] = []
        self.on_step: Optional[Callable[[ExecutionTrace], None]] = None

    def add_breakpoint(self, address: int) -> None:
        """Adds a breakpoint at a specified address.

        Args:
            address: The 16-bit memory address to set the breakpoint at.
        """
        bp = Breakpoint(address & 0xFFFF)
        self.breakpoints.append(bp)
        self.cpu.set_breakpoint(bp.address)

    def remove_breakpoint(self, address: int) -> None:
        """Removes a breakpoint from a specified address.

        Args:
            address: The 16-bit memory address of the breakpoint to remove.
        """
        address &= 0xFFFF
        self.breakpoints = [bp for bp in self.breakpoints if bp.address != address]
        self.cpu.clear_breakpoint(address)

    def clear(self) -> None:
        """Removes all breakpoints."""
        self.breakpoints.clear()
        self.cpu.clear_breakpoints()

    def step(self) -> ExecutionTrace:
        """Executes a single instruction.

        If an on_step callback is registered, it will be called with the
        execution trace of the executed instruction.

        Returns:
            The execution trace of the executed instruction.
        """
        trace = self.cpu.step()
        if self.on_step:
            self.on_step(trace)
        return trace
