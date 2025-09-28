"""MIDI playback and editing helpers for the Practical MIDI Player + Editor challenge."""

from __future__ import annotations

import argparse
import contextlib
import sys
import threading
import time
from dataclasses import dataclass
from pathlib import Path
from typing import Callable, Dict, List, Optional, Sequence, Tuple

import mido
from mido import Message, MetaMessage, MidiFile, MidiTrack


def _ensure_track_index(midi: MidiFile, track_index: int) -> None:
    if track_index < 0 or track_index >= len(midi.tracks):
        raise IndexError(
            f"Track index {track_index} out of range (0..{len(midi.tracks) - 1})"
        )


def _track_to_absolute(track: MidiTrack) -> List[Tuple[int, mido.Message]]:
    """Return a list of (absolute_tick, message) pairs for the given track."""
    absolute_events: List[Tuple[int, mido.Message]] = []
    current = 0
    for message in track:
        current += message.time
        absolute_events.append((current, message.copy()))
    return absolute_events


def _absolute_to_track(events: Sequence[Tuple[int, mido.Message]]) -> MidiTrack:
    """Convert absolute tick events back into a MidiTrack with delta times."""
    sorted_events = sorted(events, key=lambda item: item[0])
    new_track = MidiTrack()
    last_tick = 0
    for abs_tick, message in sorted_events:
        msg_copy = message.copy()
        delta = abs_tick - last_tick
        if delta < 0:
            raise ValueError("Events must be sorted by time before conversion")
        msg_copy.time = delta
        last_tick = abs_tick
        new_track.append(msg_copy)
    return new_track


@dataclass
class ScheduledEvent:
    """An event in wall-clock seconds prepared for playback."""

    time: float
    track_index: int
    message: mido.Message


class MIDIProject:
    """Encapsulates editing helpers for a MIDI file."""

    def __init__(self, path: Path | str):
        self.path = Path(path)
        self.midi = MidiFile(self.path)
        self.muted_tracks: set[int] = set()
        self._events: List[ScheduledEvent] = []
        self._rebuild_schedule()

    # ------------------------------------------------------------------
    # Public editing operations
    # ------------------------------------------------------------------
    def mute_track(self, track_index: int, muted: bool = True) -> None:
        """Toggle mute state for a track."""
        _ensure_track_index(self.midi, track_index)
        if muted:
            self.muted_tracks.add(track_index)
        else:
            self.muted_tracks.discard(track_index)

    def change_tempo(self, scale: float) -> None:
        """Scale all tempo meta messages by ``scale`` (values > 1 slow down)."""
        if scale <= 0:
            raise ValueError("Tempo scale must be positive")
        found = False
        for track in self.midi.tracks:
            for message in track:
                if message.type == "set_tempo":
                    new_tempo = max(1, int(message.tempo * scale))
                    message.tempo = new_tempo
                    found = True
        if not found:
            tempo = max(1, int(mido.bpm2tempo(120) * scale))
            tempo_message = MetaMessage("set_tempo", tempo=tempo, time=0)
            track0 = self.midi.tracks[0]
            events = _track_to_absolute(track0)
            events.append((0, tempo_message))
            self.midi.tracks[0] = _absolute_to_track(events)
        self._rebuild_schedule()

    def insert_note(
        self,
        track_index: int,
        note: int,
        tick: int,
        duration: int,
        *,
        velocity: int = 64,
        channel: int = 0,
    ) -> None:
        """Insert a note into the given track at an absolute tick position."""
        if duration <= 0:
            raise ValueError("Note duration must be positive")
        if tick < 0:
            raise ValueError("Tick must be non-negative")
        _ensure_track_index(self.midi, track_index)
        events = _track_to_absolute(self.midi.tracks[track_index])
        on_message = Message(
            "note_on", note=note, velocity=velocity, channel=channel, time=0
        )
        off_message = Message(
            "note_off", note=note, velocity=0, channel=channel, time=0
        )
        events.append((tick, on_message))
        events.append((tick + duration, off_message))
        self.midi.tracks[track_index] = _absolute_to_track(events)
        self._rebuild_schedule()

    def remove_note(
        self,
        track_index: int,
        note: int,
        *,
        start_tick: int = 0,
        channel: Optional[int] = None,
    ) -> None:
        """Remove the first note-on/off pair matching ``note`` occurring after ``start_tick``."""
        _ensure_track_index(self.midi, track_index)
        events = _track_to_absolute(self.midi.tracks[track_index])
        on_index: Optional[int] = None
        off_index: Optional[int] = None
        matched_channel: Optional[int] = None
        for idx, (abs_tick, message) in enumerate(events):
            if abs_tick < start_tick:
                continue
            if message.is_meta:
                continue
            if (
                message.type == "note_on"
                and message.velocity > 0
                and message.note == note
            ):
                if channel is not None and message.channel != channel:
                    continue
                on_index = idx
                matched_channel = message.channel
                break
        if on_index is None:
            raise ValueError("No matching note_on message found for removal")
        for idx in range(on_index + 1, len(events)):
            abs_tick, message = events[idx]
            if message.is_meta:
                continue
            if message.note == note:
                if channel is not None and message.channel != channel:
                    continue
                if matched_channel is not None and message.channel != matched_channel:
                    continue
                if message.type == "note_off" or (
                    message.type == "note_on" and message.velocity == 0
                ):
                    off_index = idx
                    break
        if off_index is None:
            raise ValueError("Matching note_off message not found for removal")
        # Delete in reverse order to keep indices valid
        for idx in sorted((on_index, off_index), reverse=True):
            events.pop(idx)
        self.midi.tracks[track_index] = _absolute_to_track(events)
        self._rebuild_schedule()

    def export(self, output_path: Path | str) -> None:
        """Render the current project (respecting mutes) to ``output_path``."""
        output = MidiFile()
        output.type = self.midi.type
        output.ticks_per_beat = self.midi.ticks_per_beat
        for idx, track in enumerate(self.midi.tracks):
            new_track = MidiTrack()
            for message in track:
                msg_copy = message.copy()
                if idx in self.muted_tracks and not msg_copy.is_meta:
                    if msg_copy.type == "note_on" and msg_copy.velocity > 0:
                        msg_copy.velocity = 0
                new_track.append(msg_copy)
            output.tracks.append(new_track)
        output.save(output_path)

    # ------------------------------------------------------------------
    # Informational helpers
    # ------------------------------------------------------------------
    def track_summaries(self) -> List[str]:
        summaries = []
        for idx, track in enumerate(self.midi.tracks):
            note_count = sum(
                1 for msg in track if msg.type == "note_on" and msg.velocity > 0
            )
            summaries.append(
                f"Track {idx}: {len(track)} events, {note_count} note-on messages"
            )
        return summaries

    def tempo_map(self) -> List[int]:
        tempos: List[int] = []
        for track in self.midi.tracks:
            for message in track:
                if message.type == "set_tempo":
                    tempos.append(message.tempo)
        return tempos

    # ------------------------------------------------------------------
    # Playback schedule
    # ------------------------------------------------------------------
    @property
    def events(self) -> List[ScheduledEvent]:
        return self._events

    def _rebuild_schedule(self) -> None:
        events: List[Tuple[int, int, mido.Message]] = []
        for track_index, track in enumerate(self.midi.tracks):
            abs_tick = 0
            for message in track:
                abs_tick += message.time
                events.append((abs_tick, track_index, message.copy()))
        events.sort(key=lambda item: (item[0], item[1]))
        tempo = 500000  # default microseconds per beat
        last_tick = 0
        current_time = 0.0
        scheduled: List[ScheduledEvent] = []
        for abs_tick, track_index, message in events:
            delta_tick = abs_tick - last_tick
            current_time += mido.tick2second(
                delta_tick, self.midi.ticks_per_beat, tempo
            )
            scheduled.append(
                ScheduledEvent(
                    time=current_time, track_index=track_index, message=message
                )
            )
            if message.type == "set_tempo":
                tempo = message.tempo
            last_tick = abs_tick
        self._events = scheduled


class SilentPort:
    """Fallback MIDI output that swallows messages when no device is available."""

    def send(self, message: mido.Message) -> None:  # pragma: no cover - trivial
        return

    def close(self) -> None:  # pragma: no cover - trivial
        return


class MIDIPlayer:
    """Minimal threaded MIDI player supporting play/pause/seek."""

    def __init__(
        self,
        project: MIDIProject,
        *,
        output_name: Optional[str] = None,
        output_factory: Optional[Callable[[], mido.ports.BaseOutput]] = None,
    ) -> None:
        self.project = project
        self.output_name = output_name
        self._output_factory = output_factory or self._default_output_factory
        self._position = 0
        self._play_thread: Optional[threading.Thread] = None
        self._stop_flag = threading.Event()
        self._lock = threading.Lock()

    # ------------------------------------------------------------------
    def _default_output_factory(self) -> mido.ports.BaseOutput:
        try:
            if self.output_name:
                return mido.open_output(self.output_name)
            return mido.open_output()
        except (IOError, OSError):
            # Fallback to a silent port if no hardware/virtual port exists.
            return SilentPort()

    def play(self) -> None:
        with self._lock:
            if self._play_thread and self._play_thread.is_alive():
                return
            if self._position >= len(self.project.events):
                self._position = 0
            self._stop_flag.clear()
            self._play_thread = threading.Thread(target=self._run_playback, daemon=True)
            self._play_thread.start()

    def pause(self) -> None:
        with self._lock:
            if not self._play_thread:
                return
            self._stop_flag.set()
            self._play_thread.join()
            self._play_thread = None
            self._stop_flag.clear()

    def stop(self) -> None:
        with self._lock:
            self.pause()
            self._position = 0

    def seek(self, seconds: float) -> None:
        if seconds < 0:
            raise ValueError("Seek time must be non-negative")
        with self._lock:
            events = self.project.events
            self._position = 0
            for idx, event in enumerate(events):
                if event.time >= seconds:
                    self._position = idx
                    break
            else:
                self._position = len(events)
            restart = self._play_thread is not None and self._play_thread.is_alive()
        if restart:
            self.pause()
            self.play()

    def wait_until_finished(self, timeout: Optional[float] = None) -> bool:
        thread = None
        with self._lock:
            thread = self._play_thread
        if not thread:
            return True
        thread.join(timeout)
        finished = not thread.is_alive()
        if finished:
            with self._lock:
                self._play_thread = None
        return finished

    # ------------------------------------------------------------------
    def _run_playback(self) -> None:
        output = self._output_factory()
        try:
            events = self.project.events
            if self._position >= len(events):
                return
            last_time = events[self._position - 1].time if self._position > 0 else 0.0
            idx = self._position
            while idx < len(events) and not self._stop_flag.is_set():
                event = events[idx]
                wait_time = event.time - last_time
                if wait_time > 0:
                    elapsed = 0.0
                    while elapsed < wait_time and not self._stop_flag.is_set():
                        slice_ = min(0.05, wait_time - elapsed)
                        time.sleep(slice_)
                        elapsed += slice_
                if self._stop_flag.is_set():
                    break
                if (
                    event.track_index not in self.project.muted_tracks
                    and not event.message.is_meta
                ):
                    output.send(event.message)
                last_time = event.time
                idx += 1
                self._position = idx
        finally:
            with contextlib.suppress(Exception):
                output.close()
            with self._lock:
                if self._position >= len(self.project.events):
                    self._play_thread = None


def _build_command_parsers() -> Dict[str, argparse.ArgumentParser]:
    parsers: Dict[str, argparse.ArgumentParser] = {}

    mute_parser = argparse.ArgumentParser(add_help=False, prog="mute")
    mute_parser.add_argument("track", type=int)
    parsers["mute"] = mute_parser

    unmute_parser = argparse.ArgumentParser(add_help=False, prog="unmute")
    unmute_parser.add_argument("track", type=int)
    parsers["unmute"] = unmute_parser

    tempo_parser = argparse.ArgumentParser(add_help=False, prog="tempo")
    tempo_parser.add_argument(
        "--scale", type=float, required=True, help="Multiply tempo by this factor"
    )
    parsers["tempo"] = tempo_parser

    insert_parser = argparse.ArgumentParser(add_help=False, prog="insert-note")
    insert_parser.add_argument("track", type=int)
    insert_parser.add_argument("--note", type=int, required=True)
    insert_parser.add_argument("--tick", type=int, required=True)
    insert_parser.add_argument("--duration", type=int, required=True)
    insert_parser.add_argument("--velocity", type=int, default=64)
    insert_parser.add_argument("--channel", type=int, default=0)
    parsers["insert-note"] = insert_parser

    remove_parser = argparse.ArgumentParser(add_help=False, prog="remove-note")
    remove_parser.add_argument("track", type=int)
    remove_parser.add_argument("--note", type=int, required=True)
    remove_parser.add_argument("--start", type=int, default=0)
    remove_parser.add_argument("--channel", type=int)
    parsers["remove-note"] = remove_parser

    export_parser = argparse.ArgumentParser(add_help=False, prog="export")
    export_parser.add_argument("output", type=str)
    parsers["export"] = export_parser

    info_parser = argparse.ArgumentParser(add_help=False, prog="info")
    parsers["info"] = info_parser

    seek_parser = argparse.ArgumentParser(add_help=False, prog="seek")
    seek_parser.add_argument("seconds", type=float)
    parsers["seek"] = seek_parser

    play_parser = argparse.ArgumentParser(add_help=False, prog="play")
    play_parser.add_argument("--duration", type=float, help="Stop after N seconds")
    play_parser.add_argument(
        "--output", type=str, help="Explicit MIDI output port name"
    )
    parsers["play"] = play_parser

    return parsers


def _parse_command_sequence(
    raw_args: Sequence[str],
) -> List[Tuple[str, argparse.Namespace]]:
    parsers = _build_command_parsers()
    commands: List[Tuple[str, argparse.Namespace]] = []
    idx = 0
    while idx < len(raw_args):
        name = raw_args[idx]
        if name not in parsers:
            raise SystemExit(f"Unknown command: {name}")
        parser = parsers[name]
        idx += 1
        parsed, remaining = parser.parse_known_args(raw_args[idx:])
        consumed = len(raw_args[idx:]) - len(remaining)
        idx += consumed
        commands.append((name, parsed))
    return commands


def main(argv: Optional[Sequence[str]] = None) -> None:
    argv = argv if argv is not None else sys.argv[1:]
    parser = argparse.ArgumentParser(description="MIDI Player + Editor CLI")
    parser.add_argument("midi_path", type=str)
    parser.add_argument("commands", nargs=argparse.REMAINDER)
    parsed = parser.parse_args(argv)
    if not parsed.commands:
        parser.error("At least one command is required")

    project = MIDIProject(parsed.midi_path)
    command_sequence = _parse_command_sequence(parsed.commands)
    player: Optional[MIDIPlayer] = None

    for command, cmd_args in command_sequence:
        if command == "mute":
            project.mute_track(cmd_args.track, True)
        elif command == "unmute":
            project.mute_track(cmd_args.track, False)
        elif command == "tempo":
            project.change_tempo(cmd_args.scale)
        elif command == "insert-note":
            project.insert_note(
                cmd_args.track,
                note=cmd_args.note,
                tick=cmd_args.tick,
                duration=cmd_args.duration,
                velocity=cmd_args.velocity,
                channel=cmd_args.channel,
            )
        elif command == "remove-note":
            project.remove_note(
                cmd_args.track,
                note=cmd_args.note,
                start_tick=cmd_args.start,
                channel=cmd_args.channel,
            )
        elif command == "export":
            project.export(cmd_args.output)
        elif command == "info":
            for summary in project.track_summaries():
                print(summary)
            tempos = project.tempo_map()
            if tempos:
                tempo_bpm = ", ".join(f"{mido.tempo2bpm(t):.2f} bpm" for t in tempos)
                print(f"Tempo markers: {tempo_bpm}")
            else:
                print("Tempo markers: default 120 bpm")
        elif command == "seek":
            if player is None:
                player = MIDIPlayer(project)
            player.seek(cmd_args.seconds)
        elif command == "play":
            if player is None:
                player = MIDIPlayer(project, output_name=cmd_args.output)
            elif cmd_args.output:
                player.output_name = cmd_args.output
            player.play()
            player.wait_until_finished(timeout=cmd_args.duration)
            if cmd_args.duration is not None:
                player.pause()
        else:  # pragma: no cover - defensive
            raise SystemExit(f"Unhandled command: {command}")


if __name__ == "__main__":  # pragma: no cover
    main()
