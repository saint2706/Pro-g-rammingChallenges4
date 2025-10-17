from __future__ import annotations

import importlib.util
import sys
from pathlib import Path

import mido
import pytest

MODULE_PATH = Path(__file__).resolve().parents[1] / "midi_tool.py"
SPEC = importlib.util.spec_from_file_location("midi_tool", MODULE_PATH)
assert SPEC and SPEC.loader
midi_tool = importlib.util.module_from_spec(SPEC)
sys.modules.setdefault("midi_tool", midi_tool)
SPEC.loader.exec_module(midi_tool)

MIDIProject = midi_tool.MIDIProject


@pytest.fixture()
def sample_midi(tmp_path: Path) -> Path:
    midi_path = tmp_path / "sample.mid"
    midi = mido.MidiFile(ticks_per_beat=480)
    melody = mido.MidiTrack()
    melody.append(mido.Message("program_change", program=12, time=0))
    melody.append(mido.Message("note_on", note=60, velocity=64, time=0))
    melody.append(mido.Message("note_off", note=60, velocity=64, time=480))
    midi.tracks.append(melody)

    harmony = mido.MidiTrack()
    harmony.append(mido.MetaMessage("set_tempo", tempo=mido.bpm2tempo(120), time=0))
    harmony.append(mido.Message("note_on", note=67, velocity=70, time=0, channel=1))
    harmony.append(mido.Message("note_off", note=67, velocity=0, time=480, channel=1))
    midi.tracks.append(harmony)

    midi.save(midi_path)
    return midi_path


def load_note_on_events(path: Path, track_index: int) -> list[mido.Message]:
    midi = mido.MidiFile(path)
    return [msg for msg in midi.tracks[track_index] if msg.type == "note_on"]


def test_mute_track_export_renders_silence(sample_midi: Path, tmp_path: Path) -> None:
    project = MIDIProject(sample_midi)
    project.mute_track(1)
    output_path = tmp_path / "muted.mid"
    project.export(output_path)

    note_ons = load_note_on_events(output_path, 1)
    assert all(msg.velocity == 0 for msg in note_ons)


def test_change_tempo_updates_meta_messages(sample_midi: Path) -> None:
    project = MIDIProject(sample_midi)
    project.change_tempo(0.5)

    tempos = project.tempo_map()
    assert tempos, "tempo meta messages should exist"
    expected = pytest.approx(mido.bpm2tempo(120) * 0.5, rel=1e-6)
    assert tempos[0] == pytest.approx(expected, rel=1e-6)


def test_insert_note_adds_events(sample_midi: Path, tmp_path: Path) -> None:
    project = MIDIProject(sample_midi)
    project.insert_note(0, note=72, tick=240, duration=120, velocity=90)
    output_path = tmp_path / "inserted.mid"
    project.export(output_path)

    note_ons = load_note_on_events(output_path, 0)
    assert any(msg.note == 72 and msg.velocity == 90 for msg in note_ons)


def test_remove_note_eliminates_first_match(sample_midi: Path, tmp_path: Path) -> None:
    project = MIDIProject(sample_midi)
    project.remove_note(0, note=60)
    output_path = tmp_path / "removed.mid"
    project.export(output_path)

    note_ons = load_note_on_events(output_path, 0)
    assert all(msg.note != 60 or msg.velocity == 0 for msg in note_ons)
