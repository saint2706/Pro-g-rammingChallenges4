"""Interactive CLI for the double pendulum simulation."""

from pathlib import Path
import sys

if __package__ in {None, ""}:  # Allow running from a checkout without installing the package.
    repo_root = Path(__file__).resolve().parents[2]
    sys.path.insert(0, str(repo_root / "src"))

from pro_g_rammingchallenges4.emulation.double_pendulum import _cli


if __name__ == "__main__":  # pragma: no cover
    _cli()
