"""Interactive CLI for the double pendulum simulation.

This script serves as the command-line entry point for running the double
pendulum simulation. It imports the core simulation logic from the main
package and exposes it through a command-line interface.
"""

from pathlib import Path
import sys

# Allow running the script from a checkout without installing the package.
if __package__ in {None, ""}:
    repo_root = Path(__file__).resolve().parents[2]
    sys.path.insert(0, str(repo_root / "src"))

from pro_g_rammingchallenges4.emulation.double_pendulum import _cli


if __name__ == "__main__":
    _cli()
