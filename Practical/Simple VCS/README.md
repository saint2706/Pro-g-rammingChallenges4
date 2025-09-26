# Simple Version Control

A lightweight, file-oriented version control system implemented in Python. It fulfils the `/g/` programming challenge requirement for "Simple Version Control supporting checkout commit (with commit message) unlocking and per-file configuration of number of revisions kept".

## Features

- File-level commit history with commit messages.
- Configurable revision limits per tracked file; older revisions are pruned automatically.
- Locking semantics to prevent accidental commits on protected files.
- Checkout command to restore any stored revision back into the working directory.
- Diff and log commands for quick inspection of file history.
- JSON-based repository metadata stored in `.svc/config.json` plus on-disk revision snapshots.
- Tested with `pytest`, covering commit/checkout cycles, revision pruning, locks, and CLI inspection commands.

## Command Overview

All commands are available through the CLI entry point: `python -m simple_vcs <command>`. Pass `--path PATH` to target a repository outside the current directory.

### `init`

Initialises a new repository by creating `.svc/config.json` and the revision store.

```bash
python -m simple_vcs init
```

Use `--force` to overwrite an existing repository configuration.

### `commit`

Creates a new revision for one or more files and stores their contents alongside a commit message.

```bash
python -m simple_vcs commit -m "Describe the change" path/to/file.txt
```

### `checkout`

Restores a committed revision into the working tree. If `--revision` is omitted, the latest revision is used.

```bash
python -m simple_vcs checkout path/to/file.txt --revision 20240101120000-123
```

### `config`

Manages repository configuration such as revision limits and file locks, or prints the current configuration as JSON.

```bash
python -m simple_vcs config set-limit notes.txt 3
python -m simple_vcs config lock notes.txt
python -m simple_vcs config unlock notes.txt
python -m simple_vcs config show
```

### `diff`

Shows a unified diff between the working copy and a stored revision (latest by default).

```bash
python -m simple_vcs diff notes.txt
```

### `log`

Lists all known revisions for the specified file.

```bash
python -m simple_vcs log notes.txt
```

## Usage Example

```bash
python -m simple_vcs init
printf "v1" > demo.txt
python -m simple_vcs commit -m "Add demo" demo.txt
printf "v2" > demo.txt
python -m simple_vcs commit -m "Update demo" demo.txt
python -m simple_vcs log demo.txt
python -m simple_vcs checkout demo.txt --revision <first-revision-id>
```

## Running the Tests

From the repository root:

```bash
pytest Practical/Simple\ VCS/tests
```
