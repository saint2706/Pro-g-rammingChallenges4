# Repository Organization Strategy

This document captures the reasoning behind the new `challenges/` layout and the
alternatives that were considered while restructuring the project.

## Background

Earlier iterations of the repository stored every challenge category directly in
the repository root (`Practical/`, `Algorithmic/`, `Games/`, `Artificial Intelligence/`,
`Emulation/`). While this mirrored the `/g/` challenge list, it also left the root
crowded with hundreds of sub-projects and made it harder to understand what was
production tooling versus learning artifacts. Python tests, packaging metadata,
and helper scripts increasingly had to special-case the layout.

## Options Considered

### 1. Maintain the flat layout
- **Pros:** zero migration work; all historical paths keep functioning.
- **Cons:** repository root remains cluttered, onboarding contributors must sift
  through dozens of directories, and automation needs to enumerate every
  category explicitly. This option also complicates packaging because namespace
  packages cannot rely on predictable prefixes.

### 2. Introduce lightweight aggregators (documentation-only)
- **Pros:** adds structure at the documentation level without moving files.
- **Cons:** does not actually solve the clutter problem, and the disconnect
  between documentation and filesystem layout risks confusing contributors.

### 3. Move categories under a dedicated `challenges/` directory *(chosen)*
- **Pros:** the repository root now highlights only active tooling (`docs/`,
  `src/`, `tests/`, `tools/`, configuration files). Categories are grouped in a
  single location, making it easier to reference them from code (`challenges/…`)
  and documentation. Updating imports to `challenges.<category>` ensures Python
  tooling can reason about modules without relying on implicit namespace
  packages rooted at the top level.
- **Cons:** requires updating documentation, import paths, and skip patterns in
  tests. Local clones with custom scripts must adjust to the new directory.

## Resulting Layout

All challenge implementations now live under `challenges/`:

```
challenges/
├── Algorithmic/
├── Artificial Intelligence/
├── Emulation/
├── Games/
└── Practical/
```

Key tooling remains at the root (see the main README for a full tree). A new
`challenges` package provides `load_category(name)` to ease dynamic imports.

## Migration Notes

- Update imports from `from Emulation...` to
  `from challenges.Emulation...` (and likewise for other categories).
- When referencing paths in documentation or configuration, prefix them with
  `challenges/` (e.g., `challenges/Practical/Image Converter/convert.py`).
- Tests now use `CHALLENGES_ROOT = REPO_ROOT / "challenges"` to enumerate CLI
  entry points and resources.

The restructuring keeps historical category names while providing a predictable
root that scales as more challenges are added.
