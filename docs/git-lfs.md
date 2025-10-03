# Git LFS Setup and Troubleshooting

Many of the projects in this repository track large binary assets (audio samples, sprites, pretrained models) using [Git Large File Storage (LFS)](https://git-lfs.com/). Install Git LFS before cloning or working on the codebase so that all assets download correctly and builds/tests succeed.

## 1. Install Git LFS

| Platform | Installation command |
| -------- | -------------------- |
| **Windows** | `winget install Git.GitLFS` or install via the [Git for Windows](https://gitforwindows.org/) installer by selecting the **Git LFS** component. |
| **macOS** | `brew install git-lfs` (Homebrew) |
| **Linux (Debian/Ubuntu)** | `sudo apt update && sudo apt install git-lfs` |
| **Linux (Fedora/RHEL)** | `sudo dnf install git-lfs` |
| **Other distros** | Refer to the [official package list](https://github.com/git-lfs/git-lfs#installation) or download the binary release. |

> Already cloned the repo? Install Git LFS now, then continue with the steps below.

## 2. Enable Git LFS in your clone

Run the following once per machine (after installing Git LFS):

```bash
git lfs install
```

This command configures the global Git hooks that fetch large files and keeps them synced with `git pull`.

## 3. Fetch LFS assets before building or testing

1. Clone the repository as usual:
   ```bash
   git clone https://github.com/saintwithataint/Pro-g-rammingChallenges4.git
   cd Pro-g-rammingChallenges4
   ```
2. Pull all tracked large files:
   ```bash
   git lfs pull
   ```
3. (Optional) Verify that LFS pointers are replaced with real files:
   ```bash
   git lfs ls-files
   ```

Always run `git lfs pull` after switching branches or when new assets land in a pull request. Without this step, many projects (pygame games, audio tools, pretrained models) will fail to load required resources during builds or tests.

## 4. Troubleshooting: `smudge filter failed` errors

The `smudge filter` runs during checkout to replace Git LFS pointers with real files. If it fails, large assets stay as tiny pointer files and downstream builds break. Use the recovery steps below.

### 4.1. Sync LFS files explicitly

```bash
git lfs pull
```

This re-downloads any missing assets from the remote and resolves most smudge failures.

### 4.2. Re-check out LFS pointers

If files remain stuck as pointers, force Git to refresh them:

```bash
git lfs checkout
```

### 4.3. Clear cached smudge/clean filters

Corrupted LFS cache or hooks can trigger repeated smudge failures. Reset them with:

```bash
git lfs uninstall
rm -rf .git/lfs/tmp
rm -rf .git/lfs/incomplete
rm -rf .git/hooks/post-checkout .git/hooks/post-merge .git/hooks/pre-push
git lfs install
```

Alternatively, clear only the LFS cache:

```bash
rm -rf .git/lfs/cache
```

Then repeat `git lfs pull`.

### 4.4. Re-clone as a last resort

If the repository was cloned without Git LFS installed, the cleanest fix is to reinstall Git LFS, remove the existing clone, and re-clone:

```bash
rm -rf Pro-g-rammingChallenges4
git clone https://github.com/saintwithataint/Pro-g-rammingChallenges4.git
cd Pro-g-rammingChallenges4
git lfs pull
```

---

Keeping Git LFS healthy ensures sprite sheets, audio packs, and other binary dependencies download with the rest of the source, letting you build and test every project without missing files.
