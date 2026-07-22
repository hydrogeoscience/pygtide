# Installation

This guide walks you through installing PyGTide, verifying the installation, and updating its internal data files.

## Prerequisites

| Requirement | Details |
|---|---|
| Python | ≥ 3.10 (pre-compiled wheels available for CPython 3.12–3.14) |
| Package manager | [Anaconda](https://www.anaconda.com/products/distribution) or [Miniconda](https://docs.conda.io/en/latest/miniconda.html) recommended, but any `pip` works |
| Fortran compiler | Only needed for source builds (Python 3.10/3.11), e.g. `gfortran` |

## Option 1: Install from PyPI (recommended)

```bash
pip install pygtide
```

Pre-compiled wheels are provided for Linux, macOS and Windows on Python 3.12 and newer — no compiler required. On Python 3.10 and 3.11, pip automatically builds from the source distribution instead, which requires a Fortran compiler (see Option 2). NumPy and pandas are installed automatically as dependencies.

To upgrade an existing installation:

```bash
pip install -U pygtide
```

> [!NOTE]
> Always use the latest release. Several numerical bug fixes in the Fortran core (geodetic coefficients, shear strain, leap-second handling) were shipped in v0.9 and v0.9.1 — see [Troubleshooting — Known issues by version](troubleshooting.md#known-issues-by-version).

## Option 2: Build from source (Linux, macOS, Windows; Python >= 3.10)

**Requirements for building:**

- A Fortran compiler (e.g. `gfortran`, via `conda install gfortran` or your system package manager)
- Meson and ninja (installed automatically by pip)

**Clone the repository and install:**

```bash
git clone https://github.com/hydrogeoscience/pygtide.git
cd pygtide
pip install .
```

> [!WARNING]
> Keep the installation path reasonably short. Versions before v0.9.1 silently truncated data paths longer than ~244 characters; v0.9.1 raises a clear `RuntimeError` instead. See [Troubleshooting](troubleshooting.md#installation-path-too-long).

## Verify the installation

Run the bundled test suite:

```bash
python -c "import pygtide; pygtide.test(msg=True)"
```

The test compares a calculated strain series against a proven reference array and checks the M2 tidal frequency. You should see:

```text
Successfully finished PyGTide tests!
```

## Update the internal databases

PyGTide ships with data files for leap seconds (`etddt.dat`) and Earth orientation (`etpolut1.dat`) in its `commdat` subdirectory. Update them to the latest IERS/USNO data with:

```bash
python -c "import pygtide; pygtide.update()"
```

> [!NOTE]
> The update requires an internet connection. If a download server is unreachable, the updater fails gracefully and the bundled (slightly older) files remain in use — see [Troubleshooting — Database update fails](troubleshooting.md#database-update-fails).

## Next steps

- [Quickstart](quickstart.md) — compute your first tide in two minutes
- [Usage Guide](usage.md) — full feature walkthrough
- [Configuration](configuration.md) — every parameter and keyword
