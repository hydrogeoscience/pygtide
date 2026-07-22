# Configuration

Complete reference for all PyGTide inputs: the required `predict()` arguments, the optional control keywords, the catalogue and component selectors, the wave-group format, and the bundled data files.

## Required `predict()` arguments

| Argument | Unit | Valid range | ETERNA equivalent | Description |
|---|---|---|---|---|
| `latitude` | degree | −90 … 90 | `STATLATITU` | Ellipsoidal latitude (WGS84) |
| `longitude` | degree | −180 … 180 | `STATLONITU` | Ellipsoidal longitude (WGS84), positive east |
| `height` | m | −500 … 5000 | `STATELEVAT` | Ellipsoidal height (WGS84) |
| `startdate` | — | `'YYYY-MM-DD'` or `datetime` | `INITIALEPO` | Initial epoch (UTC); not before year 1600 when pole/LOD corrections are on |
| `duration` | h | 0 … 87600 (10 yr) | `PREDICSPAN` | Length of the prediction |
| `samprate` | s | 0 … 86400 | `SAMPLERATE` | Sampling interval; must not exceed `duration` |

## Optional control keywords

| Keyword | Default | Valid range | Description |
|---|---|---|---|
| `statgravit` | 0 (= auto) | 0 … 20 m/s² | Station gravity; values < 9.5 are replaced by GRS80 normal gravity. Needed for tidal tilt only |
| `statazimut` | 0 | 0 … 180° | Sensor azimuth clockwise from north; used by components 1, 3, 5 |
| `tidalpoten` | 8 | 1 … 8 | Tidal potential catalogue (table below) |
| `tidalcompo` | 0 | −1 … 9 | Earth tide component (table below) |
| `amtruncate` | 1e-10 | ≥ 0 m²/s² | Amplitude threshold — waves below this are skipped (faster, less accurate) |
| `poltidecor` | 1.16 | ≥ 0 | Pole-tide amplitude factor (gravity only); 0 disables the correction |
| `lodtidecor` | 1.16 | ≥ 0 | LOD-tide amplitude factor (gravity only); 0 disables the correction |
| `fileprd` | 0 | 0 / 1 | Write the classic ETERNA output file `pygtide.out.prd` |
| `fileprn` | 0 | 0 / 1 | Write the classic ETERNA print file `pygtide.out.prn` |
| `screenout` | 0 | 0 / 1 | Enable Fortran screen output (not visible in the Python terminal) |

### Amplitude truncation vs accuracy

For the Hartmann & Wenzel (1995) catalogue, the gravity-tide accuracy depends on `amtruncate` as follows (benchmark from the ETERNA documentation):

| `amtruncate` [m²/s²] | Waves used | RMS error [nm/s²] |
|---|---|---|
| 1e-02 | 42 | 14.4 |
| 1e-03 | 155 | 2.25 |
| 1e-04 | 434 | 0.44 |
| 1e-05 | 1248 | 0.068 |
| 1e-06 | 3268 | 0.011 |
| 1e-07 | 7761 | 0.002 |
| ≤ 1e-08 | ~12000 | 0.001 |

## Tidal potential catalogues (`tidalpoten`)

| Value | Catalogue | Waves | RMS error [nGal] |
|---|---|---|---|
| 1 | Doodson (1921) | 378 | 102 |
| 2 | Cartwright–Tayler–Edden (1973) | 505 | 37.4 |
| 3 | Buellesfeld (1985) | 656 | 24 |
| 4 | Tamura (1987) | 1200 | 6.7 |
| 5 | Xi (1989) | 2933 | 7.9 |
| 6 | Roosbeek (1996) | 6499 | 2 |
| 7 | Hartmann & Wenzel HW95 (1995) | 12935 | 0.14 |
| 8 | Kudryavtsev KSM03 (2004) — **default** | 28806 | 0.025 |

## Earth tide components (`tidalcompo`)

| Value | Component | Unit | Absolute scale | Uses `statazimut` |
|---|---|---|---|---|
| −1 | Tidal potential | m²/s² | rigid (geometric) | no |
| 0 | Gravity (vertical acceleration, **positive down**) | nm/s² | rigid (geometric) | no |
| 1 | Tilt | mas | rigid (geometric) | yes |
| 2 | Vertical displacement (**positive up**) | mm | body tide (h applied) | no |
| 3 | Horizontal displacement | mm | body tide (l applied) | yes |
| 4 | Vertical strain ε_rr | nstr (1e-9) | body tide | no |
| 5 | Horizontal strain | nstr | body tide | yes |
| 6 | Areal strain (ε_θθ + ε_λλ) | nstr | body tide | no |
| 7 | Shear strain (ε_θλ) | nstr | body tide | no (tensor component) |
| 8 | Volume strain | nstr | body tide | no |
| 9 | Ocean tide | mm | — | no |

> [!IMPORTANT]
> "Rigid (geometric)" means the Wahr–Dehant–Zschau body-tide factors are divided out at the main wave of each wave group; they only modulate satellite waves within a group. Apply amplitude factors via `set_wavegroup()` to get body-tide gravity/tilt. Full explanation: [Background — Absolute scale](background.md#absolute-scale-of-the-output).

## Wave-group format (`set_wavegroup`)

A NumPy array with exactly 4 columns and up to 85 rows:

| Column | Meaning | Constraint |
|---|---|---|
| 1 | Lower frequency limit [cycles/day] | ranges increasing, non-overlapping |
| 2 | Upper frequency limit [cycles/day] | ranges increasing, non-overlapping |
| 3 | Amplitude factor | > 0 |
| 4 | Phase lead [degree] | — |

Default: `[[0, 10, 1, 0]]` — the whole catalogue, unscaled.

Standard wave bands for reference (frequencies in cycles/day, main wave in brackets):

| Band | Range | Main wave |
|---|---|---|
| Long period | 0.0000 – 0.5014 | Mf (0.0732) |
| Q1 | 0.5014 – 0.9114 | Q1 (0.8932) |
| O1 | 0.9114 – 0.9480 | O1 (0.9295) |
| P1/S1/K1 | 0.9819 – 1.0041 | K1 (1.0027) |
| N2 | 1.8803 – 1.9141 | N2 (1.8960) |
| M2 | 1.9141 – 1.9504 | M2 (1.9323) |
| S2/K2 | 1.9843 – 2.4519 | S2 (2.0000) |

## Data files (`commdat` subdirectory)

| File | Contents | Updated by |
|---|---|---|
| `etddt.dat` | Leap-second table (TDT − UTC) | `pygtide.update()` |
| `etpolut1.dat` | Daily pole coordinates, UT1−UTC, TAI−UT1 (IERS + USNO) | `pygtide.update()` |
| `etpolut1.bin` | Binary direct-access copy of the above (32-byte records) | regenerated automatically |
| `etddt_tmpl.dat` | Template used when rebuilding `etddt.dat` | — |
| `doodsehw.dat`, `cted73hw.dat`, `buellehw.dat`, `tamurahw.dat`, `xi1989hw.dat`, `ratgp95.dat`, `hw95s.dat`, `ksm03.dat` | The 8 tidal potential catalogues | static |
| `*.bin` | Unformatted catalogue caches, created on first use | automatic |

## Platform notes

- The Fortran null device is set from Python via `os.devnull`, so muting works identically on Windows, Linux and macOS.
- The data-directory path is limited to 1024 characters (Fortran `COMDIR` buffer); v0.9.1 raises a `RuntimeError` if your installation path is longer. Older versions truncated silently — see [Troubleshooting](troubleshooting.md#installation-path-too-long).
- Python status messages can be suppressed with `pygtide.pygtide(msg=False)`.
