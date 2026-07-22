# Usage Guide

This guide covers the PyGTide class API in detail: running predictions, choosing tidal components, working with wave groups, applying pole/LOD corrections, and keeping the internal databases current. For a minimal example, see the [Quickstart](quickstart.md).

## The `pygtide` class

All functionality is accessed through one class:

```python
import pygtide

pt = pygtide.pygtide(msg=True)   # msg=False suppresses status messages
```

Initialisation loads the compiled Fortran module, points it at the bundled `commdat` data files, and reads the validity ranges of the leap-second and pole-coordinate databases (used for date-range warnings).

## Running a prediction

```python
pt.predict(latitude, longitude, height, startdate, duration, samprate, **control)
```

| Parameter | Unit | Description |
|---|---|---|
| `latitude` | degree | Ellipsoidal latitude, WGS84 (−90 … 90) |
| `longitude` | degree | Ellipsoidal longitude, WGS84, positive east (−180 … 180) |
| `height` | m | Ellipsoidal height, WGS84 (−500 … 5000) |
| `startdate` | — | Initial epoch, `'YYYY-MM-DD'` string or `datetime` object (UTC) |
| `duration` | h | Length of the prediction (max 10 years) |
| `samprate` | s | Sampling interval (max 24 h; must not exceed `duration`) |

Optional `**control` keywords (`statazimut`, `tidalcompo`, `tidalpoten`, `poltidecor`, …) are documented in [Configuration](configuration.md#optional-control-keywords). `predict()` validates all inputs in Python before calling the Fortran core, so invalid values raise a `ValueError` instead of crashing the extension — see [Troubleshooting](troubleshooting.md#input-validation-errors).

## Retrieving results

| Method | Returns |
|---|---|
| `pt.results(digits=6)` | pandas DataFrame with `UTC` index column and one column per output channel |
| `pt.raw()` | Raw NumPy array handed back from Fortran (date, time, channels) |
| `pt.data(digits=6)` | NumPy array of the data channels only (no dates) |
| `pt.datetime()` | NumPy string array with separate date and time columns |

All methods return `None` if `predict()` has not been executed yet.

## Choosing the tidal component

Pass `tidalcompo` to `predict()`:

```python
# tidal gravity (default)
pt.predict(lat, lon, h, start, duration, samprate, tidalcompo=0)

# tidal tilt towards the east (azimuth 90 deg)
pt.predict(lat, lon, h, start, duration, samprate, tidalcompo=1, statazimut=90)

# volume strain
pt.predict(lat, lon, h, start, duration, samprate, tidalcompo=8)
```

The full component table (units, rigid vs body-tide scale, azimuth dependence) is in [Configuration — Earth tide components](configuration.md#earth-tide-components-tidalcompo); the physics behind it is in [Background](background.md#absolute-scale-of-the-output).

## Choosing the tidal potential catalogue

Pass `tidalpoten` (1–8). The default is 8, the Kudryavtsev KSM03 catalogue (28,806 waves, highest accuracy). Catalogue details and accuracies: [Configuration — Tidal potential catalogues](configuration.md#tidal-potential-catalogues-tidalpoten).

```python
pt.predict(lat, lon, h, start, duration, samprate, tidalpoten=7)  # Hartmann & Wenzel HW95
```

## Wave groups

Wave groups define frequency bands (in cycles per day) with individual amplitude and phase factors. They serve two purposes: applying body-tide amplitude factors to the rigid-Earth output, and isolating parts of the spectrum.

```python
import numpy as np

# 4 columns: freq from [cpd], freq to [cpd], amplitude factor, phase lead [deg]
waves = np.array([
    [0.0, 0.1, 1.0, 0.0],   # long-period band
    [0.8, 1.2, 1.0, 0.0],   # diurnal band
    [1.8, 2.2, 1.0, 0.0],   # semidiurnal band
])
pt.set_wavegroup(waves)
pt.predict(lat, lon, h, start, duration, samprate)
pt.reset_wavegroup()   # back to: all waves, factor 1
```

Constraints (validated in Python):

- exactly 4 columns, up to 85 rows
- frequency ranges must be increasing and non-overlapping
- amplitude factors must be positive

### Body-tide gravity and tilt

Potential, gravity and tilt are rigid-Earth tides by default. To obtain body tides, scale the wave group(s) with the appropriate factor — for gravity:

```python
pt.set_wavegroup(np.asarray([[0, 10, 1.16, 0]]))
```

For per-band factors (recommended, because the gravimetric factor δ varies between bands, especially across the NDFW resonance in the diurnal band), split the spectrum into the standard ETERNA bands:

```python
DC    = np.array([0.000000, 0.000001, 1.000000, 0.0])
Long  = np.array([0.000002, 0.249951, 1.160000, 0.0])
Q1    = np.array([0.721500, 0.906315, 1.154250, 0.0])
O1    = np.array([0.921941, 0.974188, 1.154240, 0.0])
P1    = np.array([0.989049, 0.998028, 1.149150, 0.0])
K1    = np.array([0.999853, 1.216397, 1.134890, 0.0])
N2    = np.array([1.719381, 1.906462, 1.161720, 0.0])
M2    = np.array([1.923766, 1.976926, 1.161720, 0.0])
S2    = np.array([1.991787, 2.002885, 1.161720, 0.0])
K2    = np.array([2.003032, 2.182843, 1.161720, 0.0])
M3    = np.array([2.753244, 3.081254, 1.073380, 0.0])
other = np.array([3.791964, 3.937897, 1.039000, 0.0])

pt.set_wavegroup(np.vstack((DC, Long, Q1, O1, P1, K1, N2, M2, S2, K2, M3, other)))
```

> [!NOTE]
> Displacement and strain components (`tidalcompo=2…8`) already include the Love numbers — do **not** scale them. See [Background — Absolute scale](background.md#absolute-scale-of-the-output).

## Pole-tide and LOD corrections

For gravity, corrections for polar motion (pole tide) and length-of-day variations are applied by default (`poltidecor=1.16`, `lodtidecor=1.16`) and appear as separate output columns. Set a factor to 0 to disable the corresponding correction:

```python
pt.predict(lat, lon, h, start, duration, samprate, poltidecor=0, lodtidecor=0)
```

The corrections use daily IERS pole coordinates — keep the database current (see below). Predicting outside the database range triggers a warning, and dates before the year 1600 are rejected.

## Module-level convenience functions

For quick scripts you can skip the class entirely:

```python
from pygtide import predict_table, predict_series, predict_spectrum

args = (49.00937, 8.40444, 120, '2018-01-01', 24 * 30, 600)

table = predict_table(*args)                  # DataFrame (like pt.results())
series = predict_series(*args, tidalcompo=8)  # NumPy array of one channel
freq, spec = predict_spectrum(*args)          # rFFT amplitude spectrum
```

| Function | Returns |
|---|---|
| `predict_table(*args, msg=False, **kwargs)` | pandas DataFrame |
| `predict_series(*args, msg=False, index=0, **kwargs)` | 1-D NumPy array of channel `index` |
| `predict_spectrum(*args, nfft=None, **kwargs)` | `(freq, complex spectrum)` from a real FFT |
| `plot_series(*args, indices=(0, 1), show=True, **kwargs)` | matplotlib plot of the series |
| `plot_spectrum(*args, ax=None, show=True, **kwargs)` | matplotlib plot of the spectrum |

All keywords of `predict()` (e.g. `tidalcompo`, `statazimut`) can be passed through.

## Worked example: internal consistency of the strain components

For a spherical Earth with Poisson ratio ν = 0.25 (the model used in ETERNA), the strain components obey `areal = 1.5 × volume` and `e_rr = −areal / 3` — a useful sanity check:

```python
import numpy as np
from pygtide import predict_series

args = (49.00937, 8.40444, 120, '2018-01-01', 24 * 7, 3600)
areal  = predict_series(*args, tidalcompo=6)
volume = predict_series(*args, tidalcompo=8)

ratio = areal / volume
print(np.nanmean(ratio))   # ≈ 1.5
```

## Updating the internal databases

The leap-second (`etddt.dat`) and Earth-orientation (`etpolut1.dat`) files in the `commdat` subdirectory can be updated to the latest IERS/USNO data at any time:

```bash
python -c "import pygtide; pygtide.update()"
```

The updater downloads the IERS leap-second history, historic EOP observations and current predictions, merges them, and rewrites both the text and binary data files. Details and failure modes: [Troubleshooting — Database update fails](troubleshooting.md#database-update-fails).

## Legacy file and screen output

For compatibility with the original ETERNA workflow, `predict()` accepts `fileprd=1` / `fileprn=1` to write the classic ETERNA output files (`pygtide.out.prd`, `pygtide.out.prn`) into the module directory, and `screenout=1` to enable the Fortran screen output (not visible in the Python terminal). All default to 0.

## Next steps

- [Configuration](configuration.md) — complete parameter and keyword reference
- [Background](background.md) — time scales, catalogues, the rigid vs body-tide convention
- [Troubleshooting](troubleshooting.md) — errors and FAQs
