# Troubleshooting

Common errors, warnings, FAQs and their solutions. If your problem is not listed, please open an issue on [GitHub](https://github.com/hydrogeoscience/pygtide/issues).

## Input validation errors

PyGTide validates all inputs in Python before calling the Fortran core, so bad input raises a `ValueError` with a descriptive message instead of crashing. Causes and fixes:

| Error message | Cause | Fix |
|---|---|---|
| `Latitude exceeds permissible range!` | latitude outside ±90° | use decimal degrees, WGS84 |
| `Longitude exceeds permissible range!` | longitude outside ±180° | use −180…180°, positive east |
| `Height exceeds permissible range!` | height outside −500…5000 m | use ellipsoidal height in metres |
| `Duration exceeds permissible range!` | duration ≤ 0 or > 87600 h (10 yr) | split long predictions into chunks |
| `samprate exceeds permissible range!` | sampling rate ≤ 0 or > 86400 s | use seconds, ≤ 24 h |
| `samprate exceeds duration!` | fewer than one sample in the series | increase `duration` or decrease `samprate` |
| `Startdate has incorrect format (YYYY-MM-DD)!` | unparseable date string | pass `'YYYY-MM-DD'` or a `datetime` object |
| `Station gravity exceeds permissible range!` | `statgravit` outside 0…20 m/s² | use 0 for automatic normal gravity |
| `Statazimut exceeds permissible range!` | azimuth outside 0…180° | azimuth clockwise from north |
| `Tidalpoten must be an integer between 1 and 8!` | invalid catalogue number | see the [catalogue table](configuration.md#tidal-potential-catalogues-tidalpoten) |
| `Tidalcompo must be an integer between -1 and 9!` | invalid component number | see the [component table](configuration.md#earth-tide-components-tidalcompo) |
| `The wave group input must have 4 columns!` | malformed wave-group array | use an N×4 NumPy array ([format](configuration.md#wave-group-format-set_wavegroup)) |
| `Wave group frequency ranges must be increasing and not overlapping!` | overlapping/unsorted bands | sort bands by frequency |
| `Amplitude factors must be positive!` | factor ≤ 0 in wave-group array | use factors > 0 |
| `PyGTide should not be used for dates before the year 1600.` | start date < 1600 with pole/LOD corrections on | use later dates, or set `poltidecor=0, lodtidecor=0` |
| `Install path too long for the Fortran interface` | installation path > 1012 characters | reinstall into a shorter path |

## Warnings about outdated data files

```text
UserWarning: Please consider updating the leap second database 'etddt.dat' ...
UserWarning: Dates exceed permissible range for pole/LOD tide correction ...
```

The prediction still runs (the last tabulated values are used), but accuracy degrades the further you extrapolate. Fix:

```bash
python -c "import pygtide; pygtide.update()"
```

## Database update fails

`pygtide.update()` downloads from IERS and USNO servers. Typical failure modes:

- **No internet connection / server unreachable** — the updater prints `ERROR: Could not connect to remote server`, keeps the bundled files, and continues. Nothing is broken; just retry later.
- **Corporate firewall or proxy** — allow HTTPS access to `hpiers.obspm.fr` and `datacenter.iers.org`. One legacy mirror uses FTP, which many networks block.
- **Partial download** — delete any `[raw]_*.dat` leftovers in the package's `commdat` directory and run the update again.

## Installation path too long

The Fortran core receives the data-directory path through a fixed-length buffer (1024 characters in v0.9.1). Symptoms and fixes:

- **v0.9.1 and later:** a clear `RuntimeError: Install path too long for the Fortran interface` at start-up.
- **v0.9.0 and earlier:** the path was *silently truncated* at 256 characters, which can lead to missing-data behaviour or wrong results without any error.

**Fix:** reinstall into a shorter path (e.g. move your virtual environment closer to the drive root, avoid long user names or deeply nested project folders), and upgrade with `pip install -U pygtide`.

## Known issues by version

> [!NOTE]
> All issues below are fixed in the current release — `pip install -U pygtide` resolves them. They are listed for users stuck on older versions.

| Symptom | Affected versions | Cause / fix |
|---|---|---|
| `AttributeError: module 'numpy' has no attribute 'core'` in `results()` | ≤ 0.9.0 with NumPy ≥ 2.2 | `np.core.defchararray.add` was removed in NumPy 2.2; fixed in v0.9.1 |
| Silent wrong results with very long installation paths | ≤ 0.9.0 | Fortran path buffer (256 chars) truncated the data path; v0.9.1 enlarges it to 1024 and raises `RuntimeError` |
| `NaN` in strain output at specific latitudes | ≤ 0.9.0 | Removable singularities in the strain formulas at geocentric latitudes ±19.88, ±22.21, ±26.57, ±35.26, ±40.89, ±50.77, ±59.44°; guarded in v0.9.1 |
| Shear strain (`tidalcompo=7`) zero at azimuth 0/90/180° | ≤ 0.8.x | Shear wrongly projected onto the sensor azimuth; fixed in v0.9 |
| Shear strain phase offset by 90° | ≤ 0.9.0 | ε_θλ is in quadrature with the potential; `DPK=-90°` fix in v0.9.1 |
| Areal strain inconsistent with volume strain (areal ≠ 1.5 × volume) | ≤ 0.8.x | Geodetic-coefficient bug (issue #47); fixed in v0.9 |
| Pole tide / LOD tide wrong at the poles or equator | ≤ 0.8.x | Missing singularity guards; fixed in v0.9 |

## FAQ

**What units is the output in?**
Depends on `tidalcompo` — see the [component table](configuration.md#earth-tide-components-tidalcompo). Gravity (default) is in nm/s² (divide by 10 for µGal, by 100 for mGal).

**Why is gravity positive when I expect it negative?**
Gravity tides are **positive downwards** (towards the Earth's centre), following the ETERNA convention. Vertical displacement is positive upwards.

**Why does my gravity amplitude look ~16% too small?**
It is not wrong — the default output is the rigid-Earth (geometric) tide. Multiply by the gravimetric factor via `set_wavegroup()` to get body tides: [Background — Absolute scale](background.md#absolute-scale-of-the-output).

**Why is my shear strain (`tidalcompo=7`) unaffected by `statazimut`?**
By design — shear strain is a tensor component and must not depend on sensor orientation (fixed in v0.9).

**Where did the `pygtide.out.prd` / `pygtide.out.prn` files come from?**
You passed `fileprd=1` or `fileprn=1`. These are legacy ETERNA output files written to the module directory; both default to 0 (disabled).

**Can I predict tides before 1962 or far into the future?**
Yes, but pole/LOD corrections then rely on extrapolated values and a warning is issued. Leap seconds are only defined up to the last announced value — for future dates the last tabulated offset is used.

**How do I convert the results to local time?**

```python
data = pt.results()
data['UTC'] = data['UTC'].dt.tz_convert('Australia/Sydney')
```

**Does PyGTide run in parallel / on a cluster?**
Each `pygtide` object is self-contained; you can run independent predictions in separate processes. Note that all objects share the compiled module, so avoid concurrent calls from threads within one process.

## Still stuck?

- Re-run the test suite to check your installation: `python -c "import pygtide; pygtide.test(msg=True)"`
- Check the [Usage Guide](usage.md) and [Configuration](configuration.md) for correct calling conventions
- Search or open an issue: <https://github.com/hydrogeoscience/pygtide/issues>
