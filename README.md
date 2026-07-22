# PyGTide

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.21483314.svg)](https://doi.org/10.5281/zenodo.21483314)



PyGTide is a Python module that computes gravitational tides on Earth. It wraps ETERNA PREDICT 3.4 — the classic Fortran 77/90 code by the late Prof. H.-G. Wenzel (Wenzel, 1996), updated with the KSM03 tidal catalogue by Kudryavtsev (2004) — compiled into a Python extension using [f2py](https://docs.scipy.org/doc/numpy/f2py/). The original Fortran code was comprehensively revised for the Python integration, and significant bugs were identified and repaired (see the CHANGELOG).

## Key features

- Computes 11 tidal components: tidal potential, gravity, tilt, vertical/horizontal displacement, and vertical/horizontal/areal/shear/volume strain
- Choice of 8 tidal potential catalogues, up to Kudryavtsev KSM03 with 28,806 waves (~0.025 nGal accuracy)
- Pole-tide and length-of-day (LOD) corrections based on IERS Earth orientation data
- Results returned as a time-stamped [pandas](https://pandas.pydata.org/) DataFrame in UTC
- Fast f2py-compiled Fortran core with a convenient Python class interface
- Pre-compiled wheels for Linux, macOS and Windows — no Fortran compiler required

## Documentation

| Document | Contents |
|---|---|
| [Installation](docs/installation.md) | Prerequisites and step-by-step setup |
| [Quickstart](docs/quickstart.md) | The fastest way to a first result |
| [Usage Guide](docs/usage.md) | Core features, class API and worked examples |
| [Configuration](docs/configuration.md) | All parameters, keywords and data files |
| [Background](docs/background.md) | Earth tides, time scales, catalogues and output conventions |
| [Troubleshooting](docs/troubleshooting.md) | Common errors, FAQs and solutions |

## Quick install and first run

```bash
pip install pygtide
```

```python
from pygtide import predict_series

# latitude, longitude, height, startdate, duration [h], samprate [s]
args = (49.00937, 8.40444, 120, '2018-01-01', 24 * 7, 3600)
series = predict_series(*args)
```

> [!IMPORTANT]
> By default, gravity, potential and tilt outputs are **rigid-Earth (geometric) tides**, while displacement and strain outputs are **body tides** (Love numbers applied). Read [Background — Absolute scale of the output](docs/background.md#absolute-scale-of-the-output) before interpreting amplitudes.

## Example

<img src="https://raw.githubusercontent.com/hydrogeoscience/pygtide/master/earth_tide_example.png" width="500">
This image shows Earth tides calculated for the city Karlsruhe (Germany) in the year 2018.

## How to cite

If you use PyGTide, please cite the work as:

*Gabriel C Rau, Eulenfeld, T., Miller, C. (2026). hydrogeoscience/pygtide: PyGTide v0.9.1 (Version v0.9.1) [Computer software]. Zenodo. https://doi.org/10.5281/zenodo.21483314*

## References

* Hartmann, T., and H.-G. Wenzel (1995), The HW95 tidal potential catalogue, *Geophysical Research Letters*, 22(24), 3553–3556, <https://doi.org/10.1029/95GL03324>.
* Kudryavtsev, S. M. (2004), Improved harmonic development of the Earth tide-generating potential, *Journal of Geodesy*, 77(12), 829–838, <https://doi.org/10.1007/s00190-003-0361-2>.
* Wenzel, H.-G. (1996), The nanogal software: Earth tide data processing package ETERNA 3.30, *Bulletin d'Informations des Marées Terrestres*, 124, 9425–9439.
* McMillan, T. C., Rau, G. C., Timms, W. A., and Andersen, M. S. (2019), Utilizing the impact of Earth and atmospheric tides on groundwater systems: A review reveals the future potential, *Reviews of Geophysics*, <https://dx.doi.org/10.1029/2018RG000630>.

## License

PyGTide is released by [Gabriel C. Rau](https://hydrogeo.science), [Tom Eulenfeld](https://scholar.google.com/citations?user=SJXF3mwAAAAJ&hl=en) and [Craig Miller](https://www.gns.cri.nz/about-us/staff-search/craig-miller/) under the [Mozilla Public License 2.0](https://www.mozilla.org/en-US/MPL/2.0/).
