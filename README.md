# PyGTide
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.1346260.svg)](https://doi.org/10.5281/zenodo.1346260)

## A Python module and wrapper for ETERNA PREDICT to compute gravitational tides on Earth

PyGTide is a Python module that wraps around ETERNA PREDICT 3.4 which is compiled from Fortran into an executable using [f2py](https://docs.scipy.org/doc/numpy/f2py/). The original ETERNA PREDICT 3.3 was written by the late Prof. H.-G. Wenzel (Wenzel, 1996) in a mix of Fortran 77 and 90. This was updated by Kudryavtsev (2004) to include the latest tidal catalogue. Note that the original Fortran code was comprehensively revised in order to facilitate integration into Python. Significant bugs were also identified and repaired, please refer to the CHANGELOG for details. The original Fortran code for ETERNA PREDICT can be downloaded from the [International Geodynamics and Earth Tide Service (IGETS)](http://igets.u-strasbg.fr/soft_and_tool.php).

## How to use

There are two options:
* Download and install on your system (see below instructions)
* Via our online calculator: <a href="https://groundwater.app/app.php?app=pygtide" target="_blank">groundwater.app</a>


## How to install and run

### Prerequisites

* Download and install [*Anaconda*](https://www.anaconda.com/products/distribution) or [*Miniconda*](https://docs.conda.io/en/latest/miniconda.html)

### Installation options

#### Option 1: Install from PyPI (recommended)

```bash
pip install pygtide
```

Pre-compiled wheels are provided for Linux, macOS and Windows on Python 3.12 and newer — no compiler required. On Python 3.10 and 3.11, pip automatically builds from the source distribution instead, which requires a Fortran compiler (see Option 2). NumPy and pandas are installed automatically as dependencies.

To upgrade an existing installation:

```bash
pip install -U pygtide
```

#### Option 2: Build from source locally (Linux, macOS, Windows; Python &gt;= 3.10)

**Requirements for building:**
- A Fortran compiler (e.g., `gfortran`, via `conda install gfortran` or your system package manager)
- Meson and ninja (installed automatically by pip)

**Clone the repository:**

```bash
git clone https://github.com/hydrogeoscience/pygtide.git
```

**Install from the local repository:**

```bash
cd pygtide
pip install .
```

### After installation

* Run tests to verify installation:
  ```
  python -c "import pygtide; pygtide.test(msg=True)"
  ```

* Update internal database files (downloads latest leap seconds and pole data):
  ```
  python -c "import pygtide; pygtide.update()"
  ```

### Example usage

See `pygtide/tests.py` for complete examples. Quick start:

```python
from pygtide import predict_series
args = (-20.82071, -70.15288, 830.0, '2020-01-01', 6, 600)
series = predict_series(*args, statazimut=90, tidalcompo=8)
```

## How to use

An updated user guide is currently in progress ...

### Note on the absolute scale of the output

PyGTide follows the ETERNA PREDICT convention: tidal potential, gravity and
tilt (`tidalcompo=-1, 0, 1`) are rigid-Earth (geometric) tides. The body-tide
amplitude factors of the Wahr–Dehant–Zschau model (gravimetric factor δ≈1.16,
1+k≈1.30 for the potential, tilt factor γ=1+k−h≈0.69) are divided out for the
main wave of each wave group and only modulate the relative amplitudes of the
satellite waves within a group (including the NDFW resonance in the diurnal
band). To obtain body-tide gravity or tilt, set the wave-group amplitude
factor accordingly, e.g. for gravity:

```python
pt.set_wavegroup(np.asarray([[0, 10, 1.16, 0]]))
```

to set multiple wave group parameters

```python
DC = np.array([0.000000, 0.000001, 1.000000, 0.0000])
Long = np.array([0.000002, 0.249951, 1.160000, 0.0000])
Q1 = np.array([0.721500, 0.906315, 1.154250, 0.0000])
O1 = np.array([0.921941, 0.974188, 1.154240, 0.0000])
P1 = np.array([0.989049, 0.998028, 1.149150, 0.0000])
K1 = np.array([0.999853, 1.216397, 1.134890, 0.0000])
N2 = np.array([1.719381, 1.906462, 1.161720, 0.0000])
M2 = np.array([1.923766, 1.976926, 1.161720, 0.0000])
S2 = np.array([1.991787, 2.002885, 1.161720, 0.0000])
K2 = np.array([2.003032, 2.182843, 1.161720, 0.0000])
M3 = np.array([2.753244, 3.081254, 1.07338, 0.0000])
other = np.array([3.791964, 3.937897, 1.03900, 0.0000])

pt.set_wavegroup(wavedata=np.vstack((DC, Long, Q1, O1, P1, K1, N2, M2, S2, K2, M3, other)))
```


In contrast, displacement and strain outputs (`tidalcompo=2...8`) already
include the Love numbers h and l, i.e. they are body tides. Keep this in mind
when combining components (e.g. gravity with displacement or strain).

## How to cite
If you use PyGTide, please cite the work as:

*Rau, Gabriel C. (2018) hydrogeoscience/pygtide: PyGTid. Zenodo. [https://doi.org/10.5281/zenodo.1346260](https://doi.org/10.5281/zenodo.1346260)*

## Example
<img src="https://raw.githubusercontent.com/hydrogeoscience/pygtide/master/earth_tide_example.png" width="500">
This image shows Earth tides calculated for the city Karlsruhe (Germany) in the year 2018.

## References
* Hartmann, T., and H.-G. Wenzel (1995), The HW95 tidal potential catalogue, Geophysical Research Letters, 22(24), 3553–3556, https://doi.org/10.1029/95GL03324.
* Kudryavtsev, S. M. (2004), Improved harmonic development of the Earth tide-generating potential, Journal of Geodesy, 17(12), 829-838, https://doi.org/10.1007/s00190-003-0361-2.
* Wenzel, H.-G. (1996), The nanogal software: Earth tide data processing package ETERNA 3.30, Bulletin d’Informations des Marées Terrestres, 124, 9425–9439.
* McMillan, T. C., and Rau, G. C., and Timms, W. A., and Andersen, M. S. (2019), Utilizing the impact of Earth and atmospheric tides on groundwater systems: A review reveals the future potential, Reviews of Geophysics, https://dx.doi.org/10.1029/2018RG000630.

## License
PyGTide is released by [Gabriel C. Rau](https://hydrogeo.science), [Tom Eulenfeld](https://scholar.google.com/citations?user=SJXF3mwAAAAJ&hl=en) and [Craig Miller](https://www.gns.cri.nz/about-us/staff-search/craig-miller/) under the [Mozilla Public License 2.0](https://www.mozilla.org/en-US/MPL/2.0/)
