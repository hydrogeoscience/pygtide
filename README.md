# PyGTide v0.5
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.1346260.svg)](https://doi.org/10.5281/zenodo.1346260)
## A Python module and wrapper for ETERNA PREDICT to compute gravitational tides on Earth

PyGTide is a Python class that wraps around ETERNA PREDICT 3.4 which was compiled from Fortran into a Python library using [f2py](https://docs.scipy.org/doc/numpy/f2py/). The original ETERNA PREDICT 3.3 was written by the late Prof. H.-G. Wenzel (Wenzel, 1996) in a mix of Fortran 77 and 90. This was updated by Kudryavtsev (2004) to include the latest tidal catalogue. The Fortran code for ETERNA PREDICT can be downloaded from the [International Geodynamics and
Earth Tide Service (IGETS)](http://igets.u-strasbg.fr/soft_and_tool.php).

## How to install and run
A user guide is available as [PDF download](https://github.com/hydrogeoscience/pygtide/raw/master/PyGTide_user-guide.pdf).

Instructions:
* Download and install [Anaconda for Python 3+](https://www.anaconda.com) (Windows 7/10 64bit or Linux (Ubuntu) 64bit)
* Use the *Anaconda Navigator* to ensure that the following standard libraries are installed: *numpy*, *pandas*, *datetime* and *astropy*.
* Download [PyGTide](https://github.com/hydrogeoscience/pygtide/), unzip into a local directory and run *test_pygtide_waves-all.py*

The module was compiled for Windows 7/10 and Linux (Ubuntu) and the Python versions 3.5, 3.6, 3.7 and 3.8 (64bit).

## How to cite
If you use PyGTide, please cite the work as:

*Rau, Gabriel C. (2020) hydrogeoscience/pygtide: PyGTide v0.5 (Version v0.5). Zenodo. [http://doi.org/10.5281/zenodo.4290320](http://doi.org/10.5281/zenodo.4290320)*

## Example
<img src="https://raw.githubusercontent.com/hydrogeoscience/pygtide/master/earth_tide_example.png" width="500">
This image shows Earth tides calculated for the city Karlsruhe (Germany) in the year 2018.

## References
* Hartmann, T., and H.-G. Wenzel (1995), The HW95 tidal potential catalogue, Geophysical Research Letters, 22(24), 3553–3556, https://doi.org/10.1029/95GL03324.
* Kudryavtsev, S. M. (2004), Improved harmonic development of the Earth tide-generating potential, Journal of Geodesy, 17(12), 829-838, https://doi.org/10.1007/s00190-003-0361-2.
* Wenzel, H.-G. (1996), The nanogal software: Earth tide data processing package ETERNA 3.30, Bulletin d’Informations des Marées Terrestres, 124, 9425–9439.
* McMillan, T. C., and Rau, G. C., and Timms, W. A., and Andersen, M. S. (2019), Utilizing the impact of Earth and atmospheric tides on groundwater systems: A review reveals the future potential, Reviews of Geophysics, https://dx.doi.org/10.1029/2018RG000630.

## License
PyGTide is released by [Gabriel C. Rau](https://hydrogeo.science) under the [Mozilla Public License 2.0](https://www.mozilla.org/en-US/MPL/2.0/)
