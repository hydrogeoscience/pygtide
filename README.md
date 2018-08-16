# PyGTide
## A Python module and wrapper for ETERNA PREDICT to compute gravitational tides on Earth

The original ETERNA PREDICT 3.3 was written by the late Prof. H.-G. Wenzel (Wenzel, 1996). This was updated to include the latest tidal catalogue (Kudryavtsev, 2004). The original Fortran code can be found at: http://igets.u-strasbg.fr/soft_and_tool.php

## How to install and run
* Download and install [Anaconda 5.2+ for Python 3.6](https://www.anaconda.com/download/) (Windows 7/10 64bit)
* Use the Anaconda navigator to ensure that the packages *libpython* (as a minimum v2.1) and *mingw* (as a minimum v4.7) are installed
* Download [PyGTide](https://github.com/hydrogeoscience/pygtide) and run *test.py*

## Example
<img src="https://raw.githubusercontent.com/hydrogeoscience/pygtide/master/earth_tide_example.png" width="500">
This image shows Earth tides calculated for the city Karlsruhe (Germany) in the year 2018.

## References
* Wenzel, H.-G. (1996), The nanogal software: Earth tide data processing package ETERNA 3.30, Bulletin d’Informations des Marées Terrestres, 124, 9425–9439.
* Hartmann, T., and H.-G. Wenzel (1995), The HW95 tidal potential catalogue, Geophysical Research Letters, 22(24), 3553–3556, https://doi.org/10.1029/95GL03324.
* Kudryavtsev, S. M. (2004), Improved harmonic development of the Earth tide-generating potential, Journal of Geodesy, 17(12), 829-838, https://doi.org/10.1007/s00190-003-0361-2.

## License
PyGTide is released under the [Mozilla Public License 2.0](https://www.mozilla.org/en-US/MPL/2.0/)
