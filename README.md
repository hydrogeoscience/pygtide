# PyGTide
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.1346260.svg)](https://doi.org/10.5281/zenodo.1346260)

## A Python module and wrapper for ETERNA PREDICT to compute gravitational tides on Earth

PyGTide is a Python module that wraps around ETERNA PREDICT 3.4 which is compiled from Fortran into an executable using [f2py](https://docs.scipy.org/doc/numpy/f2py/). The original ETERNA PREDICT 3.3 was written by the late Prof. H.-G. Wenzel (Wenzel, 1996) in a mix of Fortran 77 and 90. This was updated by Kudryavtsev (2004) to include the latest tidal catalogue. Note that the original Fortran code was comprehensively revised in order to facilitate integration into Python. The original Fortran code for ETERNA PREDICT can be downloaded from the [International Geodynamics and Earth Tide Service (IGETS)](http://igets.u-strasbg.fr/soft_and_tool.php).

## How to use

There are two options:
* Download and install on your system (see below instructions)
* Via our online calculator: <a href="https://groundwater.app/app.php?app=pygtide" target="_blank">groundwater.app</a>


## How to install and run

Instructions:
* Download and install [*Anaconda*](https://www.anaconda.com/products/distribution) or [*Miniconda*](https://docs.conda.io/en/latest/miniconda.html)

* Make sure the following packages are installed: <br />
 `conda install numpy pandas requests git`
 
* Download and install pygtide:
  * *Linux* or *MacOS*: <br />
     **NOTE**: Make sure suitable C++ and Fortran compilers are available.
     ```
     pip install pygtide
     ```
   
  * *Windows*:<br />
       Download the correct wheel for your Python version (available for 3.8 to 3.11) from the subfolder "windows" onto your system.
	   Then navigate your Anaconda explorer to the download location and execute:
     ```
     pip install [wheel_name_depending_on_python_version]
     ```
   
* Run tests: <br /> 
 ```
 python -c "import pygtide; pygtide.test(msg=True)"
 ```
* The internal database files can be updated as follows: <br />
 ```
 python -c "import pygtide; pygtide.update()"
 ```
* See `pygtide/tests.py` for example usage:

```
from pygtide import predict_series
args = (-20.82071, -70.15288, 830.0, '2020-01-01', 6, 600)
series = predict_series(*args, statazimut=90, tidalcompo=8)
```

* Development version: This can be installed by downloading the Github repository and running:
 `pip install download_path`. <br />
  Alternatively, in one step as: <br />
  ```
  pip install git+https://github.com/hydrogeoscience/pygtide.git
  ```

## How to use

An updated user guide is currently in progress ...


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
PyGTide is released by [Gabriel C. Rau](https://hydrogeo.science) and [Tom Eulenfeld](https://scholar.google.com/citations?user=SJXF3mwAAAAJ&hl=en) under the [Mozilla Public License 2.0](https://www.mozilla.org/en-US/MPL/2.0/)
