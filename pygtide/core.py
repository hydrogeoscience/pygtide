"""
PyGTide - A python class to calculate time series of the gravitational tides on Earth
-------------------------------------------------------------------------------
Author: Gabriel C. Rau (gabriel@hydrogeo.science)
Website: https://hydrogeo.science
Based on:
    ETERNA 3.4 PREDICT (by Prof. Wenzel, 1996)
    Updated updated by Kudryevtsev (2004)

Publications:
    Wenzel, H.-G. The nanogal software: Earth tide data processing package ETERNA 3.30,
        Bull. Inf. Marées Terrestres (1996) 124, 9425–9439.
    Kudryavtsev, S. Journal of Geodesy (2004) 77: 829. https://doi.org/10.1007/s00190-003-0361-2
-------------------------------------------------------------------------------
How to run:
    import pygtide
    pt = pygtide.pygtide()
    pt.predict(latitude, longitude, height, startdate, duration, samprate, **control):
    data = pt.results()

pt.results() returns:
    either False, or a Pandas dataframe with the respective data.

Python status messages can be suppressed by setting: self.msg = False.
The routine relies on files in the subdirectory 'commdat'. This includes the tidal
catalogues, corrections of the pole rotation as well as leap seconds.
Wave group parameters are being read from the "self.waves.ini" file.
-------------------------------------------------------------------------------
The original code was written in Fortran 77/90 and is available for download from:
http://igets.u-strasbg.fr/soft_and_tool.php
-------------------------------------------------------------------------------
Subroutines used in the Fortran program:
--- new ---
PREDICT: New Python interface subroutine to hand over arguments and calculate
INIT: New subroutine which sets the variables in module out for access in Python.
--- existing ---
ETASTN: computes astronomical elements.
PREDIN: reads control parameters.
ETDDTA: reads tabel of DDT = TDT - UTC.
ETDDTB: interpolates   DDT = TDT - UTC from table.
ETGCON: computes geodetic coefficients.
ETGREN: computes date from Julian date.
ETJULN: computes JULIAN date.
ETLEGN: computes fully normalized Legendre spherical harmonics.
ETLOVE: computes elastic parameters from Wahr-Dehant model.
ETPOLC: computes DUT1.
ETPHAS: computes the phases and frequencies of the tidal waves.
ETPOTS: computes amplitudes, frequencies and phases of tidal waves.
GEOEXT: computes JOBTIME.
===============================================================================
Note: While Prof. Wenzel passed away, his legacy continues to live as PyGTide
-------------------------------------------------------------------------------
      #######    ########    #######     #######      ##    ##        ####
      ##            ##       ##          ##    ##     ###   ##       ##  ##
      ##            ##       ##          ##    ##     ####  ##      ##    ##
      ######        ##       ######      #######      ## ## ##      ##    ##
      ##            ##       ##          ##  ##       ##  ####      ########
      ##            ##       ##          ##   ##      ##   ###      ##    ##
      #######       ##       #######     ##    ##     ##    ##      ##    ##

      Prof. Dr.-Ing. Hans-Georg Wenzel
      Black Forest Observatory
      Universitaet Karlsruhe
      Englerstr. 7
      D-76128 KARLSRUHE
      Germany
      Phone   : ++49-0721-6082307
      Telefax : ++49-0721-694552
      e-mail  : wenzel@gik.bau-verm.uni-karlsruhe.de
===============================================================================
This Python module was created based on the Fortran code PREDICT as part of
ETERNA 3.4, originally written by Prof. Hans George Wenzel in 1996. PREDICT is used to
calculate Earth tide gravity time series. The original PREDICT Fortran code was
updated to implement the new tidal catalogue by Kudryatvtsev (2004). The code
was then modernised (Fortran 90) for compilation as Python 3 module. This interface
provides a convenient way to utilise ETERNA PREDICT within Python.

The module relies on external files in the directory "commdat". The folowing files require
regular updating:

- etddt.dat - contains the difference between ephemeris time and UTC (include any leap seconds)
- etpolut.dat - contains the earth's pole rotation

The original Fortran code was also modified for use with f2py:
- COMMON blocks were transformed into modules
- continuous lines were updated for F90 compatibility
- the main program was changed into a subroutine (for f2py compliance)
- various other modernisations and enhancements
- BUG FIX: the original date and time data contained a rounding bug when the
    sampling rate was lower than 60 seconds. This was successfully fixed.
===============================================================================
"""
import pygtide.etpred as etpred
from datetime import datetime, timedelta, date
from warnings import warn
import numpy as np
import pandas as pd
from pkg_resources import resource_filename
import os

class pygtide(object):
    """
    The PyGTide class will initialise internal variables
    """
    def __init__(self, msg=True):
        """
        pygtide.init() initialises the etpred (Fortran) module and sets global variables
        """
        self.msg = msg
        self.version = 'PyGTide v0.6'
        self.exectime = 0
        self.fortran_version = etpred.inout.vers.astype(str)
        self.data_dir = resource_filename('pygtide', 'commdat/')
        etpred.params.comdir = self.data_dir + ' ' * (256 - len(self.data_dir))
        # set OS dependent module output
        etpred.params.nullfile = os.devnull + ' ' * (10 - len(os.devnull))
        self.args = []
        # set some common variables for external access
        etpred.init()
        # capture end date of file "etddt.dat" from module
        year = int(etpred.inout.etd_start)
        self.etddt_start = datetime(year, 1, 1)
        year = etpred.inout.etd_end
        self.etddt_end = (datetime(int(year), 1, 1) + timedelta(days=(year - int(year)) * 365))
        # capture end date of file "etpolut1.dat" from module
        self.etpolut1_start = datetime.strptime(str(etpred.inout.etpol_start), "%Y%m%d")
        self.etpolut1_end = datetime.strptime(str(etpred.inout.etpol_end), "%Y%m%d")

        self.headers = np.char.strip(etpred.inout.header.astype('str'))
        # self.units = ['(m/s)**2','nm/s**2','mas','mm','mm','nstr','nstr','nstr','nstr','nstr','mm']
        self.exec = False

        self.wavegroup_def = np.asarray([[0, 10, 1., 0.]])
        self.set_wavegroup(self.wavegroup_def)

    #%% sync the Python object
    def update(self):
        """
        self.update() refreshes the variables of PyGTide based on the Fortran module etpred
        """
        self.exectime = etpred.inout.exectime
        self.headers = np.char.strip(etpred.inout.header.astype('str'))
        self.args = etpred.inout.argsin
        self.unit = etpred.inout.etpunit.astype('str')

    #%% set wave group parameters
    def set_wavegroup(self, wavedata=None):
        if (wavedata is None):
            wavedata = self.wavegroup_def
        # require at least 4 columns
        if (wavedata.shape[1] != 4):
            raise ValueError("The wave group input must have 4 columns!")
            return False
        # require frequency ranges to increase and not overlap
        freq_diffs = np.diff(wavedata[:, 0:1].flatten())
        if ((freq_diffs < 0).any()):
            raise ValueError("Wave group frequency ranges must be increasing and not overlapping!")
            return False
        if ((wavedata[:, 2] < 0).any()):
            raise ValueError("Amplitude factors must be positive!")
            return False
        # set the wave group parameters
        etpred.waves(wavedata[:, 0], wavedata[:, 1], wavedata[:, 2], wavedata[:, 3], int(wavedata.shape[0]))
        return True

    #%% reset the wave group
    def reset_wavegroup(self):
        self.set_wavegroup(self.wavegroup_def)
        return True

    # run module etpred and return numbers
    def predict(self, latitude, longitude, height, startdate, duration, samprate, **control):
        """
        self.predict(latitude, longitude, height, startdate, duration, samprate, **control):
        -------------------------------------------------------------------------------
        Explanation of parameters used as numeric array "argsin". Parameters which are set
        will overwrite default control parameters (ETERNA ini file input is disabled).
        -------------------------------------------------------------------------------
        Required parameters:
        ---
         Latitude     Ellipsoidal latitude of the station in degree referring to
                      WGS84 reference system (ETERNA: STATLATITU).
         Longitude    Ellipsoidal longitude of the station in  degree  referring
                      to WGS84 reference system (ETERNA: STATLONITU).
         Height       Ellipsoidal height of the station in meters referring to
                      WGS84 reference system (ETERNA: STATELEVAT).
         Startdate    Initial epoch, used to compute the Fourier development  of
                      the specific earth tide component. Format is either string
                      'YYYY-MM-DD' or a datime object (ETERNA: INITIALEPO).
         Duration     Time span for the  prediction  in  hours. The model tide series
                      will start at the  initial epoch INITIALEPO and the  time  span
                      will  be  PREDICSPAN  hours (ETERNA: PREDICSPAN).
         Samprate     Data sample interval in seconds (ETERNA: SAMPLERATE).
        -------------------------------------------------------------------------------
        Optional keyword (**control) parameters:
        ---
          statgravit= Gravity of the station in m/s^2, necessary for tidal tilt
                      only.  If  the gravity is unknown, use a value of less than 1.0
                      and  the program will  compute  and  subsequently  use  the
                      normal gravity value referring to GRS80 reference system.
          statazimut= azimuth of the  instrument  in  degree  decimal,  reckoned
                      clockwise from north. This parameter is used for tidal tilt,
                      horizontal displacement and horizontal strain only.
          tidalpoten= Parameter for the tidal potential catalog to be used:
                      1 = Doodson (1921) tidal potential catalog,
                      2 = Cartwright-Tayler-Edden (1973) tidal potential catalog
                      3 = Buellesfeld (1985) tidal potential catalog,
                      4 = Tamura (1987) tidal potential catalog,
                      5 = Xi (1989) tidal potential catalog,
                      6 = Roosbeek (1996) tidal potential catalog,
                      7 = Hartmann and Wenzel (1995) tidal potential catalog.
                      8 = (default) Kudryavtsev (2004) tidal potential catalog.
          tidalcompo= Earth tide component:
                      = -1 for tidal potential (m**2/s**2)
                      =  0 (default) for tidal gravity in (nm/s**2)
                      =  1 for tidal tilt (mas), at azimuth STATAZIMUT.
                      =  2 for tidal vertical displacement (mm)
                      =  3 for tidal horizontal displacement (mm) azimuth STATAZIMUT.
                      =  4 for tidal vertical  strain (10**-9 = nstr)
                      =  5 for tidal horizontal strain (10**-9 = nstr) azimuth STATAZIMUT.
                      =  6 for tidal areal  strain (10**-9 = nstr)
                      =  7 for tidal shear  strain (10**-9 = nstr)
                      =  8 for tidal volume strain (10**-9 = nstr)
                      The computed model  tides  will  be  given  in  the  units defined above.
          amtruncate= Amplitude threshold (default 0) for the tidal potential  catalogue (m^2/s^2).
                      Only tidal waves with amplitudes exceeding  the
                      amplitude threshold are used  for  the  computation.  This
                      reduces the execution time, but also the accuracy  of  the
                      computed tidal signales. For mean latitudes, the  relation
                      between amplitude threshold and gravity tide accuracy   is
                      for the Hartmann and Wenzel (1995) tidal potential catalog
                               threshold              rms error [nm/s^2]
                                 1.D-01                88.40
                                 1.D-02                14.40
                                 1.D-03                 2.250
                                 1.D-04                 0.440
                                 1.D-05                 0.068
                                 1.D-06                 0.011
                                 1.D-07                 0.002
                                 1.D-08                 0.001
                                 1.D-09                 0.001
                                 1.D-10                 0.001
          poltidecor= Amplitude factor for gravity pole  tide. If  the amplitude
                      factor is greater zero, gravity pole tides will be  computed using
                      the IERS daily pole  coordinates. Default value is 1.16.
          lodtidecor= Amplitude factor for gravity LOD tide.  If  the  amplitude
                      factor is greater zero, gravity LOD tides will be computed
                      using  the IERS  daily  pole  coordinates. Default value is 1.16.
          fileout=    Defaults value is 0 (output is suppressed). If set to 1, the routine
                      writes two text files called "self.inout.prd" and "self.inout.prn"
                      in the original format into the directory of the module.
          screenout=  Defaults value is 0 (output is silenced). If set to 1, the routine
                      writes output to the screen (but not the Python terminal).
        -------------------------------------------------------------------------------
        """
        # prepare full input argument array
        argsin = np.zeros(18)
        # define default values as given by the Fortran code
        # tidal catalog
        argsin[10] = 8
        # amplitude truncation
        argsin[12] = 1.0E-10
        # values from: https://dx.doi.org/10.1016/j.jog.2005.08.035
        argsin[13] = 1.16
        argsin[14] = 1.16

        # iterate through optional arguments passed
        if 'statgravit' in control:
            if not (0 <= control['statgravit'] <= 20):
                raise ValueError('Station gravity exceeds permissible range!')
            else:
                argsin[8] = control['statgravit']
        if 'statazimut' in control:
            if not (0 <= control['statazimut'] <= 180):
                raise ValueError('Statazimut exceeds permissible range!')
            else:
                argsin[9] = control['statazimut']
        if 'tidalpoten' in control:
            if control['tidalpoten'] not in range(1,9):
                raise ValueError('Tidalpoten must be an integer between 1 and 8!')
            else:
                argsin[10] = control['tidalpoten']
        if 'tidalcompo' in control:
            if control['tidalcompo'] not in range(-1,10):
                raise ValueError('Tidalcompo must be an integer between -1 and 9!')
            else:
                argsin[11] = control['tidalcompo']
        if 'amtruncate' in control:
            if not (0 <= control['amtruncate']):
                raise ValueError('Amtruncate must be greater than 0!')
            else:
                argsin[12] = control['amtruncate']
        if 'poltidecor' in control:
            if not (control['poltidecor'] >= 0):
                raise ValueError('Poltidecor must be >= 0!')
            else:
                argsin[13] = control['poltidecor']
        if 'lodtidecor' in control:
            if not (control['lodtidecor'] >= 0):
                raise ValueError('Lodtidecor must be >= 0!')
            else:
                argsin[14] = control['lodtidecor']
        # additional control parameters
        if 'fileprd' in control:
            if control['fileprd'] not in range(0,2):
                raise ValueError('Fileprd flag must be 0 or 1!')
            else:
                argsin[15] = control['fileprd']
        if 'fileprn' in control:
            if control['fileprn'] not in range(0,2):
                raise ValueError('Fileprn flag must be 0 or 1!')
            else:
                argsin[16] = control['fileprn']
        if 'screenout' in control:
            if control['screenout'] not in range(0,2):
                raise ValueError('Screenout flag must be 0 or 1!')
            else:
                argsin[17] = control['screenout']
        # process required parameters here
        if not (-90 <= latitude <= 90):
            raise ValueError('Latitude exceeds permissible range!')
        else:
            argsin[0] = latitude
        if not (-180 <= longitude <= 180):
            raise ValueError('Longitude exceeds permissible range!')
        else:
            argsin[1] = longitude
        if not (-500 <= height <= 5000):
            raise ValueError('Height exceeds permissible range!')
        else:
            argsin[2] = height
        if not (0 <  duration <= 10*24*365):
            raise ValueError("Duration exceeds permissible range!")
        else:
            argsin[6] = int(duration)

        # test startdate format and validity
        if not (isinstance(startdate, date)):
            try:
                startdate = datetime.strptime(startdate, "%Y-%m-%d")
            except ValueError:
                raise ValueError("Startdate has incorrect format (YYYY-MM-DD)!" )
        enddate = startdate + timedelta(hours=duration)
        # check if requested prediction series exceeds permissible time
        if (startdate < self.etddt_start):
            fname = str(etpred.params.etddtdat)
            warn("Prediction timeframe is earlier than the available time database (%s). "
                 "For details refer to the file '%s'." % (self.etddt_start, fname))
        if (enddate > (self.etddt_end + timedelta(days=365))):
            fname = str(etpred.params.etddtdat)
            warn("Please consider updating the leap second database '%s' (last value is from %s)." % (fname, self.etddt_end))
        # if not (-50*365 < (startdate - dt.datetime.now()).days < 365):
        if ( ((argsin[13] > 0) or (argsin[14] > 0)) and ((startdate < self.etpolut1_start) or (enddate > self.etpolut1_end)) ):
            fname = str(etpred.params.etddtdat)
            warn("Dates exceed permissible range for pole/LOD tide correction (interval %s to %s). Consider update file '%s'." % (self.etpolut1_start, self.etpolut1_end, fname))
        if ( ((argsin[13] > 0) or (argsin[14] > 0)) and (startdate < datetime.strptime('1600-01-01', "%Y-%m-%d")) ):
             raise ValueError("PyGTide should not be used for dates before the year 1600.")
        # set the start date and time
        argsin[3:6] = [startdate.year,startdate.month,startdate.day]
        # test sammprate validity
        if not (0 < samprate <= 24*3600):
            raise ValueError("samprate exceeds permissible range!")
        else:
            argsin[7] = int(samprate)
        # test that samprate is not larger than duration
        if (samprate/3600 >  duration):
            raise ValueError("samprate exceeds duration!")
        # ####################################################
        # BUGFIX: fix a weird bug where the program stops before
        # the etpdata table is filled completely
        argsin[6] = duration + 1
        # run prediction routine
        # ######################################################
        # print(argsin)
        self.args = argsin
        if self.msg:
            print('%s is calculating, please wait ...' % (self.fortran_version))

        etpred.predict(argsin)

        self.exec = True
        self.exectime = etpred.inout.exectime
        if self.msg:
            print('Done after %.3f s.' % (self.exectime))
        self.update()
        return True

    # easy access to the raw data calculated by the Fortran module
    def results(self, digits=6):
        """
        self.results(digits=6)
        Returns:
            - If predict() was executed, returns a dataframe with the results
            - False
        keyword 'digits' sets the number of digits returned.
        """
        if self.exec:
            # format date and time into padded number strings
#            print(etpred.inout.etpdata[:,1])
            date = np.char.mod("%08.0f ", etpred.inout.etpdata[:,0])
            time = np.char.mod("%06.0f", etpred.inout.etpdata[:,1])
            # merge date and time arrays
            datetime = np.core.defchararray.add(date, time)
            # get the headers from Fortran
            cols = np.char.strip(etpred.inout.header.astype('str'))
            allcols = np.insert(cols[2:], 0, 'UTC')
            etdata = pd.DataFrame(columns=allcols)
            etdata['UTC'] = pd.to_datetime(datetime, format="%Y%m%d %H%M%S", utc=True)
            # obtain header strings from routine and convert
            etdata[cols[2:]] = np.around(etpred.inout.etpdata[:, 2:], digits)
            return etdata
        else:
            return None

    # easy access to the raw data calculated by Fortran
    def raw(self):
        """
        self.raw()
        Returns:
            - If predict() was executed, returns the raw data from the etpred module
            - False
        """
        if self.exec:
            return etpred.inout.etpdata
        else:
            return None

    # easy access to the formatted data calculated by Fortran
    def data(self, digits=6):
        """
        self.data(digits=6):
        Returns:
            - If predict() was executed, returns a numpy array with the results
            - False
        keyword 'digits' sets the number of digits returned.
        """
        if self.exec:
            return np.around(etpred.inout.etpdata[:, 2:], digits)
        else:
            return None

    # easy access to the raw datetime calculated by Fortran
    def datetime(self):
        """
        self.datetime():
        Returns:
            - If predict() was executed, returns a numpy string array with the
            calculated dates and times in seperate columns
            - False
        """
        if self.exec:
            # reformat the date and time values obtained from ETERNA
            date = np.char.mod("%08.0f", etpred.inout.etpdata[:,0])
            time = np.char.mod("%06.0f", etpred.inout.etpdata[:,1])
            return np.stack((date, time), axis=1)
        else:
            return None


def predict_table(*args, msg=False, **kwargs):
    kwargs.setdefault('screenout', int(msg))
    pt = pygtide(msg=msg)
    pt.predict(*args, **kwargs)
    return pt.results()


def predict_series(*args, msg=False, index=0, **kwargs):
    kwargs.setdefault('screenout', int(msg))
    pt = pygtide(msg=msg)
    pt.predict(*args, **kwargs)
    return pt.data()[:, index]


def predict_spectrum(*args, nfft=None, **kwargs):
    from numpy.fft import rfft, rfftfreq
    sr = args[-1]
    data = predict_series(*args, **kwargs)
    if nfft is None:
        nfft = len(data)
    freq = rfftfreq(nfft, sr)
    spec = rfft(data, nfft) * 2 / len(data)
    return freq, spec


def plot_series(*args, indices=(0, 1), show=True, **kwargs):
    table = predict_table(*args, **kwargs)
    table.plot(*indices)
    if show:
        import matplotlib.pyplot as plt
        plt.show()

def plot_spectrum(*args, ax=None, show=True, **kwargs):
    import matplotlib.pyplot as plt
    freq, spec = predict_spectrum(*args, **kwargs)
    if ax is None:
        ax = plt.subplot(111)
    ax.plot(freq * 24 * 3600, np.abs(spec))
    ax.set_xlabel('freq (cycles per day)')
    ax.set_ylabel('amplitude')
    if show:
        plt.show()
