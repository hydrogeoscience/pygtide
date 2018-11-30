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
import numpy as np
import pandas as pd
import datetime as dt
from etpred import etpred
import os
from sys import path
from pygtide.pygtide_update_data import etddt, etpolut1

WORKING_DIR = os.getcwd()
ETPRED_DIR = os.path.dirname(etpred.__file__)

class pygtide(object):
    """
    The PyGTide class will initialise internal variables
    """
    def __init__(self, msg=True):
        """
        pygtide.init() initialises the etpred (Fortran) module and sets global variables
        """
        self.msg = msg

        # Move to etpred directory
        os.chdir(ETPRED_DIR)

        # set some common variables for external access
        etpred.init()
        self.version = 'PyGTide v0.1'
        self.exectime = 0
        self.fortran_version = etpred.inout.vers.astype(str)
        #print(str(etpred.params.comdir, 'UTF-8').strip())
        self.data_dir = str(etpred.params.comdir, 'UTF-8').strip() + str(etpred.params.pathsep, 'UTF-8').strip()
        self.args = []

        #%% capture end date of file "etddt.dat" from module
        self.etddt_file = str(etpred.params.etddtdat, 'UTF-8').strip()
        year = int(etpred.inout.etd_start)
        # leap year is missing
        d = dt.timedelta(days=(etpred.inout.etd_start - year)*365)
        self.etddt_start = d + dt.datetime(year,1,1)
        year = int(etpred.inout.etd_end)
        # leap year is missing
        d = dt.timedelta(days=(etpred.inout.etd_end - year)*365)
        self.etddt_end = d + dt.datetime(year,1,1)

        #%% capture end date of file "etpolut1.dat" from module
        self.etpolut1_dat_file = str(etpred.params.etpolutdat, 'UTF-8').strip()
        self.etpolut1_bin_file = str(etpred.params.etpolutbin, 'UTF-8').strip()
        self.etpolut1_start = dt.datetime.strptime(str(etpred.inout.etpol_start), "%Y%m%d")
        self.etpolut1_end = dt.datetime.strptime(str(etpred.inout.etpol_end), "%Y%m%d")

        # print(end_date)
        self.headers = np.char.strip(etpred.inout.header.astype('str'))
        # self.units = ['(m/s)**2','nm/s**2','mas','mm','mm','nstr','nstr','nstr','nstr','nstr','mm']
        self.is_init = True
        self.exec = False

        #%% remote data files
        # IERS leap seconds history file
        self.leapsec_rfile = 'https://hpiers.obspm.fr/iers/bul/bulc/Leap_Second_History.dat'
        self.iauhist_rfile = 'http://hpiers.obspm.fr/iers/eop/eopc04/eopc04_IAU2000.62-now'
        self.iaucurr_rfile = 'http://maia.usno.navy.mil/ser7/finals2000A.data'

        # Move back to original working directory
        os.chdir(WORKING_DIR)

    def update(self):
        """
        self.update() refreshes the variables of PyGTide based on the Fortran module etpred
        """
        self.exectime = etpred.inout.exectime
        self.headers = np.char.strip(etpred.inout.header.astype('str'))
        self.args = etpred.inout.argsin
        self.unit = etpred.inout.etpunit.astype('str')

    #%% run module etpred and return numbers
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
        # initialise the module to make variables accessible
        if not self.is_init: self.__init__()
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

        #%% iterate through optional arguments passed
        # print(control)
        # check statgravit validity
        if 'statgravit' in control:
            if not (0 <= control['statgravit'] <= 20):
                raise ValueError('Station gravity exceeds permissible range!')
                return False
            else:
                argsin[8] = control['statgravit']
        # check statgravit validity
        if 'statazimut' in control:
            if not (0 <= control['statazimut'] <= 180):
                raise ValueError('Statazimut exceeds permissible range!')
                return False
            else:
                argsin[9] = control['statazimut']
        # check tidalpoten validity
        if 'tidalpoten' in control:
            if control['tidalpoten'] not in range(1,9):
                raise ValueError('Tidalpoten must be an integer between 1 and 8!')
                return False
            else:
                argsin[10] = control['tidalpoten']
        # check tidalcompo validity
        if 'tidalcompo' in control:
            if control['tidalcompo'] not in range(-1,10):
                raise ValueError('Tidalcompo must be an integer between -1 and 9!')
                return False
            else:
                argsin[11] = control['tidalcompo']
        # check amtruncate validity
        if 'amtruncate' in control:
            if not (0 <= control['amtruncate']):
                raise ValueError('Amtruncate must be greater than 0!')
                return False
            else:
                argsin[12] = control['amtruncate']
        # check poltidecor validity
        if 'poltidecor' in control:
            if not (control['poltidecor'] >= 0):
                raise ValueError('Poltidecor must be >= 0!')
                return False
            else:
                argsin[13] = control['poltidecor']
        # check lodtidecor validity
        if 'lodtidecor' in control:
            if not (control['lodtidecor'] >= 0):
                raise ValueError('Lodtidecor must be >= 0!')
                return False
            else:
                argsin[14] = control['lodtidecor']
        #%% additional control parameters
        # check fileout validity
        if 'fileprd' in control:
            if control['fileprd'] not in range(0,2):
                raise ValueError('Fileprd flag must be 0 or 1!')
                return False
            else:
                argsin[15] = control['fileprd']
        # check fileout validity
        if 'fileprn' in control:
            if control['fileprn'] not in range(0,2):
                raise ValueError('Fileprn flag must be 0 or 1!')
                return False
            else:
                argsin[16] = control['fileprn']
        # check fileout validity
        if 'screenout' in control:
            if control['screenout'] not in range(0,2):
                raise ValueError('Screenout flag must be 0 or 1!')
                return False
            else:
                argsin[17] = control['screenout']
        #%% process required parameters here
        # test latitude validity
        if not (-90 <= latitude <= 90):
            raise ValueError('Latitude exceeds permissible range!')
            return False
        else:
            argsin[0] = latitude
        # test longitude validity
        if not (-180 <= longitude <= 180):
            raise ValueError('Longitude exceeds permissible range!')
            return False
        else:
            argsin[1] = longitude
        # test height validity
        if not (-500 <= height <= 5000):
            raise ValueError('Height exceeds permissible range!')
            return False
        else:
            argsin[2] = height
        # test duration validity
        if not (0 <  duration <= 10*24*365):
            raise ValueError("Duration exceeds permissible range!")
            return False
        else:
            argsin[6] = int(duration)

        # test startdate format and validity
        if not (isinstance(startdate, dt.date)):
            try:
                startdate = dt.datetime.strptime(startdate, "%Y-%m-%d")
            except ValueError:
                raise ValueError("Startdate has incorrect format (YYYY-MM-DD)!" )
                return False
        enddate = startdate + dt.timedelta(hours=duration)
        # check if requested prediction series exceeds permissible time
        if (startdate < self.etddt_start):
            file = np.char.strip(etpred.params.etddtdat.astype('str'))
            if (self.msg): print("Prediction timeframe is earlier than the available time database (%s). " \
                  "For details refer to the file '%s'." % (self.etddt_start, file))
        if (enddate > (self.etddt_end + dt.timedelta(days=365))):
            file = np.char.strip(etpred.params.etddtdat.astype('str'))
            if (self.msg): print("Prediction timeframe exceeds the end of the available time database (%s) plus 1 year. " \
                  "For best accuracy, please consider updating '%s'." % (self.etddt_end, file))
        # if not (-50*365 < (startdate - dt.datetime.now()).days < 365):
        if ( ((argsin[13] > 0) or (argsin[14] > 0)) and ((startdate < self.etpolut1_start) or (enddate > self.etpolut1_end)) ):
            file = np.char.strip(etpred.params.etpolutdat.astype('str'))
            raise ValueError("Dates exceed permissible range for pole/LOD tide correction (interval %s to %s). "\
                "Please update file '%s'." % (self.etpolut1_start, self.etpolut1_end, file))
            return False
        # set the start date and time
        argsin[3:6] = [startdate.year,startdate.month,startdate.day]
        # test sammprate validity
        if not (0 < samprate <= 24*3600):
            raise ValueError("Samprate exceeds permissible range!")
            return False
        else:
            argsin[7] = int(samprate)
        # test that samprate is not larger than duration
        if (samprate/3600 >  duration):
            raise ValueError("Samprate exceeds duration!")
            return False
        # ####################################################
        # BUGFIX: fix a weird bug where the program stops before
        # the etpdata table is filled completely
        argsin[6] = duration + 1
        #%% run prediction routine
        # ######################################################
        # print(argsin)
        self.args = argsin
        if (self.msg): print('%s is calculating, please wait ...' % (self.fortran_version))

        # Move to etpred directory
        os.chdir(ETPRED_DIR)

        # run predict
        etpred.predict(argsin)

        # Move back to original working directory
        os.chdir(WORKING_DIR)

        self.exec = True
        self.exectime = etpred.inout.exectime
        if (self.msg): print('Done after %.3f s.' % (self.exectime))
        self.update()
        return True

    #%% easy access to the raw data calculated by the Fortran module
    def results(self, round=6):
        """
        self.results(round=6)
        Returns:
            - If predict() was executed, returns a dataframe with the results
            - False
        keyword 'round' sets the number of digits returned.
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
            etdata[cols[2:]] = np.around(etpred.inout.etpdata[:, 2:],round)
            return etdata
        else:
            return False

    #%% easy access to the raw data calculated by Fortran
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
            return False

    #%% easy access to the formatted data calculated by Fortran
    def data(self, round=6):
        """
        self.data(round=6):
        Returns:
            - If predict() was executed, returns a numpy array with the results
            - False
        keyword 'round' sets the number of digits returned.
        """
        if self.exec:
            return np.around(etpred.inout.etpdata[:, 2:], round)
        else:
            return False

    #%% easy access to the raw datetime calculated by Fortran
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
            return False

    #%% update the time conversion database (leap seconds)
    def update_etddt(self):
        # import update routines
        etddt(self.data_dir, self.etddt_file, self.leapsec_rfile)

    #%% update the pole coordinates and UT1 to TAI times
    def update_etpolut1(self):
        # import update routines
        data_dir = os.path.join(ETPRED_DIR, self.data_dir)
        etpolut1(data_dir, self.etpolut1_dat_file, self.leapsec_rfile, self.iauhist_rfile, self.iaucurr_rfile)
        # refresh bin file also
        self.etpolut1_dat2bin()

    #%% update the etpolut1 binary file from the text file
    def etpolut1_dat2bin(self):
        data_dir = os.path.join(ETPRED_DIR, self.data_dir)
        etpolut1_dat = data_dir + '\\' + self.etpolut1_dat_file
        etpolut1_bin = data_dir + '\\' + self.etpolut1_bin_file
        header = []
        # find the end of the header
        with open(etpolut1_dat, "r") as f:
            for num, line in enumerate(f, 1):
                header.append(line)
                if "C*******" in header[-1]: break

        # read into dataframe
        cols = ['Date', 'Time', 'MJD', 'x', 'y', 'UT1-UTC', 'TAI-UT1']
        etpolut = pd.read_csv(etpolut1_dat, names=cols, skiprows=num, header=None, delimiter=r"\s+")
        # drop the last row with EOL ('99999999')
        etpolut = etpolut[:-1]
        print("File '{:s}' has {:d} rows.".format(etpolut1_dat, etpolut.shape[0]))
        #%%
        # write as binary for use in fortran
        # in fortran, each record has 4 * 8 bytes = 32
        # header contains start date in MJD and number of rows + 1
        head = np.array([int(etpolut.iloc[0, 2]), int(etpolut.shape[0]+1)])
        data = etpolut.values[:, 3:]
        #print(data)
        with open(etpolut1_bin,'wb+') as f:
            # write header integers
            f.write(head.tobytes())
            # advance to next record (32 bytes)
            f.seek(32)
            # write the flattened matrix
            f.write(data.flatten().tobytes())
        f.close()
        print("File '{:s}' has been updated (Header: {:.0f}, {:d}).".format(etpolut1_bin, etpolut.iloc[0, 2], etpolut.shape[0]+1))

# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# FUNCTIONS BELOW THIS LINE ARE UNDER DEVELOPMENT AND THEREFORE EXPERIMENTAL
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    def read_etpolut1_bin(self):
        with open('commdat\etpolut1.bin','rb') as f:
            header = np.fromfile(f, dtype=np.int, count=2)
            f.seek(32)
            data = np.fromfile(f, dtype=np.float64)

        print(header)
        print(data)
        print(data.shape)
        data = np.reshape(data, (-1, 4))
        print(data)
        print(data.shape)

    def read_etpolut1_dat(self):
        with open('commdat\etpolut1.dat', "rb") as f:
            first = f.readline()
            #print(first[0:10])
            while first[0:10] != b"C*********":
                first = f.readline()
                #print(first[0:10])
            first = f.readline()
            # Jump to the second last byte.
            f.seek(-12, os.SEEK_END)
            # Until EOL is found...
            while f.read(1) != b"\n":
                # ...jump back the read byte plus one more.
                f.seek(-2, os.SEEK_CUR)
            last = f.readline()
        # store dates
        self.etpolut1_start = dt.datetime.strptime(first[0:8].decode("utf-8"), "%Y%m%d")
        self.etpolut1_end = dt.datetime.strptime(last[0:8].decode("utf-8"), "%Y%m%d")

    def read_etddt_dat(self):
        with open('commdat\etddt.dat', "rb") as f:
            first = f.readline()
            while first[0:10] != b"C*********":
                first = f.readline()
            first = f.readline()
            # Jump to the second last byte.
            f.seek(-12, os.SEEK_END)
            # Until EOL is found...
            while f.read(1) != b"\n":
                # ...jump back the read byte plus one more.
                f.seek(-2, os.SEEK_CUR)
            last = f.readline()
        # store dates
        self.etddt_start = self.from_floatyear(float(first[0:10]))
        self.etddt_end = self.from_floatyear(float(last[0:10]))

    def from_floatyear(self,year):
        intyear = int(year)
        # leap year is missing
        d = dt.timedelta(days=(etpred.inout.etd_date - intyear)*365)
        return d + dt.datetime(intyear,1,1)

# end class pygtide
# pt = pygtide()
