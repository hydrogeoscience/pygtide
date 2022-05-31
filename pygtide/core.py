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
from datetime import datetime, timedelta, date
import numpy as np
from pkg_resources import resource_filename
from pygtide import etpred
import os
import pandas as pd
import re

def timestampToDecyear(ts):
    year=ts.year

    jan1=pd.Timestamp(year,1,1)

    jan1next=pd.Timestamp(year+1,1,1)
    yrlen=(jan1next-jan1).total_seconds()
    return year+(ts-jan1).total_seconds()/yrlen

class pygtide(object):
    """
    The PyGTide class will initialise internal variables
    """
    def __init__(self, msg=True):
        """
        pygtide.init() initialises the etpred (Fortran) module and sets global variables
        """
        self.msg = msg


        self.version = 'PyGTide v0.3'
        self.exectime = 0
        self.fortran_version = etpred.inout.vers.astype(str)
        self.data_dir = resource_filename('pygtide', 'commdat/')
        etpred.params.comdir = self.data_dir + ' ' * (256 - len(self.data_dir))
        etpred.params.nullfile = os.devnull + ' ' * (10 - len(os.devnull))
        self.args = []
        # set some common variables for external access
        etpred.init()
        # capture end date of file "etddt.dat" from module
        year = int(etpred.inout.etd_start)
        self.etddt_start = datetime(year, 1, 1)
        year = etpred.inout.etd_end
        self.etddt_end = (datetime(int(year), 1, 1) +
                          timedelta(days=(year - int(year)) * 365))
        # capture end date of file "etpolut1.dat" from module
        self.etpolut1_start = datetime.strptime(str(etpred.inout.etpol_start), "%Y%m%d")
        self.etpolut1_end = datetime.strptime(str(etpred.inout.etpol_end), "%Y%m%d")

        self.headers = np.char.strip(etpred.inout.header.astype('str'))
        # self.units = ['(m/s)**2','nm/s**2','mas','mm','mm','nstr','nstr','nstr','nstr','nstr','mm']
        self.exec = False


        # IERS leap seconds history file
        self.leapsec_rfile = 'https://hpiers.obspm.fr/iers/bul/bulc/Leap_Second_History.dat'
        self.iauhist_rfile = 'http://hpiers.obspm.fr/iers/eop/eopc04/eopc04_IAU2000.62-now'
        self.iaucurr_rfile = 'https://maia.usno.navy.mil/ser7/finals2000A.all'

        self.leapsec_file = os.path.join(self.data_dir,"[raw]_Leap_Second_History.dat")
        self.iauhist_file = os.path.join(self.data_dir,"[raw]_eopc04_IAU2000.dat")
        self.iaucurr_file = os.path.join(self.data_dir,"[raw]_finals2000A.dat")

        self.etpolut1_file = os.path.join(self.data_dir,str(etpred.params.etpolutdat,'utf-8').strip())
        self.etpolut1_bfile = os.path.join(self.data_dir,str(etpred.params.etpolutbin,'utf-8').strip())


        self.etddt_file = os.path.join(self.data_dir,str(etpred.params.etddtdat,'utf-8').strip())

    def update_data_files(self):
        """Download the newest leap Second data, and Earth Pole data and prediction files and convert them for use of pygtide"""
        import requests
        # IERS leap seconds history file
        for url,fout in [(self.leapsec_rfile,self.leapsec_file),(self.iauhist_rfile,self.iauhist_file),(self.iaucurr_rfile,self.iaucurr_file)]:
            # fout=os.path.join(self.data_dir,os.path.basename(url))\cc
            if self.msg:
                print(f"Downloading {fout}")
            r=requests.get(url)
            with open(fout,'wb') as fid:
                fid.write(r.content)

        #modify data
        self.update_etpolut1()
        self.update_etddt()
    def update_etpolut1(self):
        """Update the pole coordinate file with newest data and predictions"""
        #%% read leap second history
        dateparse = lambda x:datetime.strptime(x, '%d %m %Y')
        leapsdf = pd.read_csv(self.leapsec_file, comment='#', header=None, parse_dates= {'date':[1,2,3]}, \
                date_parser=dateparse, delimiter=r"\s+")
        leapsdf.columns = ['date', 'MJD', 'leaps']
        leapsdf.drop(['MJD'], axis=1, inplace=True)
        leapsdf.index += 1
        # insert row at the beginning
        leapsdf.loc[0] = [datetime(1962,1,1), 10]
        leapsdf.sort_index(inplace=True)
        #%% read historic pole coordinates
        # convert = {3: lambda x: np.around(np.float64(x), 3)}
        dateparse = lambda x: datetime.strptime(x, '%Y %m %d')
        iauhist = pd.read_csv(self.iauhist_file, skiprows=13, header=None, parse_dates= {'date':[0,1,2]}, \
                date_parser=dateparse, delimiter=r"\s+", usecols=[0,1,2,3,4,5,6])
        iauhist.columns = ['date', 'MJD', 'x', 'y', 'UT1-UTC']
        #iauhist = iauhist.set_index('date')

        #%% read current pole coordinates
        dateparse = lambda x,y,z: datetime.strptime(x.zfill(2)+y.zfill(2)+z.zfill(2), '%y%m%d')
        fw = [2,2,2,9,3,9,9,10,9,3,10,10]
        iaucurr = pd.read_fwf(self.iaucurr_file, header=None, widths=fw, parse_dates= {'date':[0,1,2]}, date_parser=dateparse, \
            usecols=[0,1,2,3,5,7,10])
        iaucurr.columns = ['date', 'MJD', 'x', 'y', 'UT1-UTC']
        #iaucurr = iaucurr.set_index('date')
        #%%
        mask = (iaucurr['date'] > iauhist['date'].values[-1])
        etpolut = iauhist.append(iaucurr[mask])
        etpolut = etpolut[np.isfinite(etpolut['x'])]
        #iauhist.combine_first(iaucurr)

        #%%
        etpolut['Date'] = etpolut['date'].dt.strftime('%Y%m%d')
        etpolut['Time'] = etpolut['date'].dt.strftime('%H%M%S')
        etpolut['MJD'] = etpolut['MJD'].map('{:8.3f}'.format)
        etpolut['x'] = etpolut['x'].map('{:9.5f}'.format)
        etpolut['y'] = etpolut['y'].map('{:9.5f}'.format)
        etpolut['TAI-UT1'] = etpolut['UT1-UTC']
        etpolut['UT1-UTC'] = etpolut['UT1-UTC'].map('{:9.6f}'.format)

        #%%
        # prepare the last column
        for idx, val in leapsdf.iterrows():
            # print(idx, val[1])
            # find mask for leap seconds
            if idx+1 in leapsdf.index:
                mask = ((etpolut['date'] >= leapsdf['date'].loc[idx]) & (etpolut['date'] < leapsdf['date'].loc[idx+1]))
                #print(mask)
                # subtract leap seconds from UTC
                etpolut.loc[mask, 'TAI-UT1'] = leapsdf['leaps'].loc[idx] - etpolut.loc[mask, 'TAI-UT1']
            else:
                mask = (etpolut['date'] >= leapsdf['date'].loc[idx])
                etpolut.loc[mask, 'TAI-UT1'] = leapsdf['leaps'].loc[idx] - etpolut.loc[mask, 'TAI-UT1']

        etpolut['TAI-UT1'] = etpolut['TAI-UT1'].map('{:9.6f}'.format)

        #%%
        #etpolut[0] = etpolut[0].map('${:,.2f}'.format)
        header = \
"""File     : etpolut1.dat
Updated  : $1$
Contents : Pole coordinates and earth rotation one day sampling interval,
   given at 0 hours UTC. Historic data is combined with predictions.
   Data are from IERS and USNO.
Period   : $2$
Historic : $3$
Current  : $4$
Leap sec.: $5$

Date     Time   MJD         x         y       UT1-UTC   TAI-UT1
                       ["]       ["]      [sec]     [sec]
C****************************************************************\n"""
        header = header.replace("$1$", datetime.utcnow().strftime('%d/%m/%Y'))
        header = header.replace("$2$", etpolut['date'].iloc[0].strftime('%d/%m/%Y') \
                + ' to ' + etpolut['date'].iloc[-1].strftime('%d/%m/%Y'))
        header = header.replace("$3$", self.iauhist_rfile)
        header = header.replace("$4$", self.iaucurr_rfile)
        header = header.replace("$5$", self.leapsec_rfile)

        pd.options.display.max_colwidth = 200

        # IMPORTANT: newline needs to comply with windows platform!
        with open(self.etpolut1_file, "w", newline='') as myfile:
            myfile.write(header)
            # myfile.write(etpolut['combined'].to_string(index=False, header=False).replace('\n ', '\n'))
            # etpolut['combined'].to_string(myfile, index=False, header=False)
            # WEIRD PANDAS BUG: to_string() puts white space at beginning of each line
            for index, row in etpolut.iterrows():
                string = "{:s} {:s} {:s} {:s} {:s} {:s} {:s}".format(row['Date'], row['Time'], row['MJD'],\
                        row['x'], row['y'], row['UT1-UTC'], row['TAI-UT1'])
                myfile.write(string + '\r\n')
            myfile.write("99999999")
        myfile.close()
        # update also bin file
        self.etpolut1_dat2bin()
        print(f'Finished updating {self.etpolut1_file}')

    def etpolut1_dat2bin(self):
        """update the etpolut1 binary file from the equivalent text file"""
        header = []
        # find the end of the header
        with open(self.etpolut1_file, "r") as f:
            for num, line in enumerate(f, 1):
                header.append(line)
                if "C*******" in header[-1]: break

        # read into dataframe
        cols = ['Date', 'Time', 'MJD', 'x', 'y', 'UT1-UTC', 'TAI-UT1']
        etpolut = pd.read_csv(self.etpolut1_file, names=cols, skiprows=num, header=None, delimiter=r"\s+")
        # drop the last row with EOL ('99999999')
        etpolut = etpolut[:-1]
        print("File '{:s}' has {:d} rows.".format(self.etpolut1_file, etpolut.shape[0]))
        #%%
        # write as binary for use in fortran: each record has 4*8 bytes = 32 bytes
        # header contains start date in MJD and number of rows + 1
        head = np.array([np.int32(etpolut.iloc[0, 2]), np.int32(etpolut.shape[0]+1)])
        data = np.float64(etpolut.values[:, 3:])
        #print(data)
        with open(self.etpolut1_bfile,'wb+') as f:
            # write header integers
            f.write(head.tobytes())
            # advance to next record (32 bytes)
            f.seek(32)
            # write the flattened matrix (this may have 64 bytes)
            f.write(data.flatten().tobytes())
        f.close()
        print("File '{:s}' has been updated (Header: {:.0f}, {:d}).".format(self.etpolut1_bfile, etpolut.iloc[0, 2], etpolut.shape[0]+1))

    #%% update the time conversion database (leap seconds)
    def update_etddt(self):
        """Updates the leap second file"""
        #%%
        print("--------------------------------------")
        print("-->> Updating time conversion database '{:s}':".format(self.leapsec_file))

        #%% READ THE EXISTING FILE
        # print(etddt_file)
        # find the end of the header
        with open(self.etddt_file, "r") as f:
            print(f"Processing file '{self.etddt_file}..")
            header = []
            regex = re.compile(r"^\s*Status\s*\:.*$", re.IGNORECASE)
            for num, line in enumerate(f, 1):
                line = regex.sub("Status    : %s" % datetime.utcnow().strftime('%d/%m/%Y'), line)
                header.append(line)
                if "C*******" in header[-1]: break

        cols = ['year','JD','DDT']
        etddt = pd.read_csv(self.etddt_file, names=cols, skiprows=num, header=None, delimiter=r"\s+")
        #delete last entry so it gets updated
        etddt.drop(etddt.tail(1).index,inplace=True)
            #%% read leap second history
        dateparse = lambda x: datetime.strptime(x, '%d %m %Y')
        leapsdf = pd.read_csv(self.leapsec_file, comment='#', header=None, parse_dates= {'date':[1,2,3]}, date_parser=dateparse, delimiter=r"\s+")
        leapsdf.columns = ['date', 'MJD', 'leaps']
        #appedn current date

        currentdt=pd.Timestamp.now()
        leapsdf.loc[len(leapsdf.index)]={"date":currentdt,"MJD":currentdt.to_julian_date() - 2400000.5,"leaps":leapsdf.tail(1).leaps.item()}
        # leapsdf = leapsdf.set_index('date')
        # DDT = delta-T + delta-UT = leaps + 32.184 s offset
        leapsdf['DDT'] = leapsdf['leaps'] + 32.184

        #%%
        leapsdf['JD'] = [dt.to_julian_date() for dt in leapsdf['date']]
        leapsdf['year'] = [timestampToDecyear(dt) for dt in leapsdf['date']]


        #%%
        mask = (leapsdf['year'] > etddt['year'].values[-1])
        indices = leapsdf.index[mask]
        # print(indices)

        for i, val in enumerate(indices):
            # for each record append a new row
            etddt.loc[len(etddt.index)]={'year': leapsdf.loc[val, 'year'], 'JD': leapsdf.loc[val, 'JD'], 'DDT': leapsdf.loc[val, 'DDT']}

        # number of new records
        records = sum(mask)
        if (records > 0):
            # write header
            with open(self.etddt_file, "w+", newline='\r\n') as f:
                f.write("".join(header))
                #etddt['combined'].to_string(f, index=False, header=False)
                #f.write("\n")
                # WEIRD PANDAS BUG: to_string() puts white space at beginning of each line
                for index, row in etddt.iterrows():
                    string = "{:.5f} {:.5f} {:8.3f}".format(row['year'], row['JD'], row['DDT'])
                    # print(string)
                    f.write(string+"\n")
                f.close()
            print(f'{records} records were added to the template.')
        print(f"The File ('{self.etddt_file}') is now up to date ")

    def update(self):
        """
        self.update() refreshes the variables of PyGTide based on the Fortran module etpred
        """
        self.exectime = etpred.inout.exectime
        self.headers = np.char.strip(etpred.inout.header.astype('str'))
        self.args = etpred.inout.argsin
        self.unit = etpred.inout.etpunit.astype('str')

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
            from warnings import warn
            fname = str(etpred.params.etddtdat)
            warn("Prediction timeframe is earlier than the available time database (%s). "
                 "For details refer to the file '%s'." % (self.etddt_start, fname))
        if (enddate > (self.etddt_end + timedelta(days=365))):
            from warnings import warn
            fname = str(etpred.params.etddtdat)
            warn("Prediction timeframe exceeds the end of the available time database (%s) plus 1 year. "
                 "For best accuracy, please consider updating '%s'." % (self.etddt_end, fname))
        # if not (-50*365 < (startdate - dt.datetime.now()).days < 365):
        if ( ((argsin[13] > 0) or (argsin[14] > 0)) and ((startdate < self.etpolut1_start) or (enddate > self.etpolut1_end)) ):
            fname = str(etpred.params.etddtdat)
            raise ValueError("Dates exceed permissible range for pole/LOD tide correction (interval %s to %s). "\
                "Please update file '%s'." % (self.etpolut1_start, self.etpolut1_end, fname))
        # set the start date and time
        argsin[3:6] = [startdate.year,startdate.month,startdate.day]
        # test sammprate validity
        if not (0 < samprate <= 24*3600):
            raise ValueError("Samprate exceeds permissible range!")
        else:
            argsin[7] = int(samprate)
        # test that samprate is not larger than duration
        if (samprate/3600 >  duration):
            raise ValueError("Samprate exceeds duration!")
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
    def results(self, round=6):
        """
        self.results(round=6)
        Returns:
            - If predict() was executed, returns a dataframe with the results
            - False
        keyword 'round' sets the number of digits returned.
        """
        if self.exec:
            import pandas as pd
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

    # easy access to the formatted data calculated by Fortran
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

def update_data_files(msg=False):
    pt = pygtide(msg=msg)
    pt.update_data_files()
    print('finished pygtide data update')
