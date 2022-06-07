"""
PyGTide Update - A python class to update the files upon which PyGTide depends
-------------------------------------------------------------------------------
Author: Gabriel C. Rau (gabriel@hydrogeo.science)
Website: https://hydrogeo.science
"""
import pygtide.etpred as etpred
from pkg_resources import resource_filename
from pathlib import Path
import numpy as np
import time as tt
import pandas as pd
import datetime as dt
import urllib, urllib.request, re, os

def timestampToDecyear(ts):
    year=ts.year
    jan1=pd.Timestamp(year,1,1)
    jan1next=pd.Timestamp(year+1,1,1)
    yrlen=(jan1next-jan1).total_seconds()
    return year+(ts-jan1).total_seconds()/yrlen

class update_etpred_db(object):
    """
    The pygtide_update class will initialise internal variables
    """
    def __init__(self, msg=True):
        self.msg = msg
        self.data_dir = resource_filename('pygtide', 'commdat/')
        etpred.params.comdir = self.data_dir + ' ' * (256 - len(self.data_dir))
        # set OS dependent module output
        etpred.params.nullfile = os.devnull + ' ' * (10 - len(os.devnull))
        self.etddt_file = str(etpred.params.etddtdat, 'UTF-8').strip()
        self.etpolut1_dat_file = str(etpred.params.etpolutdat, 'UTF-8').strip()
        self.etpolut1_bin_file = str(etpred.params.etpolutbin, 'UTF-8').strip()

        #%% remote data files
        # IERS leap seconds history file
        self.etddt_tmpl = 'etddt_tmpl.dat'
        self.leapsec_rfile = 'https://hpiers.obspm.fr/iers/bul/bulc/Leap_Second_History.dat'
        # IERS pole coordinate observations
        # self.iauhist_rfile = 'http://hpiers.obspm.fr/iers/eop/eopc04/eopc04_IAU2000.62-now'
        self.iauhist_rfile = 'ftp://hpiers.obspm.fr/iers/eop/eopc04/eopc04_IAU2000.62-now'
        # US navy pole coordinate predictions
        self.iaucurr_rfile = 'https://datacenter.iers.org/data/9/finals2000A.all'
        # self.iaucurr_rfile = 'ftp://cddis.gsfc.nasa.gov/pub/products/iers/finals2000A.all'


    #%% update the pole coordinates and UT1 to TAI times
    def update_etpolut1(self):
        global etpolut
        status = True
        etpolut1_file = Path(self.data_dir + '/' + self.etpolut1_dat_file)
        leapsec_file = Path(self.data_dir + '/' + '[raw]_Leap_Second_History.dat')
        iauhist_file = Path(self.data_dir + '/' + '[raw]_eopc04_IAU2000.dat')
        iaucurr_file = Path(self.data_dir + '/' + '[raw]_finals2000A.dat')

        print("--------------------------------------")
        print("-->> Updating the Earth orientation database '{:s}':".format(etpolut1_file.as_posix()))
        start = tt.time()
        if status:
            try:
                urllib.request.urlopen(self.leapsec_rfile)
            except OSError as error:
                print("ERROR: Could not connect to remote server: {:s}".format(self.leapsec_rfile))
                print("MESSAGE: {0}.".format(error))
                status=False
                pass
            else:
                print('Start downloading: {:s} ...'.format(self.leapsec_rfile))
                urllib.request.urlretrieve(self.leapsec_rfile, leapsec_file)
                end = tt.time()
                print('Finished downloading ({:.1f} s).'.format((end - start)))

        if status:
            try:
                urllib.request.urlopen(self.iauhist_rfile)
            except OSError as error:
                print("ERROR: Could not connect to remote server: {:s}".format(self.iauhist_rfile))
                print("MESSAGE: {0}.".format(error))
                status=False
                pass
            else:
                print('Start downloading: {:s} ...'.format(self.iauhist_rfile))
                urllib.request.urlretrieve(self.iauhist_rfile, iauhist_file)
                end = tt.time()
                print('Finished downloading ({:.1f} s).'.format((end - start)))

        if status:
            try:
                urllib.request.urlopen(self.iaucurr_rfile)
            except OSError as error:
                print("ERROR: Could not connect to remote server: {:s}".format(self.iaucurr_rfile))
                print("MESSAGE: {0}.".format(error))
                status=False
                pass
            else:
                print('Start downloading: {:s} ...'.format(self.iaucurr_rfile))
                urllib.request.urlretrieve(self.iaucurr_rfile, iaucurr_file)
                end = tt.time()
                print('Finished downloading ({:.1f} s).'.format((end - start)))

        #%%
        if status:
            try:
                open(leapsec_file, "r")
            except OSError as error:
                print("ERROR: Could not open file: {:s}".format(leapsec_file.as_posix()))
                print("MESSAGE: {0}.".format(error))
                status = False
                pass

        if status:
            try:
                open(iauhist_file, "r")
            except OSError as error:
                print("ERROR: Could not open file: {:s}".format(iauhist_file.as_posix()))
                print("MESSAGE: {0}.".format(error))
                status = False
                pass

        if status:
            try:
                open(iaucurr_file, "r")
            except OSError as error:
                print("ERROR: Could not open file: {:s}".format(iaucurr_file.as_posix()))
                print("MESSAGE: {0}.".format(error))
                status = False
                pass

        if status:
            #%% read leap second history
            dateparse = lambda x: dt.datetime.strptime(x, '%d %m %Y')
            leapsdf = pd.read_csv(leapsec_file, comment='#', header=None, parse_dates= {'date':[1,2,3]}, \
                    date_parser=dateparse, delimiter=r"\s+")
            leapsdf.columns = ['date', 'MJD', 'leaps']
            leapsdf.drop(['MJD'], axis=1, inplace=True)
            leapsdf.index += 1
            # insert row at the beginning
            leapsdf.loc[0] = [dt.datetime(1962,1,1), 10]
            leapsdf.sort_index(inplace=True)
            #%% read historic pole coordinates
            # convert = {3: lambda x: np.around(np.float64(x), 3)}
            dateparse = lambda x: dt.datetime.strptime(x, '%Y %m %d')
            iauhist = pd.read_csv(iauhist_file, skiprows=13, header=None, parse_dates= {'date':[0,1,2]}, \
                    date_parser=dateparse, delimiter=r"\s+", usecols=[0,1,2,3,4,5,6])
            iauhist.columns = ['date', 'MJD', 'x', 'y', 'UT1-UTC']
            #iauhist = iauhist.set_index('date')

            #%% read current pole coordinates
            dateparse = lambda x,y,z: dt.datetime.strptime(x.zfill(2)+y.zfill(2)+z.zfill(2), '%y%m%d')
            fw = [2,2,2,9,3,9,9,10,9,3,10,10]
            iaucurr = pd.read_fwf(iaucurr_file, header=None, widths=fw, parse_dates={'date':[0,1,2]}, date_parser=dateparse, \
                usecols=[0,1,2,3,5,7,10])
            iaucurr.columns = ['date', 'MJD', 'x', 'y', 'UT1-UTC']
            #iaucurr = iaucurr.set_index('date')
            #%%
            mask = (iaucurr['date'] > iauhist['date'].values[-1])
            # etpolut = iauhist.append(iaucurr[mask])
            etpolut = pd.concat([iauhist, iaucurr[mask]])
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
            header = header.replace("$1$", dt.datetime.utcnow().strftime('%d/%m/%Y'))
            header = header.replace("$2$", etpolut['date'].iloc[0].strftime('%d/%m/%Y') \
                    + ' to ' + etpolut['date'].iloc[-1].strftime('%d/%m/%Y'))
            header = header.replace("$3$", self.iauhist_rfile)
            header = header.replace("$4$", self.iaucurr_rfile)
            header = header.replace("$5$", self.leapsec_rfile)

            pd.options.display.max_colwidth = 200

            # IMPORTANT: newline needs to comply with windows platform!
            # https://pythonconquerstheuniverse.wordpress.com/2011/05/08/newline-conversion-in-python-3/
            with open(etpolut1_file, "w", newline='') as myfile:
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
            end = tt.time()
            # update also bin file
            self.etpolut1_dat2bin()
            print('Finished updating {:s} ({:.1f} s).'.format(etpolut1_file.as_posix(), (end - start)))
        else:
            print('Update failed!')
            pass


    #%% update the etpolut1 binary file from the text file
    def etpolut1_dat2bin(self):
        etpolut1_dat = Path(self.data_dir + '/' + self.etpolut1_dat_file)
        etpolut1_bin = Path(self.data_dir + '/' + self.etpolut1_bin_file)
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
        print("File '{:s}' has {:d} rows.".format(etpolut1_dat.as_posix(), etpolut.shape[0]))
        #%%
        # write as binary for use in fortran: each record has 4*8 bytes = 32 bytes
        # header contains start date in MJD and number of rows + 1
        head = np.array([np.int32(etpolut.iloc[0, 2]), np.int32(etpolut.shape[0]+1)])
        data = np.float64(etpolut.values[:, 3:])
        #print(data)
        with open(etpolut1_bin,'wb+') as f:
            # write header integers
            f.write(head.tobytes())
            # advance to next record (32 bytes)
            f.seek(32)
            # write the flattened matrix (this may have 64 bytes)
            f.write(data.flatten().tobytes())
        f.close()
        print("File '{:s}' has been updated (Header: {:.0f}, {:d}).".format(etpolut1_bin.as_posix(), etpolut.iloc[0, 2], etpolut.shape[0]+1))


    #%% update the time conversion database (leap seconds)
    def update_etddt(self):
        global etddt, leapsdf, tmp
        leapsec_file = Path(self.data_dir + '/' + '[raw]_Leap_Second_History.dat')
        old_etddt_file = Path(self.data_dir + '/' + self.etddt_tmpl)
        etddt_file = Path(self.data_dir + '/' + self.etddt_file)

        #%%
        print("--------------------------------------")
        print("-->> Updating time conversion database '{:s}':".format(leapsec_file.as_posix()))
        #%% download leap second history
        start = tt.time()
        try:
            urllib.request.urlopen(self.leapsec_rfile)
        except OSError as error:
            print("ERROR: Could not connect to remote server!")
            print("MESSAGE: {0}.".format(error))
            pass
        else:
            print('Start downloading: {:s} ...'.format(leapsec_file.as_posix()))
            urllib.request.urlretrieve(self.leapsec_rfile, leapsec_file)
            end = tt.time()
            print('Finished downloading ({:.1f} s).'.format((end - start)))

        #%% READ THE EXISTING FILE
        # print(etddt_file)
        # find the end of the header
        with open(old_etddt_file, "r") as f:
            print("Processing file '{:s}' ...".format(etddt_file.as_posix()))
            header = []
            regex = re.compile(r"^\s*updated\s*\:.*$", re.IGNORECASE)
            for num, line in enumerate(f, 1):
                line = regex.sub("Updated    : %s" % dt.datetime.utcnow().strftime('%d/%m/%Y'), line)
                header.append(line)
                if "C*******" in header[-1]: break

        cols = ['year','JD','DDT']
        etddt = pd.read_csv(old_etddt_file, names=cols, skiprows=num, header=None, delimiter=r"\s+")

        #%% read leap second history
        dateparse = lambda x: dt.datetime.strptime(x, '%d %m %Y')
        leapsdf = pd.read_csv(leapsec_file, comment='#', header=None, parse_dates= {'date':[1,2,3]}, date_parser=dateparse, delimiter=r"\s+")
        leapsdf.columns = ['date', 'MJD', 'leaps']
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
        # tmp = []
        for i, val in enumerate(indices):
            # for each record create a new row
            etddt.loc[len(etddt) + 1] = {'year': leapsdf.loc[val, 'year'], 'JD': leapsdf.loc[val, 'JD'], 'DDT': leapsdf.loc[val, 'DDT']}

        # number of new records
        records = sum(mask)
        if (records > 0):
            # write header
            with open(self.data_dir + '/' + self.etddt_file, "w+", newline='\r\n') as f:
                f.write("".join(header))
                #etddt['combined'].to_string(f, index=False, header=False)
                #f.write("\n")
                # WEIRD PANDAS BUG: to_string() puts white space at beginning of each line
                for index, row in etddt.iterrows():
                    string = "{:.5f} {:.5f} {:8.3f}".format(row['year'], row['JD'], row['DDT'])
                    # print(string)
                    f.write(string + '\n')
                f.close()
            end = tt.time()
            print('{:d} records were added to the template ({:.1f} s).'.format(records, end - start))
        print("The leap second File ('{:s}') is now up to date ({:.1f} s).".format(self.etddt_file, end - start))

#%% run the update
def update(msg=True):
    pt = update_etpred_db(msg)
    pt.update_etddt()
    print(etddt.iloc[-10:, :])
    pt.update_etpolut1()
    print("---------------------")
