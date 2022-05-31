#from IPython import get_ipython
#get_ipython().magic('reset -sf')

import os
from astropy.time import Time
import numpy as np
import time as tt
import pandas as pd
import datetime as dt
import urllib
import re

def etpolut1(data_dir, etpolut1_file, leapsec_rfile, iauhist_rfile, iaucurr_rfile):

    status = True
    etpolut1_file = os.path.join(data_dir, etpolut1_file)
    leapsec_file = os.path.join(data_dir, '[raw]_Leap_Second_History.dat')
    iauhist_file = os.path.join(data_dir, '[raw]_eopc04_IAU2000.dat')
    iaucurr_file = os.path.join(data_dir, '[raw]_finals2000A.dat')

    print("Updating pole coordinate and UT1 time database '{:s}':".format(etpolut1_file))
    start = tt.time()
    if status:
        try:
            urllib.request.urlopen(leapsec_rfile)
        except OSError as error:
            print("ERROR: Could not connect to remote server: {:s}".format(leapsec_rfile))
            print("MESSAGE: {0}.".format(error))
            status=False
            pass
        else:
            print('Start downloading: {:s} ...'.format(leapsec_rfile))
            urllib.request.urlretrieve(leapsec_rfile, leapsec_file)
            end = tt.time()
            print('Finished downloading ({:.1f} s).'.format((end - start)))

    if status:
        try:
            urllib.request.urlopen(iauhist_rfile)
        except OSError as error:
            print("ERROR: Could not connect to remote server: {:s}".format(iauhist_rfile))
            print("MESSAGE: {0}.".format(error))
            status=False
            pass
        else:
            print('Start downloading: {:s} ...'.format(iauhist_rfile))
            urllib.request.urlretrieve(iauhist_rfile, iauhist_file)
            end = tt.time()
            print('Finished downloading ({:.1f} s).'.format((end - start)))

    if status:
        try:
            urllib.request.urlopen(iaucurr_rfile)
        except OSError as error:
            print("ERROR: Could not connect to remote server: {:s}".format(iaucurr_rfile))
            print("MESSAGE: {0}.".format(error))
            status=False
            pass
        else:
            print('Start downloading: {:s} ...'.format(iaucurr_rfile))
            urllib.request.urlretrieve(iaucurr_rfile, iaucurr_file)
            end = tt.time()
            print('Finished downloading ({:.1f} s).'.format((end - start)))

    #%%
    if status:
        try:
            open(leapsec_file, "r")
        except OSError as error:
            print("ERROR: Could not open file: {:s}".format(leapsec_file))
            print("MESSAGE: {0}.".format(error))
            status = False
            pass

    if status:
        try:
            open(iauhist_file, "r")
        except OSError as error:
            print("ERROR: Could not open file: {:s}".format(iauhist_file))
            print("MESSAGE: {0}.".format(error))
            status = False
            pass

    if status:
        try:
            open(iaucurr_file, "r")
        except OSError as error:
            print("ERROR: Could not open file: {:s}".format(iaucurr_file))
            print("MESSAGE: {0}.".format(error))
            status = False
            pass

    if status:
        #%% read leap second history
        dateparse = lambda x: pd.datetime.strptime(x, '%d %m %Y')
        leapsdf = pd.read_csv(leapsec_file, comment='#', header=None, parse_dates= {'date':[1,2,3]}, \
                date_parser=dateparse, delimiter=r"\s+")
        leapsdf.columns = ['date', 'MJD', 'leaps']
        leapsdf.drop(['MJD'], axis = 1, inplace = True)
        leapsdf.index += 1
        # insert row at the beginning
        leapsdf.loc[0] = [dt.datetime(1962,1,1), 10]
        leapsdf.sort_index(inplace=True)
        #%% read historic pole coordinates
        # convert = {3: lambda x: np.around(np.float64(x), 3)}
        dateparse = lambda x: pd.datetime.strptime(x, '%Y %m %d')
        iauhist = pd.read_csv(iauhist_file, skiprows=13, header=None, parse_dates= {'date':[0,1,2]}, \
                date_parser=dateparse, delimiter=r"\s+", usecols=[0,1,2,3,4,5,6])
        iauhist.columns = ['date', 'MJD', 'x', 'y', 'UT1-UTC']
        #iauhist = iauhist.set_index('date')

        #%% read current pole coordinates
        dateparse = lambda x,y,z: dt.datetime.strptime(x.zfill(2)+y.zfill(2)+z.zfill(2), '%y%m%d')
        fw = [2,2,2,9,3,9,9,10,9,3,10,10]
        iaucurr = pd.read_fwf(iaucurr_file, header=None, widths=fw,parse_dates= {'date':[0,1,2]}, date_parser=dateparse, \
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
        header = header.replace("$1$", dt.datetime.utcnow().strftime('%d/%m/%Y'))
        header = header.replace("$2$", etpolut['date'].iloc[0].strftime('%d/%m/%Y') \
                + ' to ' + etpolut['date'].iloc[-1].strftime('%d/%m/%Y'))
        header = header.replace("$3$", iauhist_rfile)
        header = header.replace("$4$", iaucurr_rfile)
        header = header.replace("$5$", leapsec_rfile)

        pd.options.display.max_colwidth = 200
        etpolut['combined']=etpolut['Date'].astype(str)+' '+etpolut['Time'].astype(str)+' '+etpolut['MJD'].astype(str)\
            +' '+etpolut['x'].astype(str)+' '+etpolut['y'].astype(str)+' '+etpolut['UT1-UTC'].astype(str)\
            +' '+etpolut['TAI-UT1'].astype(str)

        with open(etpolut1_file, "w") as myfile:
            myfile.write(header)
            etpolut['combined'].to_string(myfile, index=False, header=False)
            myfile.write("\n99999999")

        myfile.close()
        end = tt.time()
        print('Finished updating {:s} ({:.1f} s).'.format(etpolut1_file, (end - start)))
    else:
        print('Update failed!')
        pass

def etddt(data_dir, etddt_file, leapsec_rfile):
    etddt_file = os.path.join(data_dir, etddt_file)
    leapsec_file = os.path.join(data_dir, '[raw]_Leap_Second_History.dat')
    #%%
    print("Updating time conversion database '{:s}':".format(etddt_file))
    #%% download leap second history
    start = tt.time()
    try:
        urllib.request.urlopen(leapsec_rfile)
    except OSError as error:
        print("ERROR: Could not connect to remote server!")
        print("MESSAGE: {0}.".format(error))
        pass
    else:
        print('Start downloading: {:s} ...'.format(leapsec_rfile))
        urllib.request.urlretrieve(leapsec_rfile, leapsec_file)
        end = tt.time()
        print('Finished downloading ({:.1f} s) ...'.format((end - start)))
        #%% READ THE EXISTING FILE
        try:
            # find the end of the header
            with open(etddt_file, "r") as f:
                print("Processing file '{:s}' ...".format(etddt_file))
                header = []
                regex = re.compile(r"^\s*updated\s*\:.*$", re.IGNORECASE)
                for num, line in enumerate(f, 1):
                    line = regex.sub("Updated    : %s" % dt.datetime.utcnow().strftime('%d/%m/%Y'), line)
                    header.append(line)
                    if "C*******" in header[-1]: break

            cols = ['year','JD','DDT']
            etddt = pd.read_csv(etddt_file, names=cols, skiprows=num, header=None, delimiter=r"\s+")
            #%% read leap second history
            dateparse = lambda x: pd.datetime.strptime(x, '%d %m %Y')
            leapsdf = pd.read_csv(leapsec_file, comment='#', header=None, parse_dates= {'date':[1,2,3]}, \
                    date_parser=dateparse, delimiter=r"\s+")
            leapsdf.columns = ['date', 'MJD', 'leaps']
            # leapsdf = leapsdf.set_index('date')
            # DDT = delta-T + delta-UT = leaps + 32.184 s offset
            leapsdf['DDT'] = leapsdf['leaps'] + 32.184
            #%%
            leapsdf['JD'] = Time(leapsdf['date'].values.astype(str), scale='utc').jd
            leapsdf['year'] = Time(leapsdf['date'].values.astype(str), scale='utc').decimalyear
            #%%
            mask = (leapsdf['year'] > etddt['year'].values[-1])
            # number of new records
            records = mask[mask == True].shape[0]
            if (records > 0):
                etddt = etddt.append(leapsdf.loc[mask, ['year','JD','DDT']])
                # format to required precision
                etddt['year'] = etddt['year'].map('{:.5f}'.format)
                etddt['JD'] = etddt['JD'].map('{:.6f}'.format)
                etddt['DDT'] = etddt['DDT'].map('{:9.3f}'.format)
                #combine & write
                etddt['combined']=etddt['year'].astype(str)+' '+etddt['JD'].astype(str)+' '+etddt['DDT'].astype(str)
                # write header
                with open(etddt_file, "w") as f:
                    f.write("".join(header))
                    etddt['combined'].to_string(f, index=False, header=False)
                    f.write("\n")
                    f.close()
                print('{:d} records were added.'.format(records))
            else:
                print('Nothing to add.' )

            end = tt.time()
            print('Done after {:.1f} seconds.'.format(end - start))
        except OSError as error:
            print("ERROR: Could not open file '{0}'.".format(etddt_file))
            print("MESSAGE: {0}.".format(error))
            pass
