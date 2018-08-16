# -*- coding: utf-8 -*-
"""
Created on Fri Oct 20 16:04:05 2017

@author: z3229417
"""

# PyTide automatic update of the file 'etpolut1.dat'

import numpy as np
import pandas as pd
import datetime as dt
import time
import urllib.request 


#%%
start = time.time()
print('Start downloading ...')
urllib.request.urlretrieve('http://hpiers.obspm.fr/iers/eop/eopc04/eopc04_IAU2000.62-now', 'eopc04_IAU2000.dat')
end = time.time()
print('Finished downloading (elapsed time: %.1f s) ...' % (end - start))
#%%
leaps = np.array(\
        [[19620101, 10],[19720701, 11],[19730101, 12],[19740101, 13],[19750101, 14],
          [19760101, 15],[19770101, 16],[19780101, 17],[19790101, 18],[19800101, 19],
          [19810701, 20],[19820701, 21],[19830701, 22],[19850701, 23],[19880101, 24],
          [19900101, 25],[19910101, 26],[19920701, 27],[19930701, 28],[19940701, 29],
          [19960101, 30],[19970701, 31],[19990101, 32],[20060101, 33],[20090101, 34],
          [20120701, 35],[20150701, 36],[20170101, 37],[20190101, 37]])

leapdate = [dt.datetime.strptime(x, '%Y%m%d') for x in leaps[:, 0].astype('str')]
          
raw = pd.read_csv("eopc04_IAU2000.dat", skiprows=13, header=None, delimiter=r"\s+")

raw['datetime'] = pd.to_datetime(raw.iloc[:,0]*10000+raw.iloc[:,1]*100+raw.iloc[:,2],format='%Y%m%d')
etpolut = pd.DataFrame(columns=['Date','Time','MJD','x','y','UT1-UTC','TAI-UT1'])
etpolut['Date'] = raw['datetime'].dt.strftime('%Y%m%d')
etpolut['Time'] = raw['datetime'].dt.strftime('%H%M%S')
etpolut['MJD'] = raw.iloc[:,3].map('{:8.3f}'.format)
etpolut['x'] = raw.iloc[:,4].map('{:9.5f}'.format)
etpolut['y'] = raw.iloc[:,5].map('{:9.5f}'.format)
etpolut['UT1-UTC'] = raw.iloc[:,6].map('{:9.6f}'.format)

# prepare the last column
for idx, val in enumerate(leapdate[:-1]):
    # print(idx, val)
    # find mask for leap seconds
    mask = ((raw['datetime'] >= leapdate[idx]) & (raw['datetime'] < leapdate[idx+1]))
    #print(mask)
    # subtract leap seconds from UTC
    raw.loc[mask,6] = leaps[idx,1] - raw.loc[mask,6]

etpolut['TAI-UT1'] = raw.iloc[:,6].map('{:9.6f}'.format)

#etpolut[0] = etpolut[0].map('${:,.2f}'.format)
header = """File     : etpolut1.dat
Updated  : $$$$
Contents : Pole coordinates and earth rotation one day sampling
           interval, given at 0 hours UTC. The pole coordinates, 
           UT1-UTC and TAI-UT1 are from IERS.
Period   : ####
Basis    : http://hpiers.obspm.fr/iers/eop/eopc04/eopc04_IAU2000.62-now                      

Date     Time   MJD         x         y       UT1-UTC   TAI-UT1                 
                           ["]       ["]      [SEC]     [SEC]                   
C****************************************************************\n"""
header = header.replace("####", raw['datetime'].iloc[0].strftime('%d/%m/%Y') \
        + ' to ' + raw['datetime'].iloc[-1].strftime('%d/%m/%Y'))

header = header.replace("$$$$", dt.datetime.utcnow().strftime('%d/%m/%Y'))

etpolut['combined']=etpolut['Date'].astype(str)+' '+etpolut['Time'].astype(str)+' '+etpolut['MJD'].astype(str)\
    +' '+etpolut['x'].astype(str)+' '+etpolut['y'].astype(str)+' '+etpolut['UT1-UTC'].astype(str)\
    +' '+etpolut['TAI-UT1'].astype(str)

with open("testfile.dat", "w") as myfile:
    myfile.write(header)

pd.options.display.max_colwidth = 200
with open('testfile.dat','a') as outfile:
    etpolut['combined'].to_string(outfile, index=False, header=False)

with open("testfile.dat", "a") as myfile:
    myfile.write("\n99999999")

myfile.close()
end = time.time()
print('Finished updating (total time: %.1f s)!' % (end - start))