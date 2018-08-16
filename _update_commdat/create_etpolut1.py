# PyTide automatic fetching of the file 'etpolut1.dat'
import numpy as np
import pandas as pd
import datetime as dt
import time as tt
import urllib.request

download = False

leapsec_remote_file = 'https://hpiers.obspm.fr/iers/bul/bulc/Leap_Second_History.dat'
leapsec_file = '[raw]_Leap_Second_History.dat'

iau_remote_file = 'http://maia.usno.navy.mil/ser7/finals2000A.all'
iau_file = '[raw]_finals2000A.dat'

#%% download leap second history
start = tt.time()
if download:
    print('Start downloading: Leap_Second_History ...')
    urllib.request.urlretrieve(leapsec_remote_file, leapsec_file)
    end = tt.time()
    print('Finished downloading: (elapsed time: %.1f s) ...' % (end - start))
    
    print('Start downloading polar motions ...')
    urllib.request.urlretrieve(iau_remote_file, iau_file)
    end = tt.time()
    print('Finished downloading (elapsed time: %.1f s) ...' % (end - start))

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

#%% read current pole coordinates
dateparse = lambda x,y,z: dt.datetime.strptime(x.zfill(2)+y.zfill(2)+z.zfill(2), '%y%m%d')
fw = [2,2,2,9,3,9,9,10,9,3,10,10]
iau = pd.read_fwf(iau_file, header=None, widths=fw,parse_dates= {'date':[0,1,2]}, date_parser=dateparse, \
    usecols=[0,1,2,3,5,7,10])
iau.columns = ['date', 'MJD', 'x', 'y', 'UT1-UTC']
#iaucurr = iaucurr.set_index('date')
#%%
etpolut = iau.dropna(how='any')

#iauhist.combine_first(iaucurr)
#%%

etpolut.loc[:, 'Date'] = etpolut.loc[:,'date'].dt.strftime('%Y%m%d')
etpolut.loc[:,'Time'] = etpolut.loc[:,'date'].dt.strftime('%H%M%S')
etpolut.loc[:,'MJD'] = etpolut.loc[:,'MJD'].map('{:8.3f}'.format)
etpolut.loc[:,'x'] = etpolut.loc[:,'x'].map('{:9.5f}'.format)
etpolut.loc[:,'y'] = etpolut.loc[:,'y'].map('{:9.5f}'.format)
etpolut.loc[:,'TAI-UT1'] = etpolut.loc[:,'UT1-UTC']
etpolut.loc[:,'UT1-UTC'] = etpolut.loc[:,'UT1-UTC'].map('{:9.6f}'.format)
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

etpolut.loc[:,'TAI-UT1'] = etpolut.loc[:,'TAI-UT1'].map('{:9.6f}'.format)

#%%
#etpolut[0] = etpolut[0].map('${:,.2f}'.format)
header = """File     : etpolut1.dat
Updated  : $1$
Contents : Pole coordinates and earth rotation one day sampling interval,
           given at 0 hours UTC. Historic data is combined with predictions. 
           Data are from IERS and USNO.
Period   : $2$
Source   : $3$
Leaps    : $4$

Date     Time   MJD         x         y       UT1-UTC   TAI-UT1                 
                           ["]       ["]      [sec]     [sec]                   
C****************************************************************\n"""
header = header.replace("$1$", dt.datetime.utcnow().strftime('%d/%m/%Y'))
header = header.replace("$2$", etpolut['date'].iloc[0].strftime('%d/%m/%Y') \
        + ' to ' + etpolut['date'].iloc[-1].strftime('%d/%m/%Y'))
header = header.replace("$3$", iau_remote_file)
header = header.replace("$4$", leapsec_remote_file)

pd.options.display.max_colwidth = 200
etpolut.loc[:,'combined']=etpolut.loc[:,'Date'].astype(str)+' '+etpolut.loc[:,'Time'].astype(str)\
    +' '+etpolut.loc[:,'MJD'].astype(str)+' '+etpolut.loc[:,'x'].astype(str)+' '+etpolut.loc[:,'y'].astype(str)\
    +' '+etpolut.loc[:,'UT1-UTC'].astype(str)+' '+etpolut.loc[:,'TAI-UT1'].astype(str)
    
with open("etpolut1.dat", "w+") as myfile:
    myfile.write(header)
    etpolut['combined'].to_string(myfile, index=False, header=False)
    myfile.write("\n99999999")
    
myfile.close()
end = tt.time()
print('Finished updating (total time: %.1f s)!' % (end - start))