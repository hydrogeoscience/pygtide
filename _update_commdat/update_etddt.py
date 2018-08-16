#from IPython import get_ipython
#get_ipython().magic('reset -sf') 

from astropy.time import Time
import time as tt
import pandas as pd
import datetime as dt
import urllib
import re

def upd_etddt(remote_file, local_file):
    #%%
    etddt_file = "etddt.dat"
    
    leapsec_remote_file = 'https://hpiers.obspm.fr/iers/bul/bulc/Leap_Second_History.dat'
    leapsec_file = '[raw]_Leap_Second_History.dat'
    
    print("Updating time conversion database '{:s}':".format(etddt_file))
    #%% download leap second history
    start = tt.time()
    try:
        urllib.request.urlopen(leapsec_remote_file)
    except OSError as error:
        print("ERROR: Could not connect to remote server!")
        print("MESSAGE: {0}.".format(error))
        pass
    else:
        print('Start downloading: {:s} ...'.format(leapsec_remote_file))
        urllib.request.urlretrieve(leapsec_remote_file, leapsec_file)
        end = tt.time()
        print('Finished downloading: (elapsed time: {:.1f} s) ...'.format((end - start)))
        #%% READ THE EXISTING FILE
        try:
            # find the end of the header
            with open(etddt_file) as f:
                print("Processing file '{:s}' ...".format(etddt_file))
                header = []
                regex = re.compile(r"^\s*updated\s*\:.*$", re.IGNORECASE)
                for num, line in enumerate(f, 1):
                    line = regex.sub("Updated    : %s" % dt.datetime.utcnow().strftime('%d/%m/%Y'), line)
                    header.append(line)
                    if "C*******" in header[-1]:
                        hlines = num
                        break
            
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