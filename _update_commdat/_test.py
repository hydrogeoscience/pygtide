#%% astropy test
import datetime as dt
from astropy.time import Time
import urllib.request
from jdcal import gcal2jd, jd2gcal
import astropy.units as u
import numpy as np
import pandas as pd
import datetime as dt
import urllib.request
import astropy.units as u

times = ['1999-01-01T00:00:00.123456789', '2020-01-01T00:00:00']
t = Time(times, format='isot', scale='utc')

print(t.mjd)
