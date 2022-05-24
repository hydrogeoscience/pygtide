# PyGTide v0.5
# A Python module and wrapper for ETERNA PREDICT to compute gravitational tides on Earth
import pygtide
import datetime as dt
import numpy as np

import sys
sys.path.append("D:\KIT\GitHub\pygtide")

# create a PyGTide object
pt = pygtide.pygtide()

# define a start date
start = dt.datetime(2018, 1, 1)

# calculate the gravitational tides 
lat, lon, height = 49.00937, 8.40444, 120
duration = 60*24
splrate = 3600

# run ETERNA PREDICT
pt.predict(lat, lon, height, start, duration, splrate)

# retrieve the results as dataframe
data = pt.results()

# output
print(data.iloc[0:10, 0:3])

# convert from UTC to a different time zone
data['UTC'].dt.tz_convert('Europe/Berlin')
