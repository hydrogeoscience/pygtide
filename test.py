#%% PyGTide class test
import pygtide
import datetime as dt

# create a PyGTide object
pt = pygtide.pygtide()

# define a start date
start = dt.datetime(2017,1,1)

# calculate the gravitational tides 
# BUG: CURRENT COMBINATION CRASHING THE CODE (1 / 23) 
pt.predict(-33.33, 151.33, 589.33, start, 2, 249, fileprd=1)

# retrieve the results as Pandas data frame
data = pt.results()

# output
print(data)

# update the pole coordinates and UT1 time database
pt.update_etpolut1()

# update the time database
#pt.update_etddt()
