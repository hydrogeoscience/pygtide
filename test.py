#%% PyGTide class test
import pygtide

# create a PyGTide object
pt = pygtide.pygtide()

# calculate the gravitational tides
# BUG: CURRENT COMBINATION CRASHING THE CODE (1 / 23)
#pt.predict(-33.33, 151.33, 589.33, start, 2, 249, fileprd=0)
pt.predict(-20.82071, -70.15288, 830.0, '2017-01-01', 6, 600, statazimut=90,
           tidalcompo=8)

# retrieve the results as Pandas data frame
data = pt.results()

# output
print(data)

# update the pole coordinates and UT1 time database
#pt.update_etpolut1()

#pt.update_etddt()

#pt.etpolut1_dat2bin()

# update the time database
#pt.update_etddt()
