# -*- coding: utf-8 -*-

## code to transfer etpolut1.dat into etpolut1.bin

import pandas as pd
import numpy as np

etpolut1_dat = "etpolut1.dat"
etpolut1_bin = "etpolut1.bin"

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
#%% 
# write as binary for use in fortran
# in fortran, each record has 4 * 8 bytes = 32
# header contains start date in MJD and number of rows + 1
head = np.array([int(etpolut.iloc[0, 2]), int(etpolut.shape[0]+1)])
data = etpolut.values[:, 3:]
with open(etpolut1_bin,'wb+') as f:
    # write header integers
    f.write(head.tobytes())
    # advance to next record (32 bytes)
    f.seek(32)
    # write the flattened matrix
    f.write(data.flatten().tobytes())
f.close()

# DONE ###########################

#%% ACTUAL FILE !!!!!
with open(etpolut1_bin,'rb') as f:
    header = np.fromfile(f, dtype=np.int, count=2)
    f.seek(32)
    data = np.fromfile(f, dtype=np.float64)
f.close()

print(data)
print(data.shape)
data = np.reshape(data, (-1, 4))
print(data)
print(data.shape)
print(header)
print(data[-1,:])