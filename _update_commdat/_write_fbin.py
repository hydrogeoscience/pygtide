# -*- coding: utf-8 -*-
import numpy as np

#%%

wints = np.array([12345,34957])
wfloats = np.random.rand(100000,4)

with open('test.bin','wb+') as f:
    f.write(wints.tobytes())
    f.seek(32)
    f.write(wfloats.flatten('F').tobytes())

print('Wrote ints:')
print(wints)

print('Wrote floats:')
print(wfloats)

#%%
with open('test.bin','rb') as f:
    rints = np.fromfile(f, dtype=np.int, count=2)
    f.seek(32)
    rfloats = np.fromfile(f, dtype=np.float64)

rfloats = np.reshape(rfloats, (-1, 4))
print('Read ints:')
print(rints)
print('Read floats:')
print(rfloats)

#%% ACTUAL FILE !!!!!
with open('..\commdat\etpolut1.bin','rb') as f:
    header = np.fromfile(f, dtype=np.int, count=2)
    f.seek(32)
    data = np.fromfile(f, dtype=np.float64)

print(header)
print(data)
print(data.shape)
data = np.reshape(data, (-1, 4))
print(data)
print(data.shape)

print(data[-1,:])