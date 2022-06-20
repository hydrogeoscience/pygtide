import numpy as np
from pygtide import predict_series

args = (-20.82071, -70.15288, 830.0, '2017-01-01', 10, 600)
series = predict_series(*args, statazimut=90, tidalcompo=0)
print(repr(series))