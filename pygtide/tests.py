# PyGTide tests
import numpy as np
from pygtide import predict_series, predict_spectrum, predict_table
from pygtide import plot_series, plot_spectrum
from pygtide import pygtide

def test(msg=False):
    pt = pygtide(msg)
    args = (-20.82071, -70.15288, 830.0, '2017-01-01', 6, 600)
    pt.predict(*args, statazimut=90, tidalcompo=8)
    pt.results()

    args = (-20.82071, -70.15288, 830.0, '2020-01-01', 6, 600)
    series = predict_series(*args, statazimut=90, tidalcompo=8)
    expected = np.array([
            2.328813,  1.185839,  0.075844, -0.994862, -2.020277, -2.994753,
           -3.913028, -4.770264, -5.562074, -6.284551, -6.934289, -7.508405,
           -8.004555, -8.420941, -8.756326, -9.010027, -9.181925, -9.272451,
           -9.282583, -9.213832, -9.068222, -8.848276, -8.556987, -8.197794,
           -7.774554, -7.291505, -6.753236, -6.164649, -5.530918, -4.857453,
           -4.149855, -3.413875, -2.655371, -1.880268, -1.094508, -0.304014,
            0.485354,  1.267843,  2.037841,  2.789921,  3.518876,  4.21975 ])
    np.testing.assert_almost_equal(series, expected, 5)

    args = (-20.82071, -70.15288, 830.0, '2020-01-01', 29.5 * 24, 600)
    predict_table(*args, statazimut=90, tidalcompo=8, msg=msg)
    freq, spec = predict_spectrum(*args, statazimut=90, tidalcompo=8)
    index = np.argmax(np.abs(spec))
    freqM2 = freq[index] * 3600
    freqM2expected = 1 / 12.421
    assert abs(freqM2 - freqM2expected) / freqM2expected < 2e-3

    try:
        import matplotlib.pyplot as plt
    except ImportError:
        pass
    else:
        plot_spectrum(*args, statazimut=90, tidalcompo=8, show=False)
        plot_series(*args, statazimut=90, tidalcompo=8, show=False)
        if msg:
            plt.show()
    print('finished pygtide tests')

def test_refresh(msg=False):

    pt = pygtide(msg)
    pt.refresh_data()
    print('finished pygtide data refresh test')
