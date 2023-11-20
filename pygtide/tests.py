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

    # test against a proven array   
    args = (49.00937, 8.40444, 120, '2018-01-01', 50, 3600)
    series = predict_series(*args, tidalcompo=5)
    expected = np.array([ 20.120256,  11.371802,   0.303512, -10.684455, -19.316747,
           -23.986328, -24.121086, -20.285591, -13.997906,  -7.31219 ,
            -2.274406,  -0.387909,  -2.221906,  -7.260202, -14.028836,
           -20.472805, -24.490089, -24.490442, -19.837274, -11.056935,
             0.244288,  11.741719,  20.946235,  25.812105,  25.243209,
            19.366281,   9.499723,  -2.173853, -13.145977, -21.192355,
           -24.899299, -23.975888, -19.27862 , -12.549307,  -5.938311,
            -1.436475,  -0.358888,  -3.008326,  -8.60056 , -15.468955,
           -21.497314, -24.673643, -23.626746, -18.009927,  -8.631809,
             2.704171,  13.61692 ,  21.720306,  25.207537,  23.297815,
            16.429417])
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

    print('------------------------------------')
    print('Successfully finished PyGTide tests!')


def plot_pole_and_lod_tide():
    try:
        import matplotlib.pyplot as plt
    except ImportError:
        return False
    ploty = ['Signal [nm/s**2]', 'Pole tide [nm/s**2]', 'LOD tide [nm/s**2]']
    dates = ['1961-12-24', '2016-12-24', '2023-06-01']
    for date in dates:
        args = (20, 0, 0, date, 24*30, 600)
        tab = predict_table(*args)
        tab.plot(x='UTC', y=ploty, sharex=True, subplots=True)
        plt.savefig(f'tides_{date}.png')
    args = (-20.82071, -70.15288, 830.0, '2017-01-01', 10, 600)
    tab = predict_table(*args, statazimut=90)
    tab.plot(x='UTC', y=ploty, sharex=True, subplots=True)
    plt.savefig('tides_2017-01-01_10h.png')
    plt.show()

# test()
#_plot_tides()
