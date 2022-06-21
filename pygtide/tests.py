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
    args = (-20.82071, -70.15288, 830.0, '2017-01-01', 10, 600)
    series = predict_series(*args, statazimut=90, tidalcompo=0)
    expected = np.array([ 607.605208,  630.191906,  646.733265,  657.225843,  661.710028,
        660.269321,  653.029317,  640.156373,  621.855993,  598.370935,
        569.979057,  536.99093 ,  499.74723 ,  458.615933,  413.989338,
        366.280944,  315.922193,  263.359123,  209.048945,  153.456565,
        97.051092,   40.302345,  -16.322614,  -72.362893, -127.367107,
    -180.896615, -232.528646, -281.859268, -328.506215, -372.11152 ,
    -412.343966, -448.901318, -481.51234 , -509.938572, -533.975866,
    -553.455669, -568.246045, -578.252435, -583.418153, -583.724611,
    -579.191278, -569.875375, -555.87131 , -537.309859, -514.357095,
    -487.213081, -456.110337, -421.312083, -383.110279, -341.823479,
    -297.794502, -251.387943, -202.987549, -152.993454, -101.819321,
        -49.889385,    2.36457 ,   54.506287,  106.098136,  156.704236,
            205.893556])
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



#_plot_tides()
