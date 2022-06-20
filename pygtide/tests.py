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
    expected = np.array([ 607.605208,  630.191908,  646.733272,  657.225859,  661.710055,
            660.269363,  653.029376,  640.156453,  621.856097,  598.371065,
            569.979217,  536.991122,  499.747456,  458.616196,  413.989642,
            366.28129 ,  315.922583,  263.35956 ,  209.049431,  153.457102,
             97.051682,   40.30299 ,  -16.321912,  -72.362132, -127.366285,
           -180.895731, -232.527698, -281.858255, -328.505135, -372.110372,
           -412.342748, -448.900029, -481.510979, -509.937138, -533.974358,
           -553.454086, -568.244386, -578.250699, -583.41634 , -583.72272 ,
           -579.189308, -569.873325, -555.869181, -537.30765 , -514.354806,
           -487.210711, -456.107887, -421.309551, -383.107667, -341.820786,
           -297.791728, -251.38509 , -202.984615, -152.990441, -101.816229,
            -49.886214,    2.367819,   54.509613,  106.101539,  156.707715,
            205.89711 ])
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
