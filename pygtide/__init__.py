from pygtide.core import pygtide
from pygtide.core import predict_series, predict_spectrum, predict_table
from pygtide.core import plot_series, plot_spectrum
from pygtide.tests import test
from pygtide.update_etpred_data import update

from importlib.metadata import version

try:
    __version__ = version("pygtide")
except Exception:
    __version__ = "0.8.0" 

__all__ = [
    "pygtide",
    "predict_series",
    "predict_spectrum",
    "predict_table",
    "plot_series",
    "plot_spectrum",
    "test",
    "update",
]
