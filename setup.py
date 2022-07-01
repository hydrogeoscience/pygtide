import os
import re
import platform

from numpy.distutils.core import setup, Extension

def find_version(*paths):
    fname = os.path.join(os.path.dirname(__file__), *paths)
    with open(fname) as fp:
        code = fp.read()
    match = re.search(r"^__version__ = ['\"]([^'\"]*)['\"]", code, re.M)
    if match:
        return match.group(1)
    raise RuntimeError("Unable to find version string.")


VERSION = find_version('pygtide', '__init__.py')
extra_link_args = None
if platform.system() == 'Windows' and os.environ['CIBW_BUILD']:
    # provide extra link args for wheel building on Windows
    raise
    extra_link_args = ('-static', '-static-libgfortran', '-static-libgcc')
ext = [Extension(name='pygtide.etpred', sources=['src/etpred.f90'],
                 extra_link_args=extra_link_args)]

setup(
    name='pygtide',
    version=VERSION,
    packages=['pygtide'],
    package_data={'pygtide': ['commdat/*']},
    ext_modules=ext,
    install_requires=['numpy', 'pandas'],
    author='Gabriel C. Rau, Tom Eulenfeld',
    author_email='gabriel@hydrogeo.science',
    url='https://github.com/hydrogeoscience/pygtide',
    description=('A Python module and wrapper for ETERNA PREDICT to compute '
                 'gravitational tides on Earth'),
    )
