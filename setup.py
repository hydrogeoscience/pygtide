# this is adapted from
# http://omz-software.com/pythonista/numpy/user/c-info.python-as-glue.html

name = "etpred"

def configuration(name, parent_package='', top_path=None):
    from numpy.distutils.misc_util import Configuration
    config = Configuration('', parent_package, top_path)
    config.add_extension('etpred', sources=['etpred.pyf','etpred.f90'])
    return config

if __name__ == '__main__':
    from numpy.distutils.core import setup
    setup(
        name = name,
        version = "0.1",
        description = "Calculates gravitational time series using tidal catalogues",
        url = "https://hydrogeo.science",
        author = "Gabriel C. Rau",
        author_email = "gabriel@hydrogeo.science",
        license = "None",
        platform = "Windows 7",
        **configuration(name, top_path='').todict())

