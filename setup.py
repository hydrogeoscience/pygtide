from numpy.distutils.core import setup, Extension

with open('README.md', 'r') as f:
    long_description = f.read()

ext = [Extension(name='pygtide.etpred',
                 sources=['src/etpred.f90'])]

setup(
    name='pygtide',
    version='0.3',
    packages=['pygtide'],
    package_data={'pygtide': ['commdat/*']},
    ext_modules=ext,
    install_requires=['numpy', 'pandas'],
    author='Gabriel C. Rau, Tom Eulenfeld',
    author_email='gabriel@hydrogeo.science',
    url='http://doi.org/10.5281/zenodo.1346664',
    description=('A Python module and wrapper for ETERNA PREDICT to compute '
                 'gravitational tides on Earth'),
    long_description=long_description,
    long_description_content_type='text/markdown')