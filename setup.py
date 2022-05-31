from setuptools import setup

with open('README.md', 'r') as f:
    long_description = f.read()

setup(
    name='pygtide',
    version='0.2',
    packages=['pygtide', 'etpred'],
    package_data={'etpred': ['*.pyd', '*.so', 'commdat/*']},
    install_requires=['numpy', 'pandas'],
    author='Gabriel C. Rau',
    author_email='gabriel@hydrogeo.science',
    url='http://doi.org/10.5281/zenodo.1346664',
    description=('A Python module and wrapper for ETERNA PREDICT to compute '
                 'gravitational tides on Earth'),
    long_description=long_description,
    long_description_content_type='text/markdown')
