# setup.py
import sys
import os
from setuptools import setup, Extension, find_packages

# Detect platform-specific ABI extension
ext = '.pyd' if sys.platform.startswith('win') else '.so'
etpred_path = os.path.join('pygtide', f'etpred{ext}')

if not os.path.exists(etpred_path):
    raise FileNotFoundError(f"Prebuilt ABI module not found: {etpred_path}\n"
                            "Run build.py first to generate it.")

# Define the prebuilt extension
etpred_module = Extension(
    name='pygtide.etpred',
    sources=[],               # No sources; using prebuilt ABI
    extra_objects=[etpred_path],
)

# Setup configuration
setup(
    name='pygtide',
    version='0.8.0',
    description='A Python module and wrapper for ETERNA PREDICT to compute gravitational tides on Earth',
    author='Gabriel C. Rau',
    author_email='gabriel@hydrogeo.science',
    packages=find_packages(),  # finds 'pygtide'
    #ext_modules=[etpred_module],
    include_package_data=True,  # ensures any package data is included
    package_data={
        'pygtide': ['etpred.*', 'commdat/*'],
    },
    python_requires='>=3.8',
    install_requires=[
        'numpy>=1.21.0',
        'pandas',
        'requests',
    ],
    classifiers=[
        "Development Status :: 4 - Beta",
        "Intended Audience :: Science/Research",
        "License :: OSI Approved :: Mozilla Public License 2.0 (MPL 2.0)",
        "Operating System :: OS Independent",
        "Programming Language :: Python :: 3",
        "Topic :: Scientific/Engineering :: Physics",
    ],
)
