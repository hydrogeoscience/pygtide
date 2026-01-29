# setup.py
from pathlib import Path
import sys

from setuptools import setup, Extension


# Detect platform-specific ABI extension
HERE = Path(__file__).resolve().parent

ext = '.pyd' if sys.platform.startswith('win') else '.so'
etpred_path = HERE / 'pygtide' / f'etpred{ext}'

if etpred_path.exists():
    print(f"Use ABI module {etpred_path}")
else:
    print(f"Prebuilt ABI module not found: {etpred_path}\n"
            "Run build_pygtide_abi.py")
    sys.path.insert(0, str(HERE))
    import build_pygtide_abi
    build_pygtide_abi.build()
    if not etpred_path.exists():
        raise FileNotFoundError(f"No ABI module, error in Meson build")


# Define the prebuilt extension
etpred_module = Extension(
    name='pygtide.etpred',
    sources=[],               # No sources; using prebuilt ABI
    extra_objects=[str(etpred_path)],
)


# Metadata pulled from pyproject.toml
setup()
