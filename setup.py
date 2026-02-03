# setup.py
from pathlib import Path
import sys

from setuptools import setup, Distribution
from setuptools.command.build import build as _build


class build(_build):
    def run(self):
        # Detect platform-specific ABI extension
        here = Path(__file__).resolve().parent
        ext = '.pyd' if sys.platform.startswith('win') else '.so'
        etpred_path = here / 'pygtide' / f'etpred{ext}'
        if etpred_path.exists():
            print(f"Use ABI module {etpred_path}")
        else:
            print(f"Prebuilt ABI module not found: {etpred_path}\n"
                    "Run build_pygtide_abi.py")
            sys.path.insert(0, str(here))
            import build_pygtide_abi
            # Start Meson build
            build_pygtide_abi.build()
            if not etpred_path.exists():
                raise FileNotFoundError(f"No ABI module, error in Meson build")
        super().run()


# tell setuptools to build platform-dependent wheels
class BinaryDistribution(Distribution):
    def has_ext_modules(self):
        return True


# Metadata pulled from pyproject.toml
setup(
    distclass=BinaryDistribution,
    cmdclass={'build': build}
)

