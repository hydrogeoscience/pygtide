import subprocess
import shutil
import os
import glob
import sys


def build():
    # --- Configuration ---
    print('Start Meson build')
    build_dir = 'build_abi'
    ext = 'pyd' if os.name == 'nt' else 'so'

    # --- Step 1: Setup Meson build directory ---
    if os.path.exists(build_dir):
        print(f"Build directory '{build_dir}' already exists, skipping setup.")
    else:
        print(f"Setting up Meson build directory '{build_dir}'...")
        subprocess.check_call(['meson', 'setup', build_dir])
    # --- Step 2: Compile Meson targets ---
    print("Compiling Meson targets...")
    subprocess.check_call(['meson', 'compile', '-C', build_dir])

    # --- Step 3: Copy ABI module to pygtide/ ---
    print("Locating ABI module in build directory...")
    build_path = os.path.abspath(build_dir)
    pattern = os.path.join(build_path, '**', f'etpred*.{ext}')
    matches = glob.glob(pattern, recursive=True)

    if not matches:
        print("ERROR: ABI module not found! Did Meson compile succeed?")
        sys.exit(1)

    dst = os.path.join(os.path.abspath('.'), 'pygtide', f'etpred.{ext}')
    shutil.copy(matches[0], dst)
    print(f"Copied ABI module {matches[0]} -> {dst}")

    # --- Step 4: Clean up build directory ---
    print(f"Deleting build directory '{build_dir}'...")
    shutil.rmtree(build_dir)
    print("Meson build complete!")

    #print("\nNext step: run 'pip install .' or 'pip install -e .' to install the package.")


if __name__ == '__main__':
    build()
