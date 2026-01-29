import subprocess
import shutil
import sys
import os

# --- Step 1: Setup build dir ---
build_dir = "build"

if not os.path.exists(build_dir):
    subprocess.check_call(["meson", "setup", build_dir])
else:
    print(f"Build directory '{build_dir}' already exists, skipping setup.")

# --- Step 2: Compile ---
subprocess.check_call(["meson", "compile", "-C", build_dir])

# --- Step 3: Clean up build dir ---
shutil.rmtree(build_dir)
print(f"Deleted build directory '{build_dir}'")
