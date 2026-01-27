project('pygtide', 'fortran',
  version : '0.81',
  license : 'MPL-2.0',
  meson_version : '>=1.1.0',
)

# Get Python installation
py = import('python').find_installation()

# Determine file extension based on OS
host_os = host_machine.system()
if host_os == 'windows'
  ext = 'pyd'
else
  ext = 'so'
endif

# Source file
src = 'src/etpred.f90'

# Build the Fortran extension module using f2py
# f2py generates a version-specific filename like etpred.cp314-win_amd64.pyd
etpred_module = custom_target(
  'etpred_build',
  input : src,
  output : 'etpred.' + ext,
  command : [
    py,
    '-m', 'numpy.f2py',
    '-c',
    '--f90flags=-fPIC -O3',
    '-m', 'etpred',
    '@INPUT@',
  ],
  build_by_default : true,
)

# Final copy: after the module is built, copy the versioned file into the
# package directory with a stable name `pygtide/etpred.{ext}` so imports work.
copy_code = 'import glob,shutil,os; os.chdir(os.environ.get("MESON_SOURCE_ROOT","../..")); files=glob.glob(os.path.join("build","**","etpred*.' + ext + '"),recursive=True); files and shutil.copy(files[0],os.path.join("pygtide","etpred.' + ext + '")) and print("Copied to pygtide/etpred.' + ext + '")'

final_copy = custom_target(
  'copy_etpred_final',
  input : etpred_module,
  output : 'copy_etpred.stamp',
  command : [py, '-c', copy_code],
  build_by_default : true,
  depends : etpred_module,
)

# Install package data (commdat files)
install_subdir(
  'pygtide/commdat',
  install_dir : py.get_install_dir() / 'pygtide',
  exclude_files : ['.gitkeep']
)

# Install the .pyd/.so file that was copied to pygtide/ by the copy_etpred_final target
install_data(
  'pygtide/etpred.' + ext,
  install_dir : py.get_install_dir() / 'pygtide',
)

# Install Python source files
install_data(
  'pygtide/__init__.py',
  'pygtide/core.py',
  'pygtide/tests.py',
  'pygtide/update_etpred_data.py',
  install_dir : py.get_install_dir() / 'pygtide',
)
