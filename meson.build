project('pygtide', ['c', 'fortran'],
  version : '0.9.5',
  meson_version : '>=1.1.0',
  default_options : ['optimization=3'],
)

py = import('python').find_installation()

numpy_include = run_command(
  py, '-c', 'import numpy; print(numpy.get_include())',
  check : true,
).stdout().strip()
f2py_include = run_command(
  py, '-c', 'import numpy.f2py; print(numpy.f2py.get_include())',
  check : true,
).stdout().strip()

f2py_sources = custom_target(
  'etpred_f2py_sources',
  input : 'src/etpred.f90',
  output : [
    'etpredmodule.c',
    'etpred-f2pywrappers.f',
    'etpred-f2pywrappers2.f90',
  ],
  command : [
    py,
    '-m', 'numpy.f2py',
    '@INPUT@',
    '-m', 'etpred',
    '--lower',
  ],
)

py.extension_module(
  'etpred',
  [
    'src/etpred.f90',
    f2py_sources,
    join_paths(f2py_include, 'fortranobject.c'),
  ],
  include_directories : include_directories(numpy_include, f2py_include),
  install : false,
)
