project('pygtide', 'fortran',
  version : '0.8.1',
  meson_version : '>=1.1.0',
)

py = import('python').find_installation()

host_os = host_machine.system()
fc = meson.get_compiler('fortran')

if host_os == 'windows'
  ext = 'pyd'
  if fc.get_id() == 'msvc'
    f90flags = ['/O2']
  else
    f90flags = ['-O3']
  endif
else
  ext = 'so'
  f90flags = ['-fPIC', '-O3']
endif

src = 'src/etpred.f90'
f90_flag_args = ['--f90flags=' + ' '.join(f90flags)]

# --------------------------------------------------
# 1. Build f2py (ABI-tagged output)
# --------------------------------------------------
etpred_build = custom_target(
  'etpred_build',
  input : src,
  output : 'etpred.build.stamp',
  command : [
    py,
    '-m', 'numpy.f2py',
    '-c',
  ] + f90_flag_args + [
    '-m', 'etpred',
    '@INPUT@',
  ],
  build_by_default : true,
)

# --------------------------------------------------
# 2. Copy ABI module → pygtide/etpred.pyd
# --------------------------------------------------
copy_code = '''
import glob, os, shutil, sys

build_dir = os.getcwd()
src_root = r"@0@"

pattern = os.path.join(build_dir, '**', 'etpred*.@1@')
matches = glob.glob(pattern, recursive=True)

if not matches:
    print("ERROR: etpred ABI module not found")
    sys.exit(1)

dst = os.path.join(src_root, 'pygtide', 'etpred.@1@')
shutil.copy(matches[0], dst)
print(f"Copied {matches[0]} -> {dst}")
'''.format(
  meson.project_source_root(),
  ext
)

custom_target(
  'copy_etpred',
  input : etpred_build,
  output : 'etpred.copy.stamp',
  command : [py, '-c', copy_code],
  build_by_default : true,
)
