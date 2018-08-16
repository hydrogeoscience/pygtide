# from
# http://omz-software.com/pythonista/numpy/user/c-info.python-as-glue.html

#import numpy.f2py as f2py
#
#fid = open('etpred_f2py.f90')
#source = fid.read()
#fid.close()
#
#f2py.compile(source, modulename='etpred')
#
#import etpred

import os
os.system("f2py -c etpred.f90 -m etpred --compiler=mingw32")
