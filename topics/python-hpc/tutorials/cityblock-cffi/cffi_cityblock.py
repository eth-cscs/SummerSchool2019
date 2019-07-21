import os
import cffi


ffi = cffi.FFI()

with open(os.path.join(os.getcwd(), 'cityblock.h')) as f:
    ffi.cdef(f.read())

ffi.set_source("_cityblock",    # python-binding source file to be generated
               '#include "cityblock.h"',   # include header file
               libraries=["cityblock"],    # name of the library (lib<name>.so)
               library_dirs=[os.getcwd()])  # path where libcityblock.so is

ffi.compile()
