#!/usr/bin/env python3

import os, sys

lib_files = ['prebuilt.o', 'test.o']

if len(sys.argv) < 2:
    print('No .oc file supplied')
    sys.exit(1)

libs = ' '.join(lib_files)

for f in sys.argv[1:]:

    base = '.'.join(f.split('.')[:-1])
    os.system(f'./toplevel.native {f} > {base}.ll')
    os.system(f'llc -relocation-model=pic {base}.ll > {base}.s')
    os.system(f'cc -o {base}.exe {base}.s {libs}')
    os.system(f'./{base}.exe')
