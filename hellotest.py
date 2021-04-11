#!/usr/bin/env python3

import os
os.system('make clean')
os.system('make')

print('\nrunning hello world test\n')

code  = os.system('./toplevel.native helloworld.oc | lli > helloworld.output')

with open('helloworld.output', 'r') as f:
    with open('helloworld.goldstd', 'r') as gs:
        if gs.read() == f.read():
            print('Hello world check passed')

