#!/usr/bin/env python3

import os
os.system('make clean')
os.system('make')

print('\nrunning hello world test\n')

code  = os.system('./toplevel.native helloworld.oc | lli > helloworld.output')

with open('helloworld.output', 'r') as f:
    with open('helloworld.goldstd', 'r') as gs:
        out = f.read()
        if gs.read() == out:
            print('Hello world check passed')
        else: 
            print(f'Unexpected output: {out}.')
