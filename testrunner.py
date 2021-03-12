#!/usr/bin/env python3

# usage: ./testrunner <files>

import sys
import subprocess

statPass = 0
statFail = 0

tests = sys.argv[1:]
for t in tests:
    result = subprocess.run(['cat', t, '|', './toplevel.native'], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    if(result.returncode != 0):
        statFail += 1
        print('test {} failed with exit code {}...\nstdout:{}\nstderr:{}'.format(t, result.returncode, result.stdout, result.stderr))
    else:
        statPass += 1
        print('test {} passed with stdout: {}'.format(result.stdout))

print('{} tests run, {} passes, {} fails'.format(len(tests), statPass, statFail))
