#!/usr/local/bin/python3

# usage: ./testrunner <file> (star operator works)

import sys
import glob
import subprocess

statPass = 0
statFail = 0

tests = glob.glob(sys.argv[1])
for t in tests:
    result = subprocess.run(['./toplevel.native', '<', 'cat', t], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    if(result.returncode != 0):
        statFail += 1
        print('test {} failed with exit code {}...\nstdout:{}\nstderr:{}'.format(t, result.returncode, result.stdout, result.stderr))
    else:
        statPass += 1
        print('test {} passed with stdout: {}'.format(result.stdout))

print('{} tests run, {} passes, {} fails'.format(len(tests), statPass, statFail))
