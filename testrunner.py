#!/usr/bin/env python3

# usage: ./testrunner <files>

import sys, os

statFail = 0

tests = sys.argv[1:]
for t in tests:
    print('running test {}'.format(t))
    code  = os.system('cat {} | ./codeRebuild.native'.format(t))
    if(code != 0):
        statFail += 1
        print()

print('{} tests run, {} passes, {} fails'.format(len(tests), len(tests) - statFail, statFail))
