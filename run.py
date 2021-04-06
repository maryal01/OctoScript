import subprocess

p1 = subprocess.run(["./toplevel.native", "test.oc", "|", "tee" "test.ll"])

p2 = subprocess.run(["llc", "test.ll"])

p3 = subprocess.run(["gcc", "test.s"])

p4 = subprocess.run(["./a.out"])
