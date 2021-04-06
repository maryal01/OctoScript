import subprocess

p1 = subprocess.Popen(["./toplevel.native", "test.oc", "|", "tee" "test.ll"])
p1.wait()

p2 = subprocess.Popen(["llc", "test.ll"])
p2.wait()

p3 = subprocess.Popen(["gcc", "test.s"])
p3.wait()

p4 = subprocess.Popen(["./a.out"])
p4.wait()
