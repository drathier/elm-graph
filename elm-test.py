import subprocess
import sys
from time import sleep

import os


def filter_stacktrace(s):
    ret = ""
    for line in s.splitlines(keepends=True):
        if "_user$project$" in line or all([x not in line for x in ["A1", "A2", "A3", "A4", "A5", "A6", "A7", "Function.eval", "Object.eval", "at eval", "at Object."]]):
            ret += line.replace("_user$project$", "").replace("$", ".", 1)
    return ret

try:  # windows
    x = subprocess.run([os.environ['USERPROFILE'] + r"\AppData\Roaming\npm\elm-test.cmd", "--stack-trace-limit=300"], stdout=subprocess.PIPE,
                       stderr=subprocess.PIPE)
except:  # macos / linux
    try:
        x = subprocess.run([r"/usr/local/bin/elm-test", "--stack-trace-limit=300"], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    except:  # travis ci
        x = subprocess.run([r"elm-test", "--stack-trace-limit=300"], stdout=subprocess.PIPE, stderr=subprocess.PIPE)

print("#stdout#")
sleep(0.2)
sys.stdout.write(x.stdout.decode("utf-8")[-10000:])
sleep(0.2)
print("#stdout#")
stderr = x.stderr.decode("utf-8")
if False and stderr != filter_stacktrace(stderr):
    print("#!stderr-raw!#")
    sleep(0.2)
    sys.stderr.write(stderr)
    sleep(0.2)
    print("#!stderr-raw!#")
print("#!stderr!#")
sleep(0.2)
sys.stderr.write(filter_stacktrace(stderr))
sleep(0.2)
print("#!stderr!#")


exit(x.returncode)
