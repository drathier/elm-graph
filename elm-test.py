import subprocess
import sys
from time import sleep

import os


def filter_stacktrace(s):
    ret = ""
    for line in s.splitlines(keepends=True):
        if "_user$project$" in line:
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
sys.stdout.write(x.stdout.decode("utf-8"))
print("#stdout#")
sleep(0.2)
print("#!stderr!#")
stderr = x.stderr.decode("utf-8")
sys.stderr.write(filter_stacktrace(stderr))
print("#!stderr!#")
exit(x.returncode)
