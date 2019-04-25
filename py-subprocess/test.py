#!/usr/bin/env python3

import subprocess
import io
import tempfile

# this is just a simple test to see how to spawn a process,
# do something, and relay its stdout / stderr to the process
# that spawned it.
# turns out this is working, not 100% satisfied because
# stderr does not appear before stdout is done,
# but I feel this is good enough to work with.

def main():
    proc = subprocess.Popen(
        ['./out.sh'],
        shell=True,
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
    )
    for line in proc.stdout:
        print(f'subproc stdout:{line}')
    for line in proc.stderr:
        print(f'subproc stderr:{line}')
    proc.wait()


if __name__ == '__main__':
    main()
