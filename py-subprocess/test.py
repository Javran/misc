#!/usr/bin/env python3

import subprocess
import io
import tempfile


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
