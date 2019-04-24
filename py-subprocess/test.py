#!/usr/bin/env python3

import subprocess
import io
import tempfile


def main():
    with tempfile.NamedTemporaryFile(mode='w', buffering=1) as f_out, \
         tempfile.NamedTemporaryFile(mode='w', buffering=1) as f_err:
        proc = subprocess.Popen(
            ['./out.sh'],
            shell=True,
            stdin=subprocess.PIPE,
            stdout=f_out,
            stderr=f_err,
        )
        proc.wait()

if __name__ == '__main__':
    main()
