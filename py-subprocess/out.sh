#!/bin/bash

# a simple shell script simulating a running script that
# prints to both stdout and stderr over a period of time.

for i in {1..5}
do
    echo "stdout: $i"
    >&2 echo "stderr: $i"
    sleep .5
done
