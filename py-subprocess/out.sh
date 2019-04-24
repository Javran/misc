#!/bin/bash

for i in {1..5}
do
    echo "stdout: $i"
    >&2 echo "stderr: $i"
    sleep .5
done
