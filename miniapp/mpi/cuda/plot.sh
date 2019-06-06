#!/bin/bash

module load daint-gpu
module load PyExtensions/2.7.13.1-CrayGNU-17.08

python2 $(dirname $0)/plotting.py
