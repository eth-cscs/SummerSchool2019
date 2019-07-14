#!/bin/bash

module load daint-gpu
module load PyExtensions/2.7.15.1-CrayGNU-18.08

python2 $(dirname $0)/plotting.py
