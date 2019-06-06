#!/bin/bash

module load daint-gpu
module load Python/2.7.12-CrayGNU-2016.11

python $(dirname $0)/plotting.py
