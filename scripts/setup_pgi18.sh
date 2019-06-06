#!/bin/bash

PGIV=18.4
module swap PrgEnv-cray PrgEnv-pgi
module use /apps/common/UES/pgi/$PGIV/modulefiles
module rm pgi/17.5.0
module load pgi/$PGIV
export PGI_VERS_STR=$PGIV.0
