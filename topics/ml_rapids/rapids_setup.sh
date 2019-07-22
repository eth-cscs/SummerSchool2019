#!/bin/bash

echo "Source this to make available the changes that conda does to bashrc."

#download miniconda
if [ ! -f  Miniconda3-latest-Linux-x86_64.sh ]; then
    wget https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86_64.sh
fi

#install into scratch dir
bash Miniconda3-latest-Linux-x86_64.sh -u -b -p /scratch/snx3000/$USER/miniconda

#setup conda
/scratch/snx3000/$USER/miniconda/bin/conda init bash
. $HOME/.bashrc

#create conda environment
conda create --name myrapids
conda activate myrapids


#install python libraries
conda install -y -c nvidia -c rapidsai -c numba -c conda-forge -c pytorch -c defaults -c rapidsai/label/xgboost \
    cudf=0.8 cuml=0.8 cugraph=0.8 python=3.6 cudatoolkit=9.2 xgboost=0.90 jupyter matplotlib graphviz python-graphviz libgd

#conda install -y -c conda-forge graphviz python-graphviz 

#register kernel
#we cannot run: python -m ipykernel install --user --name rapids --display-name "Python 3.7 (RAPIDS)"
# because we need to set env

mkdir -p $HOME/.local/share/jupyter/kernels/rapids

printf "{\n \
 \"argv\": [\n \
  \"/scratch/snx3000/$USER/miniconda/envs/myrapids/bin/python\",\n \
  \"-m\",\n \
  \"ipykernel_launcher\",\n \
  \"-f\",\n \
  \"{connection_file}\"\n \
 ],\n \
 \"display_name\": \"Python 3.7 (RAPIDS)\",\n \
 \"language\": \"python\",\n \
\"env\":{\"PYTHONPATH\":\"\"}\n \
}\n " > $HOME/.local/share/jupyter/kernels/rapids/kernel.json

echo "PATH=/apps/daint/UES/jenscscs/6.0.UP07/gpu/easybuild/tools/software/graphviz/2.40.1/bin:$PATH" >> .bashrc
echo "LD_LIBRARY_PATH=/apps/daint/UES/jenscscs/6.0.UP07/gpu/easybuild/tools/software/graphviz/2.40.1/lib:$LD_LIBRARY_PATH" >> .bashrc

