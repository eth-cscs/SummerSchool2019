# Tutorials

To avoid the need for restarting JupyterLab, we will use the kernel `miniconda-sumsch` for the notebook with  TensorFlow-1.12.0 and the default kernel (`Python3`) for the notebooks with TensorFlow-2.0.

In order to access TensorFlow-2.0 with the default kernel, before launching JupyterLab, we need to create the file `$HOME/.jupyterhub.env` and write the following there
```bash
module use /apps/daint/UES/6.0.UP04/sandboxes/tensorflow-sumsch/modules/all
module load TensorFlow/2.0.0-beta1-CrayGNU-18.08-cuda-9.2-python3
```
If you already have a JupyterLab running, please restart it.
