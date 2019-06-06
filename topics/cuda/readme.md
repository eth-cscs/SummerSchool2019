# CUDA

This path contains the slides and exercise material for the CUDA part of the course.

To compile and run the exercises and examples, the environment on Daint must first be configured:

```
module load daint-gpu
module swap PrgEnv-cray PrgEnv-gnu
module load cudatoolkit/9.0.103_3.7-6.0.4.1_2.1__g72b395b
```

To get an interactive session on one node:

```
salloc -Cgpu -t60 --reservation=course
```
