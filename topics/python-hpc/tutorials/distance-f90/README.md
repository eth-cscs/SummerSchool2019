# Implementation in Fortran90 of the Euclidean and Cityblock Distance Matrices

## Build with `setup.py`
```bash
python setup.py build_ext -i
```

## Build with F2PY by hand
```bash
f2py -c euclidean.f90 -m edm --f90flags='-fopenmp -O3' -lgomp
mkdir metrics
mv edm.cpy* metrics

f2py -c cityblock.f90 -m cbdm --f90flags='-fopenmp -O3' -lgomp
mkdir metrics
mv cityblock.cpy* metrics
```
