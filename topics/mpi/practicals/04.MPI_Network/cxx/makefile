CC=CC
NVCC=nvcc
CFLAGS=-arch=sm_60 -std=c++11 -O2 -lineinfo -I.
EXT=cu
LDFLAGS=-lcublas -lcuda
SUBFIX=_solution

.SUFFIXES: .$(EXT)
all: 1

1: 1.diffusion2d_mpi$(SUBFIX)


1.diffusion2d_mpi$(SUBFIX).o: 1.diffusion2d_mpi$(SUBFIX).$(EXT)
	$(NVCC) -c $(CFLAGS) 1.diffusion2d_mpi$(SUBFIX).$(EXT) -I$(CRAY_MPICH2_DIR)/include

1.diffusion2d_mpi$(SUBFIX): 1.diffusion2d_mpi$(SUBFIX).o
	$(CC) 1.diffusion2d_mpi$(SUBFIX).o -o 1.diffusion2d_mpi$(SUBFIX) $(LDFLAGS)

clean:
	rm -f *.o
	rm -f 1.diffusion2d_mpi$(SUBFIX)
	rm -f output.*
