FC=ftn
FFLAGS=-O3
EXT=f90
#SUBFIX=_solution

.SUFFIXES: .$(EXT)
#all: 1 2

1: 1.hello$(SUBFIX)
2: 2.hello_withrank$(SUBFIX)

1.hello$(SUBFIX).o: 1.hello$(SUBFIX).$(EXT)
	$(FC) $(FFLAGS) -c 1.hello$(SUBFIX).$(EXT) -o 1.hello$(SUBFIX).o

1.hello$(SUBFIX): 1.hello$(SUBFIX).o
	$(FC) $(FFLAGS) 1.hello$(SUBFIX).o -o 1.hello$(SUBFIX)

2.hello_withrank$(SUBFIX).o: 2.hello_withrank$(SUBFIX).$(EXT)
	$(FC) $(FFLAGS) -c 2.hello_withrank$(SUBFIX).$(EXT) -o 2.hello_withrank$(SUBFIX).o

2.hello_withrank$(SUBFIX): 2.hello_withrank$(SUBFIX).o
	$(FC) $(FFLAGS) 2.hello_withrank$(SUBFIX).o -o 2.hello_withrank$(SUBFIX)

clean:
	rm -f *.o
	rm -f 1.hello$(SUBFIX)
	rm -f 2.hello_withrank$(SUBFIX)

