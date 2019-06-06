CC=ftn
CFLAGS=-O3
EXT=f90
#SUBFIX=_solution

.SUFFIXES: .$(EXT)
#all: 1 2 3 4

1: 1.broadcast$(SUBFIX)
2: 2.scatter$(SUBFIX)
3: 3.reduce$(SUBFIX)
4: 4.allreduce$(SUBFIX)

1.broadcast$(SUBFIX).o: 1.broadcast$(SUBFIX).$(EXT)
	$(CC) $(CFLAGS) -c 1.broadcast$(SUBFIX).$(EXT) -o 1.broadcast$(SUBFIX).o

1.broadcast$(SUBFIX): 1.broadcast$(SUBFIX).o
	$(CC) $(CFLAGS) 1.broadcast$(SUBFIX).o -o 1.broadcast$(SUBFIX)

2.scatter$(SUBFIX).o: 2.scatter$(SUBFIX).$(EXT)
	$(CC) $(CFLAGS) -c 2.scatter$(SUBFIX).$(EXT) -o 2.scatter$(SUBFIX).o

2.scatter$(SUBFIX): 2.scatter$(SUBFIX).o
	$(CC) $(CFLAGS) 2.scatter$(SUBFIX).o -o 2.scatter$(SUBFIX)

3.reduce$(SUBFIX).o: 3.reduce$(SUBFIX).$(EXT)
	$(CC) $(CFLAGS) -c 3.reduce$(SUBFIX).$(EXT) -o 3.reduce$(SUBFIX).o

3.reduce$(SUBFIX): 3.reduce$(SUBFIX).o
	$(CC) $(CFLAGS) 3.reduce$(SUBFIX).o -o 3.reduce$(SUBFIX)

4.allreduce$(SUBFIX).o: 4.allreduce$(SUBFIX).$(EXT)
	$(CC) $(CFLAGS) -c 4.allreduce$(SUBFIX).$(EXT) -o 4.allreduce$(SUBFIX).o

4.allreduce$(SUBFIX): 4.allreduce$(SUBFIX).o
	$(CC) $(CFLAGS) 4.allreduce$(SUBFIX).o -o 4.allreduce$(SUBFIX)

clean:
	rm -f *.o
	rm -f 1.broadcast$(SUBFIX)
	rm -f 2.scatter$(SUBFIX)
	rm -f 3.reduce$(SUBFIX)
	rm -f 4.allreduce$(SUBFIX)

