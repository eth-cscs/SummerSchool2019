CC=CC
CFLAGS=-O3
EXT=c
#SUBFIX=_solution

.SUFFIXES: .$(EXT)
#all: 1

1: 1.struct$(SUBFIX)

1.struct$(SUBFIX).o: 1.struct$(SUBFIX).$(EXT)
	$(CC) $(CFLAGS) -c 1.struct$(SUBFIX).$(EXT) -o 1.struct$(SUBFIX).o

1.struct$(SUBFIX): 1.struct$(SUBFIX).o
	$(CC) $(CFLAGS) 1.struct$(SUBFIX).o -o 1.struct$(SUBFIX)

clean:
	rm -f *.o
	rm -f 1.struct$(SUBFIX)

