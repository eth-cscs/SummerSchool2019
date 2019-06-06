CC=CC
CFLAGS=-O3
EXT=c
#SUBFIX=_solution

.SUFFIXES: .$(EXT)
all: 1 2

1: 1.get$(SUBFIX)
2: 2.put$(SUBFIX)

2.put$(SUBFIX).o: 2.put$(SUBFIX).$(EXT)
	$(CC) $(CFLAGS) -c 2.put$(SUBFIX).$(EXT) -o 2.put$(SUBFIX).o

2.put$(SUBFIX): 2.put$(SUBFIX).o
	$(CC) $(CFLAGS) 2.put$(SUBFIX).o -o 2.put$(SUBFIX)

1.get$(SUBFIX).o: 1.get$(SUBFIX).$(EXT)
	$(CC) $(CFLAGS) -c 1.get$(SUBFIX).$(EXT) -o 1.get$(SUBFIX).o

1.get$(SUBFIX): 1.get$(SUBFIX).o
	$(CC) $(CFLAGS) 1.get$(SUBFIX).o -o 1.get$(SUBFIX)


clean:
	rm -f *.o
	rm -f 1.get$(SUBFIX)
	rm -f 2.put$(SUBFIX)
