CC=ftn
CFLAGS=-O3
EXT=f90
#SUBFIX=_solution

.SUFFIXES: .$(EXT)
#all: 1 2

1: 1.ring$(SUBFIX)
2: 2.exchange_ghost_cells$(SUBFIX)

1.ring$(SUBFIX).o: 1.ring$(SUBFIX).$(EXT)
	$(CC) $(CFLAGS) -c 1.ring$(SUBFIX).$(EXT) -o 1.ring$(SUBFIX).o

1.ring$(SUBFIX): 1.ring$(SUBFIX).o
	$(CC) $(CFLAGS) 1.ring$(SUBFIX).o -o 1.ring$(SUBFIX)

2.exchange_ghost_cells$(SUBFIX).o: 2.exchange_ghost_cells$(SUBFIX).$(EXT)
	$(CC) $(CFLAGS) -c 2.exchange_ghost_cells$(SUBFIX).$(EXT) -o 2.exchange_ghost_cells$(SUBFIX).o

2.exchange_ghost_cells$(SUBFIX): 2.exchange_ghost_cells$(SUBFIX).o
	$(CC) $(CFLAGS) 2.exchange_ghost_cells$(SUBFIX).o -o 2.exchange_ghost_cells$(SUBFIX)

clean:
	rm -f *.o
	rm -f 1.ring$(SUBFIX)
	rm -f 2.exchange_ghost_cells$(SUBFIX)

