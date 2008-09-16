
.c.o:	; $(CC) $(HSI_CF) -c $*.c

all:	rtar.e

rtar.e:	rtar.o
	$(CC) $(HSI_LF) -o $@ rtar.o $(HSI_LIBS)
	cp -p $@ ../../bin/.

clean::
	rm -f *.o rtar.e ../../bin/rtar.e
