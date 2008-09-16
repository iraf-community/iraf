
.c.o:	; $(CC) $(HSI_CF) -c $*.c

all:	rmfiles.e

rmfiles.e:	rmfiles.o
	$(CC) $(HSI_LF) -o $@ rmfiles.o $(HSI_LIBS)
	cp -p $@ ../../bin/.

clean::
	rm -f *.o rmfiles.e ../../bin/rmfiles.e
