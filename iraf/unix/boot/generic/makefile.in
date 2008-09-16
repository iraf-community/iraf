
.c.o:	; $(CC) $(HSI_CF) -c $*.c

all:	generic.e

generic.e:	lexyy.o chario.o generic.o yywrap.o
	$(CC) $(HSI_LF) -o $@ lexyy.o chario.o generic.o yywrap.o
	cp -p $@ ../../bin/.

clean::
	rm -f *.o generic.e ../../bin/generic.e
