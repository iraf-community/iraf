
.c.o:	; $(CC) $(HSI_CF) -c $*.c

all:	wtar.e

wtar.e:	wtar.o
	$(CC) $(HSI_LF) -o $@ wtar.o $(HSI_LIBS)
	cp -p $@ ../../bin/.

clean::
	rm -f *.o wtar.e ../../bin/wtar.e
