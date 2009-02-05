
.c.o:	; $(CC) $(HSI_CF) -c $*.c

all:	rpp.e

librpp.a::
	(cd rppfor ; make)

libf.a::
	(cd ratlibf ; make)

libc.a::
	(cd ratlibc ; make)

rpp.e:	rpp.o librpp.a libf.a libc.a
	$(CC) $(HSI_LF) rpp.o librpp.a libf.a libc.a $(HSI_F77LIBS) -o $@
	cp -p $@ ../../../bin/.

clean::
	(cd rppfor ; make clean)
	(cd ratlibf ; make clean)
	(cd ratlibc ; make clean)
	rm -f *.o rpp.e ../../../bin/rpp.e

