
.c.o:	; $(CC) $(HSI_CF) -c $*.c

all:	mkpkg.e

mkpkg.e:	main.o char.o fdcache.o fncache.o host.o pkg.o scanlib.o \
		sflist.o tok.o
	$(CC) $(HSI_LF) -o $@ main.o char.o fdcache.o fncache.o host.o pkg.o scanlib.o sflist.o tok.o $(HSI_LIBS) 
	cp -p $@ ../../bin/.

clean::
	rm -f *.o mkpkg.e ../../bin/mkpkg.e
