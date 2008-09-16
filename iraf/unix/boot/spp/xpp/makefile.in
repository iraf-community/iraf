
.c.o:	; $(CC) $(HSI_CF) -c $*.c

all:	xpp.e

xpp.e:	xppmain.o lexyy.o xppcode.o decl.o
	$(CC) $(HSI_LF) -o $@ xppmain.o lexyy.o xppcode.o decl.o $(HSI_LIBS)
	cp -p $@ ../../../bin/.

clean::
	rm -f *.o xpp.e ../../../bin/xpp.e
