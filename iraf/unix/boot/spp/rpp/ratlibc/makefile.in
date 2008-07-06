
.c.o:	; $(CC) $(HSI_CF) -c $*.c

all:	libc.a

libc.a:	cant.o close.o endst.o getarg.o getlin.o initst.o open.o \
	putch.o putlin.o r4tocstr.o remark.o
	ar r $@ $?
	$(RANLIB) $@
	cp -p $@ ../.

clean::
	rm -f *.o libc.a ../libc.a

