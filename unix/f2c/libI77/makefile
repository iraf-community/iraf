.SUFFIXES: .c .o
CC = cc
CFLAGS = -O
SHELL = /bin/sh

# compile, then strip unnecessary symbols
.c.o:
	$(CC) -c -DSkip_f2c_Undefs $(CFLAGS) $*.c
	ld -r -x -o $*.xxx $*.o
	mv $*.xxx $*.o
## Under Solaris (and other systems that do not understand ld -x),
## omit -x in the ld line above.
## If your system does not have the ld command, comment out
## or remove both the ld and mv lines above.

# To get signed zeros in write statements on IEEE-arithmetic systems,
# add -DSIGNED_ZEROS to the CFLAGS assignment above and add signbit.o
# to the end of the "OBJ =" assignment below.  Also copy or link
# libF77/arith.h to this directory (after "make arith.h" if necessary
# in the libF77 directory).  It's simpler to do things all at once
# with libf2c.zip and its makefile.u.

OBJ =	backspace.o close.o dfe.o dolio.o due.o endfile.o err.o fmt.o \
	fmtlib.o ftell_.o i77vers.o iio.o ilnw.o inquire.o lread.o lwrite.o \
	open.o rdfmt.o rewind.o rsfe.o rsli.o rsne.o sfe.o sue.o typesize.o \
	uio.o util.o wref.o wrtfmt.o wsfe.o wsle.o wsne.o xwsne.o

all: sysdep1.h libI77.a

libI77.a:	$(OBJ)
		ar r libI77.a $?
		ranlib libI77.a || true

### If your system lacks ranlib, you don't need it; see README.

install:	libI77.a
	cp libI77.a $(LIBDIR)/libI77.a
	ranlib $(LIBDIR)/libI77.a || true

# i77vers.c was "Version.c"; renamed on 20010623 to accord with libf2c.zip.

i77vers.o: i77vers.c
	$(CC) -c i77vers.c

# To compile with C++, first "make f2c.h"
f2c.h: f2ch.add
	cat /usr/include/f2c.h f2ch.add >f2c.h


clean:
	rm -f $(OBJ) libI77.a

clobber:	clean
	rm -f libI77.a

backspace.o:	fio.h
close.o:	fio.h
dfe.o:		fio.h
dfe.o:		fmt.h
due.o:		fio.h
endfile.o:	fio.h rawio.h
err.o:		fio.h rawio.h
fmt.o:		fio.h
fmt.o:		fmt.h
ftell_.o:	fio.h
ftell64_.o:	fio.h
iio.o:		fio.h
iio.o:		fmt.h
ilnw.o:		fio.h
ilnw.o:		lio.h
inquire.o:	fio.h
lread.o:	fio.h
lread.o:	fmt.h
lread.o:	lio.h
lread.o:	fp.h
lwrite.o:	fio.h
lwrite.o:	fmt.h
lwrite.o:	lio.h
open.o:		fio.h rawio.h
rdfmt.o:	fio.h
rdfmt.o:	fmt.h
rdfmt.o:	fp.h
rewind.o:	fio.h
rsfe.o:		fio.h
rsfe.o:		fmt.h
rsli.o:		fio.h
rsli.o:		lio.h
rsne.o:		fio.h
rsne.o:		lio.h
sfe.o:		fio.h
sue.o:		fio.h
uio.o:		fio.h
util.o:		fio.h
wref.o:		fio.h
wref.o:		fmt.h
wref.o:		fp.h
wrtfmt.o:	fio.h
wrtfmt.o:	fmt.h
wsfe.o:		fio.h
wsfe.o:		fmt.h
wsle.o:		fio.h
wsle.o:		fmt.h
wsle.o:		lio.h
wsne.o:		fio.h
wsne.o:		lio.h
xwsne.o:	fio.h
xwsne.o:	lio.h
xwsne.o:	fmt.h

sysdep1.h: sysdep1.h0
	cp sysdep1.h0 sysdep1.h

check:
	xsum Notice README backspace.c close.c dfe.c dolio.c due.c \
	endfile.c err.c f2ch.add fio.h fmt.c fmt.h fmtlib.c fp.h ftell_.c \
	ftell64_.c i77vers.c iio.c ilnw.c inquire.c lio.h lread.c lwrite.c \
	makefile open.c rawio.h rdfmt.c rewind.c rsfe.c rsli.c rsne.c sfe.c \
	sue.c typesize.c uio.c util.c wref.c wrtfmt.c wsfe.c wsle.c wsne.c \
	xwsne.c >zap
	cmp zap libI77.xsum && rm zap || diff libI77.xsum zap
