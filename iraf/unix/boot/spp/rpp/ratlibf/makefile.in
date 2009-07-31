
.f.o:	; F2C=$(F2C) CFLAGS='$(HSI_CF)' $(F77) $(HSI_FF) -c $*.f

all:	libf.a

libf.a:	addset.o addstr.o amatch.o catsub.o clower.o concat.o \
	ctoc.o ctoi.o ctomn.o cupper.o delete.o docant.o dodash.o \
	dsdbiu.o dsdump.o dsfree.o dsget.o dsinit.o enter.o equal.o \
	error.o errsub.o esc.o fcopy.o filset.o fmtdat.o fold.o \
	gctoi.o getc.o getccl.o getpat.o getwrd.o gfnarg.o index.o \
	insub.o itoc.o length.o locate.o lookup.o lower.o makpat.o \
	maksub.o match.o mktabl.o mntoc.o omatch.o outsub.o patsiz.o \
	prompt.o putc.o putdec.o putint.o putstr.o query.o rmtabl.o \
	scopy.o sctabl.o sdrop.o skipbl.o slstr.o stake.o stclos.o \
	stcopy.o stlu.o strcmp.o strim.o termin.o trmout.o type.o \
	upper.o wkday.o
	ar r $@ $?
	$(RANLIB) $@
	cp -p $@ ../.

clean::
	rm -f *.o *.c libf.a ../libf.a
