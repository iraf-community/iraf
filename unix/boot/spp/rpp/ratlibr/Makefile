# Ratfor source for the ratfor library.  A TOOLS compatible ratfor compiler
# is required to compile this.  The original UNIX ratfor compiler may not do
# the job.

SRCS=	addset.r addstr.r amatch.r catsub.r clower.r concat.r ctoc.r\
	ctoi.r ctomn.r cupper.r delete.r docant.r dodash.r dsdbiu.r\
	dsdump.r dsfree.r dsget.r dsinit.r enter.r equal.r error.r\
	errsub.r esc.r fcopy.r filset.r fmtdat.r fold.r gctoi.r getc.r\
	getccl.r getpat.r getwrd.r gfnarg.r index.r insub.r\
	itoc.r length.r locate.r lookup.r lower.r makpat.r maksub.r\
	match.r mktabl.r mntoc.r omatch.r outsub.r patsiz.r prompt.r\
	putc.r putdec.r putint.r putstr.r query.r rmtabl.r scopy.r\
	sctabl.r sdrop.r skipbl.r slstr.r stake.r stclos.r stcopy.r\
	stlu.r strcmp.r strim.r termin.r trmout.r type.r upper.r wkday.r

fort:	$(SRCS)
	find $(SRCS) -newer fort -print | sed -e 's/.*/f77 -F &/' | sh
	mv *.f ../ratlibf; touch fort
