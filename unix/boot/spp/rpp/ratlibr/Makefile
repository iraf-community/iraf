# Ratfor source for the ratfor library.  A TOOLS compatible ratfor compiler
# is required to compile this.  The original UNIX ratfor compiler may not do
# the job.
RATFOR = ratfor

.r.f:
	${RATFOR} $*.r > $*.f

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

FORT=	addset.f addstr.f amatch.f catsub.f clower.f concat.f ctoc.f\
	ctoi.f ctomn.f cupper.f delete.f docant.f dodash.f dsdbiu.f\
	dsdump.f dsfree.f dsget.f dsinit.f enter.f equal.f error.f\
	errsub.f esc.f fcopy.f filset.f fmtdat.f fold.f gctoi.f getc.f\
	getccl.f getpat.f getwrd.f gfnarg.f index.f insub.f\
	itoc.f length.f locate.f lookup.f lower.f makpat.f maksub.f\
	match.f mktabl.f mntoc.f omatch.f outsub.f patsiz.f prompt.f\
	putc.f putdec.f putint.f putstr.f query.f rmtabl.f scopy.f\
	sctabl.f sdrop.f skipbl.f slstr.f stake.f stclos.f stcopy.f\
	stlu.f strcmp.f strim.f termin.f trmout.f type.f upper.f wkday.f

fort:	$(SRCS) defs
	make fsrc
	mv *.f ../ratlibf
	touch fort

fsrc:	$(FORT)
