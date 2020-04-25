# Ratfor source for the SPP preprocessor.  A TOOLS compatible ratfor compiler
# is required to compile this.  The original UNIX ratfor compiler may not do
# the job.
RATFOR = ratfor

.r.f:
	${RATFOR} $*.r > $*.f

SRCS=	addchr.r allblk.r alldig.r baderr.r balpar.r beginc.r brknxt.r\
	cascod.r caslab.r declco.r deftok.r doarth.r docode.r doif.r\
	doincr.r domac.r dostat.r dosub.r eatup.r elseif.r endcod.r\
	entdef.r entdkw.r entfkw.r entrkw.r entxkw.r errchk.r errgo.r\
	errorc.r evalr.r finit.r forcod.r fors.r getdef.r gettok.r\
	gnbtok.r gocode.r gtok.r ifcode.r iferrc.r ifgo.r ifparm.r\
	indent.r initkw.r labelc.r labgen.r lex.r litral.r lndict.r\
	ludef.r mapid.r ngetch.r ogotos.r otherc.r outch.r outcon.r\
	outdon.r outdwe.r outgo.r outnum.r outstr.r outtab.r parse.r\
	pbnum.r pbstr.r poicod.r push.r putbak.r putchr.r puttok.r\
	ratfor.r relate.r repcod.r retcod.r sdupl.r skpblk.r squash.r\
	strdcl.r swcode.r swend.r swvar.r synerr.r thenco.r ulstal.r\
	uniqid.r unstak.r untils.r whilec.r whiles.r

FORT=	addchr.f allblk.f alldig.f baderr.f balpar.f beginc.f brknxt.f\
	cascod.f caslab.f declco.f deftok.f doarth.f docode.f doif.f\
	doincr.f domac.f dostat.f dosub.f eatup.f elseif.f endcod.f\
	entdef.f entdkw.f entfkw.f entrkw.f entxkw.f errchk.f errgo.f\
	errorc.f evalr.f finit.f forcod.f fors.f getdef.f gettok.f\
	gnbtok.f gocode.f gtok.f ifcode.f iferrc.f ifgo.f ifparm.f\
	indent.f initkw.f labelc.f labgen.f lex.f litral.f lndict.f\
	ludef.f mapid.f ngetch.f ogotos.f otherc.f outch.f outcon.f\
	outdon.f outdwe.f outgo.f outnum.f outstr.f outtab.f parse.f\
	pbnum.f pbstr.f poicod.f push.f putbak.f putchr.f puttok.f\
	ratfor.f relate.f repcod.f retcod.f sdupl.f skpblk.f squash.f\
	strdcl.f swcode.f swend.f swvar.f synerr.f thenco.f ulstal.f\
	uniqid.f unstak.f untils.f whilec.f whiles.f

fort:	$(SRCS) common defs
	make fsrc
	mv *.f ../rppfor
	touch fort

fsrc:	$(FORT)
