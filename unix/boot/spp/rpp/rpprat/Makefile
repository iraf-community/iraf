# Ratfor source for the SPP preprocessor.  A TOOLS compatible ratfor compiler
# is required to compile this.  The original UNIX ratfor compiler may not do
# the job.

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

# NOTE -- After regenerating the fortran CASLAB.F, comment out the unreachable
# goto on line 32, generated due to a bug in the ratfor.

fort:	$(SRCS)
	find $(SRCS) -newer fort -print | sed -e 's/.*/f77 -F &/' | sh
	mv *.f ../rppfor; touch fort
