OBJS = addchr.o allblk.o alldig.o baderr.o balpar.o beginc.o brknxt.o	\
    cascod.o caslab.o declco.o deftok.o doarth.o docode.o doif.o	\
    doincr.o domac.o dostat.o dosub.o eatup.o elseif.o endcod.o		\
    entdef.o entdkw.o entfkw.o entrkw.o entxkw.o errchk.o errgo.o	\
    errorc.o evalr.o finit.o forcod.o fors.o getdef.o gettok.o		\
    gnbtok.o gocode.o gtok.o ifcode.o iferrc.o ifgo.o ifparm.o		\
    indent.o initkw.o labelc.o labgen.o lex.o litral.o lndict.o		\
    ludef.o mapid.o ngetch.o ogotos.o otherc.o outch.o outcon.o		\
    outdon.o outdwe.o outgo.o outnum.o outstr.o outtab.o parse.o	\
    pbnum.o pbstr.o poicod.o push.o putbak.o putchr.o puttok.o		\
    ratfor.o relate.o repcod.o retcod.o sdupl.o skpblk.o squash.o	\
    strdcl.o swcode.o swend.o swvar.o synerr.o thenco.o ulstal.o	\
    uniqid.o unstak.o untils.o whilec.o whiles.o

librpp.a: $(OBJS)
	ar r $@ $?

clean:
	rm -f $(OBJS) librpp.a

.PHONY: clean
