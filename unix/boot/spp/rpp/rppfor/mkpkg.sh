# Fortran source for RPP preprocessor.

f77 -c $HSI_FF	addchr.f allblk.f alldig.f baderr.f balpar.f beginc.f
f77 -c $HSI_FF	brknxt.f cascod.f caslab.f declco.f deftok.f doarth.f
f77 -c $HSI_FF	docode.f doif.f doincr.f domac.f dostat.f dosub.f
f77 -c $HSI_FF	eatup.f elseif.f endcod.f entdef.f entdkw.f entfkw.f
f77 -c $HSI_FF	entrkw.f entxkw.f errchk.f errgo.f errorc.f evalr.f
f77 -c $HSI_FF	finit.f forcod.f fors.f getdef.f gettok.f gnbtok.f
f77 -c $HSI_FF	gocode.f gtok.f ifcode.f iferrc.f ifgo.f ifparm.f
f77 -c $HSI_FF	indent.f initkw.f labelc.f labgen.f lex.f litral.f
f77 -c $HSI_FF	lndict.f ludef.f mapid.f ngetch.f ogotos.f otherc.f
f77 -c $HSI_FF	outch.f outcon.f outdon.f outdwe.f outgo.f outnum.f
f77 -c $HSI_FF	outstr.f outtab.f parse.f pbnum.f pbstr.f poicod.f
f77 -c $HSI_FF	push.f putbak.f putchr.f puttok.f ratfor.f relate.f
f77 -c $HSI_FF	repcod.f retcod.f sdupl.f skpblk.f squash.f strdcl.f
f77 -c $HSI_FF	swcode.f swend.f swvar.f synerr.f thenco.f ulstal.f
f77 -c $HSI_FF	uniqid.f unstak.f untils.f whilec.f whiles.f

ar rv		librpp.a *.o
ranlib		librpp.a
mv -f		librpp.a ..
rm		*.o
