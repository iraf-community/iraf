# Utility library subroutines for RPP.

f77 -c $HSI_FF	addset.f addstr.f amatch.f catsub.f clower.f concat.f
f77 -c $HSI_FF	ctoc.f ctoi.f ctomn.f cupper.f delete.f docant.f dodash.f
f77 -c $HSI_FF	dsdbiu.f dsdump.f dsfree.f dsget.f dsinit.f enter.f equal.f
f77 -c $HSI_FF	error.f errsub.f esc.f fcopy.f filset.f fmtdat.f fold.f
f77 -c $HSI_FF	gctoi.f getc.f getccl.f getpat.f getwrd.f gfnarg.f index.f
f77 -c $HSI_FF	insub.f itoc.f length.f locate.f lookup.f lower.f makpat.f
f77 -c $HSI_FF	maksub.f match.f mktabl.f mntoc.f omatch.f outsub.f patsiz.f
f77 -c $HSI_FF	prompt.f putc.f putdec.f putint.f putstr.f query.f rmtabl.f
f77 -c $HSI_FF	scopy.f sctabl.f sdrop.f skipbl.f slstr.f stake.f stclos.f
f77 -c $HSI_FF	stcopy.f stlu.f strcmp.f strim.f termin.f trmout.f type.f
f77 -c $HSI_FF	upper.f wkday.f

ar rv		libf.a *.o
ranlib		libf.a
mv -f		libf.a ..
rm		*.o
