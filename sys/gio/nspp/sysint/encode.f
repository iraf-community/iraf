        subroutine encode (nchars, ftnfmt, ftnout, rval)

	character*11 ftnfmt, ftnout
	integer*2 sppfmt(12), sppout(12)
	integer SZFMT
	parameter (SZFMT=11)

c unpack the fortran character string, call fencd to actually encode the
c output string, then pack the output string into a fortran string for return
c
	call f77upk (ftnfmt, sppfmt, SZFMT)
	call fencd (nchars, sppfmt, sppout, rval)
	call f77pak (sppout, ftnout, SZFMT)

	end
