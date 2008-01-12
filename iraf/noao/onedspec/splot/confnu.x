include	<error.h>
include	<smw.h>

define	VLIGHT		2.997925e18

# CONFNU -- Convert to FNU from FLAMBDA

procedure confnu (sh)

pointer	sh			# SHDR pointer

int	i
real	lambda
pointer	ang, un_open()
errchk	un_open, un_ctranr

begin
	ang = un_open ("angstroms")
	iferr {
	    do i = 0, SN(sh)-1 {
		call un_ctranr (UN(sh), ang, Memr[SX(sh)+i], lambda, 1)
		Memr[SY(sh)+i] = Memr[SY(sh)+i] * lambda**2 / VLIGHT
	    }
	} then
	    call erract (EA_WARN)

	call un_close (ang)
end
