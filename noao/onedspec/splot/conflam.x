include	<error.h>
include	<smw.h>

define	VLIGHT		2.997925e18

# CONFLAM -- Convert to FLAMBDA from FNU

procedure conflam (sh)

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
		Memr[SY(sh)+i] = Memr[SY(sh)+i] * VLIGHT / lambda**2
	    }
	} then
	    call erract (EA_WARN)

	call un_close (ang)
end
