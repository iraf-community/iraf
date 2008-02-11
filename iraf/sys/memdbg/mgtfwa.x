# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<config.h>
include	<mach.h>

# MGTFWA -- Given a user buffer pointer, retrieve physical address of buffer.
# If physical address of buffer does not seem reasonable, memory has probably
# been overwritten, a fatal error.

pointer procedure mgtfwa (ptr, dtype)

pointer	ptr
int	dtype

pointer	bufptr, locbuf, fwa
size_t	almf
pointer	coerce()

begin
	bufptr = coerce (ptr, dtype, TY_POINTER)
	fwa = Memp[bufptr-1]
	call zlocva (Memp[bufptr-1], locbuf)

	almf = locbuf - fwa
	if ( almf < 0 ) {
	    almf = 0 - almf
	}
	if (almf > SZ_VMEMALIGN)
	    call sys_panic (SYS_MCORRUPTED, "Memory has been corrupted")

	return (fwa)
end
