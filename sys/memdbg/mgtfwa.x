# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<config.h>
include	<mach.h>

# MGTFWA -- Given a user buffer pointer, retrieve physical address of buffer.
# If physical address of buffer does not seem reasonable, memory has probably
# been overwritten, a fatal error.

int procedure mgtfwa (ptr, dtype)

pointer	ptr, bufptr
int	dtype
int	locbuf, fwa
int	coerce()

begin
	bufptr = coerce (ptr, dtype, TY_INT)
	fwa = Memi[bufptr-1]
	call zlocva (Memi[bufptr-1], locbuf)

	if (abs (locbuf - fwa) > SZ_VMEMALIGN)
	    call sys_panic (SYS_MCORRUPTED, "Memory has been corrupted")

	return (fwa)
end
