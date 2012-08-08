# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<config.h>
include	<mach.h>

# MGTLWL -- Given a user buffer pointer, retrieve location of the last word.

int procedure mgtlwl (ptr, dtype)

pointer	ptr, bufptr
int	dtype
int	coerce()

begin
	bufptr = coerce (ptr, dtype, TY_INT)
	return (Memi[bufptr-4])
end
