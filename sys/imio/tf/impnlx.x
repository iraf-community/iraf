# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	<imio.h>

# IMPNL -- Put the next line to an image of any dimension or datatype.
# This is a sequential operator.  The index vector V should be initialized 
# before the first call to the first line to be written.  Each call increments 
# the leftmost subscript by one, until V equals IM_LEN, at which time EOF
# is returned.  Subsequent writes are ignored.

int procedure impnlx (imdes, lineptr, v)

pointer	imdes
pointer	lineptr				# on output, points to the pixels
long	v[IM_MAXDIM]			# loop counter
int	npix
int	impnln()
extern	imflsx()
errchk	impnln

begin
	if (IM_FLUSH(imdes) == YES)
	    call zcall1 (IM_FLUSHEPA(imdes), imdes)

	npix = impnln (imdes, lineptr, v, TY_COMPLEX)
	if (IM_FLUSH(imdes) == YES)
	    call zlocpr (imflsx, IM_FLUSHEPA(imdes))

	return (npix)
end
