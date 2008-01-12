# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>

# IMGNL -- Get the next line from an image of any dimension or datatype.
# This is a sequential operator.  The index vector V should be initialized
# to the first line to be read before the first call.  Each call increments
# the leftmost subscript by one, until V equals IM_LEN, at which time EOF
# is returned.

int procedure imgnld (imdes, lineptr, v)

pointer	imdes
pointer	lineptr				# on output, points to the pixels
long	v[IM_MAXDIM]			# loop counter
int	npix, dtype, imgnln()
errchk	imgnln

begin
	npix = imgnln (imdes, lineptr, v, TY_DOUBLE)

	if (npix != EOF) {
	    dtype = IM_PIXTYPE(imdes)
	    if (dtype != TY_DOUBLE)
		call imupkd (Memd[lineptr], Memd[lineptr], npix, dtype)
	}

	return (npix)
end
