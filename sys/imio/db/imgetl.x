# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# IMGETL -- Get an image header parameter of type long integer.

long procedure imgetl (im, key)

pointer	im			# image descriptor
char	key[ARB]		# parameter to be returned

double	dval, imgetd()
errchk	imgetd

begin
	dval = imgetd (im, key)
	if (IS_INDEFD(dval))
	    return (INDEFL)
	else
	    return (nint (dval))
end
