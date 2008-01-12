# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# IMGETR -- Get an image header parameter of type real.

real procedure imgetr (im, key)

pointer	im			# image descriptor
char	key[ARB]		# parameter to be returned

double	dval, imgetd()
errchk	imgetd

begin
	dval = imgetd (im, key)
	if (IS_INDEFD(dval))
	    return (INDEFR)
	else
	    return (dval)
end
