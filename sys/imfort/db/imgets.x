# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# IMGETS -- Get an image header parameter of type short integer.

short procedure imgets (im, key)

pointer	im			# image descriptor
char	key[ARB]		# parameter to be returned

long	lval, imgetl()
errchk	imgetl

begin
	lval = imgetl (im, key)
	if (IS_INDEFL(lval))
	    return (INDEFS)
	else
	    return (lval)
end
