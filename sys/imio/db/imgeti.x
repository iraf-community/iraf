# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# IMGETI -- Get an image header parameter of type integer.

int procedure imgeti (im, key)

pointer	im			# image descriptor
char	key[ARB]		# parameter to be returned

long	lval, imgetl()
errchk	imgetl

begin
	lval = imgetl (im, key)
	if (IS_INDEFL(lval))
	    return (INDEFI)
	else
	    return (lval)
end
