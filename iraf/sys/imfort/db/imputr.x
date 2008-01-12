# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# IMPUTR -- Put an image header parameter of type real.

procedure imputr (im, key, rval)

pointer	im			# image descriptor
char	key[ARB]		# parameter to be set
real	rval			# parameter value
double	dval

begin
	if (IS_INDEFR (rval))
	    dval = INDEFD
	else
	    dval = rval
	call imputd (im, key, dval)
end
