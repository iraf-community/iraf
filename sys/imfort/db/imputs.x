# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# IMPUTS -- Put an image header parameter of type short integer.

procedure imputs (im, key, sval)

pointer	im			# image descriptor
char	key[ARB]		# parameter to be set
short	sval			# parameter value
long	lval

begin
	if (IS_INDEFS (sval))
	    lval = INDEFL
	else
	    lval = sval
	call imputl (im, key, lval)
end
