# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# IMPUTI -- Put an image header parameter of type integer.

procedure imputi (im, key, ival)

pointer	im			# image descriptor
char	key[ARB]		# parameter to be set
int	ival			# parameter value
long	lval

begin
	if (IS_INDEFI (ival))
	    lval = INDEFL
	else
	    lval = ival
	call imputl (im, key, lval)
end
