# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# IMADDD -- Add a new field to the image header and initialize to the value
# given.  It is not an error if the parameter already exists.

procedure imaddd (im, key, value, comment)

pointer	im			# image descriptor
char	key[ARB]		# parameter or field value
double	value			# new or initial value of parameter
char	comment[ARB]		# comment describing new parameter

int	imaccf()
errchk	imaccf, imaddf

begin
	if (imaccf (im, key) == NO)
	    call imaddf (im, key, TY_DOUBLE, comment)
	call imputd (im, key, value)
end
