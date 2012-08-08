# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# IMADDL -- Add a new field to the image header and initialize to the value
# given.  It is not an error if the parameter already exists.

procedure imaddl (im, key, value, comment)

pointer	im			# image descriptor
char	key[ARB]		# parameter or field value
long	value			# new or initial value of parameter
char	comment[ARB]		# comment describing new parameter

int	imaccf()
errchk	imaccf, imaddf

begin
	if (imaccf (im, key) == NO)
	    call imaddf (im, key, TY_LONG, comment)
	call imputl (im, key, value)
end
