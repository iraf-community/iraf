# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# IMADDR -- Add a new field to the image header and initialize to the value
# given.  It is not an error if the parameter already exists.

procedure imaddr (im, key, value, comment)

pointer	im			# image descriptor
char	key[ARB]		# parameter or field value
real	value			# new or initial value of parameter
char	comment[ARB]		# comment describing new parameter

int	imaccf()
errchk	imaccf, imaddf

begin
	if (imaccf (im, key) == NO)
	    call imaddf (im, key, TY_REAL, comment)
	call imputr (im, key, value)
end
