# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# IMASTR -- Add a new field to the image header and initialize to the value
# given.  It is not an error if the parameter already exists.

procedure imastr (im, key, value, comment)

pointer	im			# image descriptor
char	key[ARB]		# parameter or field value
char	value[ARB]		# new or initial value of parameter
char	comment[ARB]		# comment string
int	imaccf()

begin
	if (imaccf (im, key) == NO)
	    call imaddf (im, key, TY_CHAR, comment)
	call impstr (im, key, value)
end
