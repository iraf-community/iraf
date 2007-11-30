# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# IMASTRC -- Add a new field to the image header and initialize to the value
# given.  It is not an error if the parameter already exists.

procedure imastrc (im, key, value, comment)

pointer	im			# image descriptor
char	key[ARB]		# parameter or field value
char	value[ARB]		# new or initial value of parameter
char	comment[ARB]		# 

int	imaccf()
errchk	imaccf, imaddf

begin
	if (imaccf (im, key) == NO)
	    call imaddf (im, key, "c")
	call impstrc (im, key, value, comment)
end
