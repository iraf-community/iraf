# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# IMADDB -- Add a new field to the image header and initialize to the value
# given.  It is not an error if the parameter already exists.

procedure imaddb (im, key, value)

pointer	im			# image descriptor
char	key[ARB]		# parameter or field value
bool	value			# new or initial value of parameter

int	imaccf()
errchk	imaccf, imaddf

begin
	if (imaccf (im, key) == NO)
	    call imaddf (im, key, "b")
	call imputb (im, key, value)
end
