# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# IMAKSCI -- Insert a new field to the image header after the given keyword
# and initialize to the value
# given.  It is not an error if the parameter already exists.

procedure imaksci (im, key, value, comment, pkey, baf)

pointer	im			# image descriptor
char	key[ARB]		# parameter or field value
short	value			# new or initial value of parameter
char	comment[ARB]		
char    pkey[ARB]               # Pivot keyword to insert 'key'
int     baf                     # I Insert BEFORE or AFTER

int	imaccf()
errchk	imaccf, iminfi

begin
	if (imaccf (im, key) == NO)
	    call iminfi (im, key, pkey, "s", baf)
	call impksc (im, key, value, comment)
end
