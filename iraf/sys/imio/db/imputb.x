# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# IMPUTB -- Put an image header parameter of type boolean.

procedure imputb (im, key, bval)

pointer	im			# image descriptor
char	key[ARB]		# parameter to be set
bool	bval			# parameter value
char	sval[2]

begin
	if (bval)
	    sval[1] = 'T'
	else
	    sval[1] = 'F'
	sval[2] = EOS

	call impstr (im, key, sval)
end
