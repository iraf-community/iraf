# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# IMPKBC -- Put an image header parameter of type boolean.

procedure impkbc (im, key, bval, comment)

pointer	im			# image descriptor
char	key[ARB]		# parameter to be set
bool	bval			# parameter value
char	comment[ARB]		# 
char	sval[2]

begin
	if (bval)
	    sval[1] = 'T'
	else
	    sval[1] = 'F'
	sval[2] = EOS

	call impstrc (im, key, sval, comment)
end
