# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# IMSETI -- Set an IMIO parameter of type integer.

procedure imseti (im, param, value)

pointer	im			# image descriptor
int	param			# parameter to be set
int	value			# integer value of parameter

begin
	call imsetr (im, param, real(value))
end
