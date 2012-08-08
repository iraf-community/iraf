# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# IMGETC -- Get an image header parameter of type char.

char procedure imgetc (im, key)

pointer	im			# image descriptor
char	key[ARB]		# parameter to be returned
long	imgetl()

begin
	return (imgetl (im, key))
end
