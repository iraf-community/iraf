# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# IMMAP -- Map an imagefile to an image structure.  This is the "open"
# procedure for an imagefile.

pointer procedure immap (imspec, acmode, hdr_arg)

char	imspec[ARB]		#I image specification
int	acmode			#I image access mode
int	hdr_arg			#I length of user fields, or header pointer

pointer	immapz()
errchk	iki_init

begin
	call iki_init()
	return (immapz (imspec, acmode, hdr_arg))
end
