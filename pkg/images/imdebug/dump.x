# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	<ctype.h>

# DUMP -- Dump the user area of an image header for diagnostic purposes.
# Blanks are rendered into underscores to make them visible.  This is a
# throwaway task.

procedure t_dump()

char	image[SZ_FNAME]
int	i
pointer	ip, im
pointer	immap()

begin
	call clgstr ("image", image, SZ_FNAME)
	im = immap (image, READ_ONLY, 0)

	# Print ruler.
	do i = 1, 80
	    if (mod(i,10) == 0)
		call putci (STDOUT, TO_DIGIT(i/10))
	    else
		call putci (STDOUT, ' ')
	call putci (STDOUT, '\n')

	do i = 1, 80
	    call putci (STDOUT, TO_DIGIT(mod(i,10)))
	call putci (STDOUT, '\n')

	# Map blanks into underscores.
	for (ip = IM_USERAREA(im);  Memc[ip] != EOS;  ip=ip+1)
	    if (Memc[ip] == ' ')
		Memc[ip] = '_'

	# Dump user area.
	call putline (STDOUT, Memc[IM_USERAREA(im)])
	call imunmap (im)
end
