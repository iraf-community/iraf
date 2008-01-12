# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<pmset.h>
include	"mio.h"

# MIO_OPEN -- Open a pixel mask for masked i/o on the given data image.
# The data image also serves as the reference image for coordinate (section)
# transformations.

pointer procedure mio_open (mask, flags, im)

char	mask[ARB]		#I mask name
int	flags			#I flag bits
pointer	im			#I data (and reference) image

pointer	pm, mp
char	title[1]
pointer	im_pmopen(), mio_openo()
errchk	im_pmopen

begin
	pm = im_pmopen (mask, flags, title, 0, im)
	mp = mio_openo (pm, im)

	M_PMCLOSE(mp) = YES
	M_DEPTH(mp) = PM_MAXDEPTH
	if (and (flags, BOOLEAN_MASK) != 0)
	    M_DEPTH(mp) = 1

	return (mp)
end
