# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<pmset.h>
include	<imhdr.h>
include	<imio.h>
include	<plio.h>

# PM_NEWMASK -- Create an empty mask with the same dimensionality and size as
# the given reference image.

pointer procedure pm_newmask (ref_im, depth)

pointer	ref_im			#I reference image
int	depth			#I mask depth, bits

pointer	pl
pointer	pl_open()
errchk	pl_open

begin
	pl = pl_open (NULL)
	call pl_ssize (pl, IM_NDIM(ref_im), IM_SVLEN(ref_im,1), depth)

	PM_REFIM(pl) = ref_im
	PM_MAPXY(pl) = IM_SECTUSED(ref_im)

	return (pl)
end
