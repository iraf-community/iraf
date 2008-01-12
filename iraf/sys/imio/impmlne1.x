# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	<imio.h>

# IM_PMLNE1 -- Pixel mask line not empty.

bool procedure im_pmlne1 (im)

pointer	im			#I image descriptor
long	v[IM_MAXDIM]

bool	pm_linenotempty()

begin
	call amovkl (1, v, IM_MAXDIM)
	return (pm_linenotempty (IM_PL(im), v))
end
