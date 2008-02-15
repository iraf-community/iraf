# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	<imio.h>

# IM_PMLNE1 -- Pixel mask line not empty.

bool procedure im_pmlne1 (im)

pointer	im			#I image descriptor
long	v[IM_MAXDIM]

size_t	sz_val
long	lg_val
bool	pm_linenotempty()

begin
	lg_val = 1
	sz_val = IM_MAXDIM
	call amovkl (lg_val, v, sz_val)
	return (pm_linenotempty (IM_PL(im), v))
end
