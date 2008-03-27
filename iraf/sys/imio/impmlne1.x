# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	<imio.h>

# IM_PMLNE1 -- Pixel mask line not empty.

bool procedure im_pmlne1 (im)

pointer	im			#I image descriptor

size_t	sz_val
long	lval
long	v[IM_MAXDIM]

bool	pm_linenotempty()

begin
	lval = 1
	sz_val = IM_MAXDIM
	call amovkl (lval, v, sz_val)
	return (pm_linenotempty (IM_PL(im), v))
end
