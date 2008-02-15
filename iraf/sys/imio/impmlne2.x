# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	<imio.h>

# IM_PMLNE2 -- Pixel mask line not empty.

bool procedure im_pmlne2 (im, lineno)

pointer	im			#I image descriptor
int	lineno			#I line number

size_t	sz_val
long	lg_val
long	v[IM_MAXDIM]
bool	pm_linenotempty()

begin
	lg_val = 1
	sz_val = IM_MAXDIM
	call amovkl (lg_val, v, sz_val)
	v[2] = lineno

	return (pm_linenotempty (IM_PL(im), v))
end
