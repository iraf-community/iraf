# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	<imio.h>

# IM_PMLNE2 -- Pixel mask line not empty.

bool procedure im_pmlne2 (im, lineno)

pointer	im			#I image descriptor
int	lineno			#I line number

long	v[IM_MAXDIM]
bool	pm_linenotempty()

begin
	call amovkl (1, v, IM_MAXDIM)
	v[2] = lineno

	return (pm_linenotempty (IM_PL(im), v))
end
