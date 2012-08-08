# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	<imio.h>

# IM_PMLNE3 -- Pixel mask line not empty.

bool procedure im_pmlne3 (im, lineno, bandno)

pointer	im			#I image descriptor
int	lineno			#I line number
int	bandno			#I band number

long	v[IM_MAXDIM]
bool	pm_linenotempty()

begin
	call amovkl (1, v, IM_MAXDIM)
	v[2] = lineno
	v[3] = bandno

	return (pm_linenotempty (IM_PL(im), v))
end
