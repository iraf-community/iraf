# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	<imio.h>

# IM_PMLNEV -- Test if a mask image line is nonempty.

bool procedure im_pmlnev (im, v)

pointer	im			#I image descriptor
long	v[IM_MAXDIM]		#I vector coordinates of image line

bool	pm_linenotempty()

begin
	return (pm_linenotempty (IM_PL(im), v))
end
