# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	<imio.h>

# IM_PMSNEV -- Test if a mask image section is nonempty.

bool procedure im_pmsnev (im, vs, ve, ndim)

pointer	im			#I image descriptor
long	vs[IM_MAXDIM]		#I vector coordinates of start of section
long	ve[IM_MAXDIM]		#I vector coordinates of end of section
int	ndim			#I dimensionality of section

bool	pm_sectnotempty()

begin
	return (pm_sectnotempty (IM_PL(im), vs, ve, ndim))
end
