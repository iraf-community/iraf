# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	"mio.h"

# MIO_SETRANGE -- Set the region of the image to be accessed, and rewind the
# i/o pointer to the beginning of the specified region.

procedure mio_setrange (mp, vs, ve, ndim)

pointer	mp			#I MIO descriptor
long	vs[IM_MAXDIM]		#I vector coords of start of region
long	ve[IM_MAXDIM]		#I vector coords of end of region
int	ndim			#I dimensionality of region

int	i
int	btoi()

begin
	do i = 1, IM_MAXDIM
	    if (i <= ndim) {
		M_VS(mp,i) = min (vs[i], ve[i])
		M_VN(mp,i) = abs (ve[i] - vs[i]) + 1
	    } else {
		M_VS(mp,i) = 1
		M_VN(mp,i) = 1
	    }

	M_LINEIO(mp) = btoi (vs[1] == 1 && ve[1] == IM_LEN(M_IM(mp),1))
	M_REGCOORDS(mp) = YES
	M_ACTIVE(mp) = NO
	M_NDIM(mp) = ndim
end
