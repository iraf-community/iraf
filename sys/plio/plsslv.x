# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <plset.h>
include	<plio.h>

# PLSSLV -- Given two vectors (VS, VN) defining the starting coordinates and
# size of an image section, initialize the "loop index" vector V, and the
# loop-end vector VE.

procedure plsslv (pl, vs, vn, v, ve)

pointer	pl			#I mask descriptor
long	vs[PL_MAXDIM]		#I vector coordinates of start of section
long	vn[PL_MAXDIM]		#I vector size of section
long	v[PL_MAXDIM]		#O vector for i/o (vector loop index)
long	ve[PL_MAXDIM]		#O vector coordinates of end of section

int	i

begin
	do i = 1, PL_NAXES(pl) {
	     v[i] = vs[i]
	    ve[i] = vs[i] + vn[i] - 1
	}
end
