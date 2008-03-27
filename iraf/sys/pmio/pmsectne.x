# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <pmset.h>
include	<plio.h>

# PM_SECTNOTEMPTY -- Test whether the indicated mask image section is empty.

bool procedure pm_sectnotempty (pl, vs, ve, ndim)

pointer	pl			#I mask descriptor
long	vs[PM_MAXDIM]		#I starting coordinates of section
long	ve[PM_MAXDIM]		#I ending coordinates of section
int	ndim

size_t	sz_val
long	c_1
bool	pl_sectnotempty()
include	"pmio.com"

begin
	c_1 = 1

	if (PM_MAPXY(pl) == YES) {
	    sz_val = PM_MAXDIM
	    call amovkl (c_1, v1, sz_val)
	    sz_val = ndim
	    call amovl (vs, v1, sz_val)
	    call imaplv (PM_REFIM(pl), v1, v2, PM_MAXDIM)

	    sz_val = PM_MAXDIM
	    call amovkl (c_1, v3, sz_val)
	    sz_val = ndim
	    call amovl (ve, v3, sz_val)
	    call imaplv (PM_REFIM(pl), v3, v4, PM_MAXDIM)

	    return (pl_sectnotempty (pl, v2, v4, PM_MAXDIM))

	} else
	    return (pl_sectnotempty (pl, vs, ve, ndim))
end
