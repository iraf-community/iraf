# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <pmset.h>
include	<plio.h>

# PM_SECTNOTEMPTY -- Test whether the indicated mask image section is empty.

bool procedure pm_sectnotempty (pl, vs, ve, ndim)

pointer	pl			#I mask descriptor
long	vs[PM_MAXDIM]		#I starting coordinates of section
long	ve[PM_MAXDIM]		#I ending coordinates of section
int	ndim

bool	pl_sectnotempty()
include	"pmio.com"

begin
	if (PM_MAPXY(pl) == YES) {
	    call amovkl ( 1, v1, PM_MAXDIM)
	    call amovl (vs, v1, ndim)
	    call imaplv (PM_REFIM(pl), v1, v2, PM_MAXDIM)

	    call amovkl ( 1, v3, PM_MAXDIM)
	    call amovl (ve, v3, ndim)
	    call imaplv (PM_REFIM(pl), v3, v4, PM_MAXDIM)

	    return (pl_sectnotempty (pl, v2, v4, PM_MAXDIM))

	} else
	    return (pl_sectnotempty (pl, vs, ve, ndim))
end
