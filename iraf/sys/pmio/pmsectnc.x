# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <pmset.h>
include	<plio.h>

# PM_SECTNOTCONST -- Test whether the indicated mask image section is constant,
# i.e., all the mask pixels therein are set to the same value.  If so, return
# the mask value as an output argument.

bool procedure pm_sectnotconst (pl, vs, ve, ndim, mval)

pointer	pl			#I mask descriptor
long	vs[PM_MAXDIM]		#I starting coordinates of section
long	ve[PM_MAXDIM]		#I ending coordinates of section
int	ndim			#I dimension of section
int	mval			#O mask value

size_t	sz_val
long	lg_val
bool	pl_sectnotconst()
include	"pmio.com"

begin
	if (PM_MAPXY(pl) == YES) {
	    lg_val = 1
	    sz_val = PM_MAXDIM
	    call amovkl (lg_val, v1, sz_val)
	    sz_val = ndim
	    call amovl (vs, v1, sz_val)
	    call imaplv (PM_REFIM(pl), v1, v2, PM_MAXDIM)

	    lg_val = 1
	    sz_val = PM_MAXDIM
	    call amovkl (lg_val, v3, sz_val)
	    sz_val = ndim
	    call amovl (ve, v3, sz_val)
	    call imaplv (PM_REFIM(pl), v3, v4, PM_MAXDIM)

	    return (pl_sectnotconst (pl, v2, v4, PM_MAXDIM, mval))

	} else
	    return (pl_sectnotconst (pl, vs, ve, ndim, mval))
end
