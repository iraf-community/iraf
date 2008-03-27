# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <imhdr.h>
include <pmset.h>
include	<plio.h>

# PM_LINENOTEMPTY -- Test whether the indicated mask image line is empty.

bool procedure pm_linenotempty (pl, v)

pointer	pl			#I mask descriptor
long	v[PM_MAXDIM]		#I coordinates of desired line

size_t	sz_val
bool	pl_sectnotempty(), pl_linenotempty()
include	"pmio.com"

begin
	if (PM_MAPXY(pl) == YES) {
	    call imaplv (PM_REFIM(pl), v, v1, PM_MAXDIM)
	    sz_val = PM_MAXDIM
	    call amovl (v, v2, sz_val)
	    v2[1] = IM_LEN(PM_REFIM(pl),1)
	    call imaplv (PM_REFIM(pl), v2, v3, PM_MAXDIM)

	    return (pl_sectnotempty (pl, v1, v3, PM_MAXDIM))

	} else
	    return (pl_linenotempty (pl, v))
end
