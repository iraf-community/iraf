# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <pmset.h>
include <plio.h>

# PM_ACCESS -- Return a pointer (type short) to the indicated mask image
# line.  A valid pointer is always returned; if the mask line is empty, the
# pointer will reference the "empty line" linelist.

pointer procedure pm_access (pl, v)

pointer	pl			#I mask descriptor
long	v[PM_MAXDIM]		#I coordinates of desired line

pointer	pl_access()
include	"pmio.com"

begin
	if (PM_MAPXY(pl) == YES) {
	    call imaplv (PM_REFIM(pl), v, v1, PM_MAXDIM)
	    return (pl_access (pl, v1))
	} else
	    return (pl_access (pl, v))
end
