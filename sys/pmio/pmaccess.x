# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <pmset.h>
include <plio.h>

# PM_ACCESS -- Return a pointer (llbuf offset) to the indicated mask image
# line.  A valid offset is always returned; if the mask line is empty, the
# offset will be that of the "empty line" linelist.

int procedure pm_access (pl, v)

pointer	pl			#I mask descriptor
long	v[PM_MAXDIM]		#I coordinates of desired line

int	pl_access()
include	"pmio.com"

begin
	if (PM_MAPXY(pl) == YES) {
	    call imaplv (PM_REFIM(pl), v, v1, PM_MAXDIM)
	    return (pl_access (pl, v1))
	} else
	    return (pl_access (pl, v))
end
