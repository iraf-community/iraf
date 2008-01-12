# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <pmset.h>
include	<plio.h>

# PM_POINT -- Perform a rasterop operation on a single point in a line of a
# 2-dimensional plane of a mask.  If the dimensionality of the mask exceeds 2,
# the pm_setplane() procedure should be called first to define the plane of
# the mask to be modified.

procedure pm_point (pl, x, y, rop)

pointer	pl			#I mask descriptor
int	x			#I pixel to be modified
int	y			#I line to be modified
int	rop			#I rasterop defining operation

errchk	pl_getplane
include	"pmio.com"

begin
	if (PM_MAPXY(pl) == YES) {
	    call pl_getplane (pl, v1)
	    v1[1] = x;  v1[2] = y
	    call imaplv (PM_REFIM(pl), v1, v2, PM_MAXDIM)

	    call pl_point (pl, v2[1], v2[2], rop)

	} else
	    call pl_point (pl, x, y, rop)
end
