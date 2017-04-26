# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <pmset.h>
include	<plio.h>

# PM_CIRCLE -- Rasterop between a circular region as source, and an existing
# mask as destination.  It is not necessary for the center of the circle to
# be inside the mask; if it is outside, the boundary of the circle will be
# clipped to the boundary of the mask.  This is a 2-dim operator.  If the
# image dimensionality is greater than two the pm_setplane procedure should
# be called first to specify the plane to be modified.

procedure pm_circle (pl, x, y, radius, rop)

pointer	pl			#I mask descriptor
int	x,y			#I center coords of circle
int	radius			#I radius of circle
int	rop			#I rasterop

errchk	pl_getplane
include	"pmio.com"

begin
	if (PM_MAPXY(pl) == YES) {
	    call pl_getplane (pl, v1)
	    v1[1] = x;  v1[2] = y
	    call imaplv (PM_REFIM(pl), v1, v2, PM_MAXDIM)

	    call pl_getplane (pl, v3)
	    v3[1] = x + radius;  v3[2] = y
	    call imaplv (PM_REFIM(pl), v3, v4, PM_MAXDIM)

	    call pl_circle (pl, v2[1], v2[2], abs(v4[1]-v2[1]), rop)

	} else
	    call pl_circle (pl, x, y, radius, rop)
end
