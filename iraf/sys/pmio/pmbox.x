# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <pmset.h>
include	<plio.h>

# PM_BOX -- Rasterop between a box as source, and an existing mask as dest.
# This is a 2-dim operator.  The pm_setplane procedure is used to specify
# the plane to be modified.

procedure pm_box (pl, x1,y1, x2,y2, rop)

pointer	pl			#I mask descriptor
int	x1,y1			#I lower left corner of box
int	x2,y2			#I upper right corner of box
int	rop			#I rasterop

errchk	pl_getplane
include	"pmio.com"

begin
	if (PM_MAPXY(pl) == YES) {
	    call pl_getplane (pl, v1)
	    v1[1] = x1;  v1[2] = y1
	    call imaplv (PM_REFIM(pl), v1, v2, PM_MAXDIM)

	    call pl_getplane (pl, v3)
	    v3[1] = x2;  v3[2] = y2
	    call imaplv (PM_REFIM(pl), v3, v4, PM_MAXDIM)

	    call pl_box (pl, v2[1],v2[2], v4[1],v4[2], rop)

	} else
	    call pl_box (pl, x1,y1, x2,y2, rop)
end
