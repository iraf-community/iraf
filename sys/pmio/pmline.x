# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <pmset.h>
include	<plio.h>

# PM_LINE -- Perform a rasterop operation upon a line of arbitrary width drawn
# at an arbitrary orientation in a 2-dimensional plane of a mask.  If the
# dimensionality of the mask exceeds 2, the pm_setplane() procedure should be
# called first to define the plane of the mask to be modified.

procedure pm_line (pl, x1, y1, x2, y2, width, rop)

pointer	pl			#I mask descriptor
int	x1,y1			#I start point of line
int	x2,y2			#I end point of line
int	width			#I width of line to be drawn, pixels
int	rop			#I rasterop defining operation

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

	    call pl_line (pl, v2[1],v2[2], v4[1],v4[2], width, rop)

	} else
	    call pl_line (pl, x1, y1, x2, y2, width, rop)
end
