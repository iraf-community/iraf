# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <plset.h>
include <plio.h>
include "plcircle.h"


# PL_CIRCLE -- Rasterop between a circular region as source, and an existing
# mask as destination.  It is not necessary for the center of the circle to
# be inside the mask; if it is outside, the boundary of the circle will be
# clipped to the boundary of the mask.  This is a 2-dim operator.  If the
# image dimensionality is greater than two the pl_setplane procedure should
# be called first to specify the plane to be modified.

procedure pl_circle (pl, x, y, radius, rop)

pointer	pl			#I mask descriptor
int	x,y			#I center coords of circle
int	radius			#I radius of circle
int	rop			#I rasterop

int	y1, y2
pointer	sp, ufd
extern	pl_ucircle()

begin
	call plvalid (pl)
	call smark (sp)
	call salloc (ufd, LEN_CIRCLEDES, TY_STRUCT)

	y1 = max ( 1, min (PL_AXLEN(pl,2), y - radius))
	y2 = max (y1, min (PL_AXLEN(pl,2), y + radius))

	C_PL(ufd) = pl
	C_XCEN(ufd) = x
	C_YCEN(ufd) = y
	C_RADIUS(ufd) = radius
	C_PV(ufd) = 1

	call pl_regionrop (pl, pl_ucircle, ufd, y1, y2, rop)

	call sfree (sp)
end
