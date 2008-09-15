# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <plset.h>
include <plio.h>
include "plbox.h"


# PL_BOX -- Rasterop between a box as source, and an existing mask as dest.
# This is a 2-dim operator.  The pl_setplane procedure is used to specify
# the plane to be modified.

procedure pl_box (pl, x1,y1, x2,y2, rop)

pointer	pl			#I mask descriptor
int	x1,y1			#I lower left corner of box
int	x2,y2			#I upper right corner of box
int	rop			#I rasterop

pointer	sp, ufd
extern	pl_ubox()

begin
	call plvalid (pl)
	call smark (sp)
	call salloc (ufd, LEN_BOXDES, TY_STRUCT)

	B_PL(ufd) = pl
	B_X1(ufd) = max(1, min(PL_AXLEN(pl,1), x1))
	B_Y1(ufd) = max(1, min(PL_AXLEN(pl,2), y1))
	B_X2(ufd) = max(1, min(PL_AXLEN(pl,1), x2))
	B_Y2(ufd) = max(1, min(PL_AXLEN(pl,2), y2))
	B_PV(ufd) = 1

	call pl_regionrop (pl, pl_ubox, ufd, y1, y2, rop)

	call sfree (sp)
end
