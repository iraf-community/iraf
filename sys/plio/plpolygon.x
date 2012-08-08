# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <plset.h>
include <plio.h>
include "plpolygon.h"


# PL_POLYGON -- Perform a rasterop operation on the area enclosed by a polygon
# drawn in a 2-dimensional plane of a mask.  If the dimensionality of the mask
# exceeds 2, the pl_setplane() procedure should be called first to define the
# plane of the mask to be modified.

procedure pl_polygon (pl, x, y, npts, rop)

pointer	pl			#I mask descriptor
int	x[npts]			#I polygon x-vertices
int	y[npts]			#I polygon y-vertices
int	npts			#I number of points in polygon
int	rop			#I rasterop defining operation

int	line_1, line_2, i
pointer	sp, ufd, xp, yp, oo
extern	pl_upolygon()
errchk	plvalid

begin
	call plvalid (pl)
	if (npts <= 0)
	    return
	else if (npts == 1) {
	    call pl_point (pl, x[1], y[1], rop)
	    return
	}

	call smark (sp)
	call salloc (ufd, LEN_PGONDES, TY_STRUCT)
	call salloc (oo, RL_FIRST + (npts+1)*3, TY_INT)
	call salloc (xp, npts + 1, TY_REAL)
	call salloc (yp, npts + 1, TY_REAL)

	# Initialize the region descriptor.
	P_PL(ufd) = pl
	P_XP(ufd) = xp
	P_YP(ufd) = yp
	P_PV(ufd) = 1
	P_OO(ufd) = oo
	P_OY(ufd) = -1
	P_NS(ufd) = npts - 1
	RLI_LEN(oo) = 0

	# Copy the user supplied polygon vertices into the descriptor,
	# normalizing the polygon in the process.

	do i = 1, npts {
	    Memr[xp+i-1] = x[i]
	    Memr[yp+i-1] = y[i]
	}

	if (npts > 2)
	    if (abs(x[1]-x[npts]) > TOL || abs(y[1]-y[npts]) > TOL) {
		Memr[xp+npts] = x[1]
		Memr[yp+npts] = y[1]
		P_NS(ufd) = npts
	    }

	# Compute the range in Y in which the polygon should be drawn.
	call alimi (y, npts, line_1, line_2)

	call pl_regionrop (pl, pl_upolygon, ufd, line_1, line_2, rop)
	call sfree (sp)
end
