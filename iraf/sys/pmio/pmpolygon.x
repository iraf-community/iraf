# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <pmset.h>
include	<plio.h>

# PM_POLYGON -- Perform a rasterop operation on the area enclosed by a polygon
# drawn in a 2-dimensional plane of a mask.  If the dimensionality of the mask
# exceeds 2, the pm_setplane() procedure should be called first to define the
# plane of the mask to be modified.

procedure pm_polygon (pl, x, y, npts, rop)

pointer	pl			#I mask descriptor
long	x[npts]			#I polygon x-vertices
long	y[npts]			#I polygon y-vertices
int	npts			#I number of points in polygon
int	rop			#I rasterop defining operation

size_t	sz_val
int	i
pointer	sp, xp, yp
errchk	pl_getplane
include	"pmio.com"

begin
	if (PM_MAPXY(pl) == YES) {
	    call smark (sp)
	    sz_val = npts
	    call salloc (xp, sz_val, TY_LONG)
	    call salloc (yp, sz_val, TY_LONG)

	    call pl_getplane (pl, v1)
	    do i = 1, npts {
		v1[1] = x[i];  v1[2] = y[i]
		call imaplv (PM_REFIM(pl), v1, v2, PM_MAXDIM)
		Meml[xp+i-1] = v2[1];  Meml[yp+i-1] = v2[2]
	    }

	    call pl_polygon (pl, Meml[xp], Meml[yp], npts, rop)
	    call sfree (sp)

	} else
	    call pl_polygon (pl, x, y, npts, rop)
end
