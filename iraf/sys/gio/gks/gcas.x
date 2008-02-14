# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gset.h>
include	"gks.h"

# GCAS -- Cell array.  Output a cell array to the specified output device
# area.  This version of GCA intended for input color array of type short.

procedure gcas (px, py, qx, qy, dimx, dimy, ncs, nrs, dx, dy, colia)

real	px, py, qx, qy		# Two points (P, Q) in world coordinates
int	dx, dy			# Number of columns, number of rows 
int	dimx, dimy		# Dimensions of color index array
int	ncs, nrs		# Starting column, row of color array
short	colia[dimx, dimy]	# Colour index array

size_t	sz_val
int	i, j, off
pointer	sp, pixels
include	"gks.com"

begin
	if (ncs == 1 && nrs == 1) {
	    # Output color array to all active workstations.
	    do i = 1, NDEV 
	        if (gk_status[i] == ACTIVE)
		    call gpcell (gp[i], Mems[pixels], dx, dy, px, py, qx, qy)
	
	} else {
	    # Cell array is subraster of a larger array
	    call smark (sp)
	    sz_val = dx * dy
	    call salloc (pixels, sz_val, TY_SHORT)

	    # Extract subraster
	    do j = 1, dy {
		off = (j - 1) * dx
		call amovs (colia[ncs,nrs+j-1], Mems[off], dx)
	    }

	    # Output color array to all active workstations.
	    do i = 1, NDEV 
	        if (gk_status[i] == ACTIVE)
		    call gpcell (gp[i], Mems[pixels], dx, dy, px, py, qx, qy)

	    call sfree (sp)
	}
end
