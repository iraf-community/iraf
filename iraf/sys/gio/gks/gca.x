# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gset.h>
include	"gks.h"

# GCA -- Cell array.  Output a cell array to the specified output device
# area.

procedure gca (px, py, qx, qy, dimx, dimy, ncs, nrs, dx, dy, colia)

real	px, py, qx, qy		# Two points (P, Q) in world coordinates
int	dx, dy			# Number of columns, number of rows 
int	dimx, dimy		# Dimensions of color index array
int	ncs, nrs		# Starting column, row of color array
int	colia[dimx,dimy]	# Colour index array

size_t	sz_val
int	i, j, off
pointer	sp, pixels
include	"gks.com"

begin
	# Extract subraster and convert to type short.
	call smark (sp)
	sz_val = dx * dy
	call salloc (pixels, sz_val, TY_SHORT)
	do j = 1, dy {
	    off = (j - 1) * dx
	    sz_val = dx
	    call achtis (colia[ncs,nrs+j-1], Mems[pixels+off], sz_val)
	}

	# Output color array to all active workstations.
	do i = 1, NDEV 
	    if (gk_status[i] == ACTIVE)
		call gpcell (gp[i], Mems[pixels], dx, dy, px, py, qx, qy)

	call sfree (sp)
end
