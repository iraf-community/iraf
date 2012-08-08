# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gki.h>
include	"stdgraph.h"

define	ZSTEP		4	# bit to be tested (step function width)


# STG_PUTCELLARRAY -- Draw a cell array, i.e., two dimensional array of pixels
# (greylevels or colors).  The algorithm used here maps 8 bits in into 1 bit
# out, using a step function lookup table.  The result is a band-contoured
# image, where the spacing and width of the contour bands decreases as the
# rate of change of intensity in the input cell array increases.

procedure stg_putcellarray (m, nx, ny, ax1,ay1, ax2,ay2)

short	m[nx,ny]		# cell array
int	nx, ny			# number of pixels in X and Y
int	ax1, ay1		# lower left corner of output window
int	ax2, ay2		# upper right corner of output window

real	dx, dy
int	my, i1, i2, v, i, j, k
include	"stdgraph.com"
int	and()

begin
	# Set polyline width to 1 for max y-res.
	call stg_ctrl1 ("LW", 1)
	SG_PLWIDTH(g_sg) = 1

	# Determine the width of a cell array pixel in GKI units.
	dx = real (ax2 - ax1) / nx

	# Determine the height of a device pixel in GKI units.
	dy = max (1.0, real(GKI_MAXNDC) / real(g_yres))

	# Process the cell array.  The outer loop runs over device pixels in Y;
	# each iteration writes one line of the output raster.  The inner loop
	# runs down a line of the cell array.

	k = 0
	for (my = ay1 + dy/2;  my < ay2;  my = k * dy + ay1) {
	    j = max(1, min(ny, int (real(my-ay1) / real(ay2-ay1) * (ny-1)) + 1))
	    my = min (my, int (ay2 - dy/2))

	    for (i=1;  i <= nx;  ) {
		do i = i, nx {
		    v = m[i,j]
		    if (and (v, ZSTEP) != 0)
			break
		}

		if (i <= nx) {
		    i1 = i
		    i2 = nx
		    do i = i1 + 1, nx {
			v = m[i,j]
			if (and (v, ZSTEP) == 0) {
			    i2 = i
			    break
			}
		    }

		    # The following decreases the length of dark line segments
		    # to make features more visible.

		    if (i2 - i1 >= 2)
			if (i1 > 1 && i2 < nx) {
			    i1 = i1 + 1
			    i2 = i2 - 1
			}

		    # Draw the line segment.
		    call stg_move (int ((i1-1) * dx + ax1), my)
		    call stg_draw (int (i2 * dx + ax1), my)

		    if (i2 >= nx)
			i = nx + 1
		}
	    }

	    k = k + 1
	}
end
