# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gio.h>

# GPCELL -- Put a cell array.  Display a two dimensional array of pixels in the
# given window, scaling as necessary to fit the window.  For maximum efficiency
# no clipping is performed.  Only linear coordinate transformations are
# permitted.  The cell array is defined by (x1,y1) and (x2,y2), the NDC coords
# of the corners of the display area.  The graphics kernel is expected to
# map cell array pixels into device pixels by mapping the coordinates of a
# device pixel into the cell array and assigning the value of the nearest
# cell array pixel to the device pixel.  In other words, the cell array is
# sampled or block replicated as necessary to fit the device window.  The kernel
# is not expected to perform area integration or filtering (interpolation)
# to map cell array pixels into device pixels.  In the limiting case M may
# contain a single pixel which will be replicated to fill the specified window,
# e.g., nx=ny=1, (x1,y1)=(0,0), and (x2,y2)=(1,1).
#
#		+--+--+--+--Q (x2,y2)
#	      4	|  |  |  |  |
#		+--+--+--+--+
#	      3	|  |  |  |  |		Sample Cell Array
#	  Y	+--+--+--+--+		   nx = ny = 4
#	      2	|  |  |  |  |
#		+--+--+--+--+
#	      1	|  |  |  |  |
#	(x1,y1)	P--+--+--+--+
#
#	         1  2  3  4	X
#
# A sample 4 by 4 cell array is shown above.  The coordinates of the device
# window into which the cell array is to be mapped refer to the corners P and
# Q of the first and last pixels in the cell array.

procedure gpcell (gp, m, nx, ny, x1, y1, x2, y2)

pointer	gp			# device descriptor
short	m[nx,ny]		# pixels
int	nx, ny			# size of pixel array
real	x1, y1			# lower left corner of output window
real	x2, y2			# upper right corner of output window

real	dy
int	ly1, ly2, i
int	sx1, sx2, sy1, sy2
include	"gpl.com"

begin
	# Flush any buffered polyline output.  Make sure the wcs transformation
	# in the cache is up to date.

	if (op > 1)
	    call gpl_flush()
	else if (gp != gp_out || GP_WCS(gp) != wcs)
	    call gpl_cache (gp)

	# Transform cell window to GKI coordinates.  The coordinate
	# transformation must be linear.

	sx1 = (x1 - wxorigin) * xscale + mxorigin
	sx2 = (x2 - wxorigin) * xscale + mxorigin
	sy1 = (y1 - wyorigin) * yscale + myorigin
	sy2 = (y2 - wyorigin) * yscale + myorigin

	dy = real (sy2 - sy1) / ny	# height of a line in GKI coords

	# Write out the cell array, one line at a time.  Take care that the
	# GKI integer value of ly1 of one line is the same as the ly2 value
	# of the previous line, or there will be a blank line in the output
	# image.

	do i = 1, ny {
	    ly1 = (i-1) * dy + sy1
	    ly2 = (i  ) * dy + sy1
	    call gki_putcellarray (GP_FD(gp), m[1,i], nx,1, sx1,ly1, sx2,ly2)
	}
end
