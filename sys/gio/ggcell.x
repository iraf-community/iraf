# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gio.h>

# GGCELL -- Get a cell array, i.e., a two dimensional array of pixels.  If the
# resolution of the graphics device does not match that of the cell array the
# kernel is expected to compute the coordinates of each cell array pixel in 
# device coordinates and return the value of the nearest device pixel as the
# cell array value.  This equates to either subsampling or block replication
# depending on the relative scale of the two devices.  See put cell array for
# additional information.

procedure ggcell (gp, m, nx, ny, x1, y1, x2, y2)

pointer	gp			# device descriptor
int	nx, ny			# size of pixel array
short	m[nx,ny]		# pixels
real	x1, y1			# lower left corner of input window
real	x2, y2			# upper right corner of input window

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

	# Read the cell array into M, one line at a time.  Take care that the
	# GKI integer value of ly1 of one line is the same as the ly2 value
	# of the previous line, or there will be a blank line in the output
	# image.

	do i = 1, ny {
	    ly1 = (i-1) * dy + sy1
	    ly2 = (i  ) * dy + sy1
	    call gki_getcellarray (GP_FD(gp), m[1,i], nx,1, sx1,ly1, sx2,ly2)
	}
end
