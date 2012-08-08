# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gki.h>
include	"sgi.h"

define	DEF_YRES	2048	# default height of device pixel in GKI units
define	ZSTEP		4	# bit to be tested (step function width)


# SGI_PUTCELLARRAY -- Draw a cell array, i.e., two dimensional array of pixels
# (greylevels or colors).  The algorithm used here maps 8 bits in into 1 bit
# out, using a step function lookup table.  The result is a band-contoured
# image, where the spacing and width of the contour bands decreases as the
# rate of change of intensity in the input cell array increases.

procedure sgi_putcellarray (m, nx, ny, ax1,ay1, ax2,ay2)

short	m[nx,ny]		# cell array
int	nx, ny			# number of pixels in X and Y
int	ax1, ay1		# lower left corner of output window
int	ax2, ay2		# upper right corner of output window

bool	ttygetb()
include	"sgi.com"

begin
	if (ttygetb (g_tty, "BI"))
	    call sgi_bcell (m, nx, ny, ax1,ay1, ax2,ay2)
	else
	    call sgi_mcell (m, nx, ny, ax1,ay1, ax2,ay2)
end


# SGI_BCELL -- Put cell array, optimized for a bitmap device.  In this case,
# to get the maximum resolution at maximum efficiency it is desirable for the
# main loop to be over device pixels, mapping the device pixel into the
# nearest line of the input cell array.

procedure sgi_bcell (m, nx, ny, ax1,ay1, ax2,ay2)

short	m[nx,ny]		# cell array
int	nx, ny			# number of pixels in X and Y
int	ax1, ay1		# lower left corner of output window
int	ax2, ay2		# upper right corner of output window

real	dx, dy
int	my, i1, i2, v, i, j, k
include	"sgi.com"
int	and()

begin
	# Count drawing instruction, set polyline width to 1 for max y-res.
	g_ndraw = g_ndraw + 1
	call sgk_linewidth (g_out, 1)
	SGI_WIDTH(g_kt) = 0

	# Determine the width of a cell array pixel in GKI units.
	dx = real (ax2 - ax1) / nx

	# Determine the height of a device pixel in GKI units.
	if (SGI_YRES(g_kt) <= 0)
	    dy = GKI_MAXNDC / DEF_YRES
	else
	    dy = max (1.0, real(GKI_MAXNDC) / real(SGI_YRES(g_kt)))

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
		    call sgk_move (g_out, int ((i1-1) * dx + ax1), my)
		    call sgk_draw (g_out, int (i2 * dx + ax1), my)

		    if (i2 >= nx)
			i = nx + 1
		}
	    }

	    k = k + 1
	}
end


# SGI_MCELL -- Put cell array, optimized for a metafile device.  In this case,
# it is prohibitively expensive to draw into each resolvable line of the
# output device.  It is better to set the linewidth to the width of a cell
# array pixel, output the minimum number of drawing instructions, and let the
# metafile device widen the lines.

procedure sgi_mcell (m, nx, ny, ax1,ay1, ax2,ay2)

short	m[nx,ny]		# cell array
int	nx, ny			# number of pixels in X and Y
int	ax1, ay1		# lower left corner of output window
int	ax2, ay2		# upper right corner of output window

real	dx, dy
int	yres, my, i1, i2, v, i, j
include	"sgi.com"
int	and()

begin
	# Count drawing instruction, clobber saved polyline width.
	g_ndraw = g_ndraw + 1
	SGI_WIDTH(g_kt) = 0

	# Determine the width and height of a cell array pixel in GKI units.
	dx = real (ax2 - ax1) / nx
	dy = real (ay2 - ay1) / ny

	# Set the SGK line width to the height of a pixel in the cell array.
	yres = SGI_YRES(g_kt)
	if (yres <= 0)
	    yres = DEF_YRES
	call sgk_linewidth (g_out,
	    max (1, nint (dy / (real(GKI_MAXNDC) / real(yres)))))

	# Process the cell array.  The outer loop runs over lines of the input
	# cell array; each iteration writes only one line of the output raster,
	# but the width of the line is adjusted to the height of a pixel in
	# the cell array (the resolution of the cell array should not exceed
	# that of the device).

	for (j=1;  j <= ny;  j=j+1) {
	    my = int ((j - 0.5) * dy) + ay1

	    for (i=1;  i <= nx;  ) {
		do i = i, nx {
		    v = m[i,j]
		    if (and (v, ZSTEP) != 0)
			break
		}

		if (i <= nx) {
		    i1 = i
		    i2 = nx
		    do i = i + 1, nx {
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
		    call sgk_move (g_out, int ((i1-1) * dx + ax1), my)
		    call sgk_draw (g_out, int (i2 * dx + ax1), my)

		    if (i2 >= nx)
			i = nx + 1
		}
	    }
	}
end
