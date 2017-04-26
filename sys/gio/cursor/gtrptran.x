# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<gio.h>
include	<gki.h>
include	"gtr.h"

# GTR_PTRAN -- Set the workstation transformation.  The workstation
# transformation is automatically zeroed whenever the screen is cleared
# or when a workstation is opened.

procedure gtr_ptran (stream, x1, x2, y1, y2)

int	stream			# graphics stream to be set
real	x1, x2			# range of workstation viewport in X
real	y1, y2			# range of workstation viewport in Y

pointer	tr
real	tol, min_width, dx, dy
real	cx1, cx2, cy1, cy2
include	"gtr.com"

begin
	tr = trdes[stream]
	tol = 5.0 * EPSILON

	if (abs(x1) < tol && abs (x2 - 1.0) < tol &&
	    abs(y1) < tol && abs (y2 - 1.0) < tol) {

	    wstranset = NO

	} else {
	    # Save viewport.
	    vx1 = x1
	    vx2 = x2
	    vy1 = y1
	    vy2 = y2

	    # Clip viewport at NDC boundary.
	    cx1 = max (0., min (1., x1))
	    cx2 = max (0., min (1., x2))
	    cy1 = max (0., min (1., y1))
	    cy2 = max (0., min (1., y2))

	    # Make sure the viewport does not have a zero extent in either
	    # axis after clipping.
	    min_width = 1E-4
	    if (cx2 - cx1 < min_width)
		cx2 = cx1 + min_width
	    if (cy2 - cy1 < min_width)
		cy2 = cy1 + min_width

	    # Set clipping viewport in input GKI space.
	    mx1 = nint (cx1 * GKI_MAXNDC)
	    mx2 = nint (cx2 * GKI_MAXNDC)
	    my1 = nint (cy1 * GKI_MAXNDC)
	    my2 = nint (cy2 * GKI_MAXNDC)

	    # Set transformation upon the clipped GKI coordinates.
	    dx = max (min_width, (x2 - x1))
	    dy = max (min_width, (y2 - y1))
	    xorigin = (cx1 - x1) / dx * GKI_MAXNDC
	    yorigin = (cy1 - y1) / dy * GKI_MAXNDC
	    xscale = 1. / dx
	    yscale = 1. / dy

	    wstranset = YES
	}

	# Clear the scratch buffer whenever the workstation viewport is
	# changed.

	TR_OPSB(tr) = TR_SCRATCHBUF(tr)
end
