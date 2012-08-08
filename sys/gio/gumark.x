# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<gio.h>

# GUMARK -- Draw a user defined mark.  The mark is defined by the polygon
# (x[i],y[i], i=1,npts), normalized to the unit square.  This mark is mapped
# into the window at (XCEN,YCEN) of size XSIZE, YSIZE, where the mark center
# is always given in world coordinates but the size may be given in any of a
# number of ways, independently in X and Y.

procedure gumark (gp, x, y, npts, xcen, ycen, xsize, ysize, fill)

pointer	gp			# graphics descriptor
real	x[ARB]			# X coordinates of marker polygon (unit square)
real	y[ARB]			# Y coordinates of marker polygon (unit square)
int	npts			# number of points in marker polygon
real	xcen, ycen		# world coordinates of center of marker
real	xsize, ysize		# marker size in X and Y
int	fill			# draw marker using area fill

pointer	plap, pmap
bool	scale_unset
int	save_linetype, index, i
real	x1, y1, xs, ys, dx, dy
real	size[2], ndc_size[2], wcs_size[2]

begin
	plap = GP_PLAP(gp)
	pmap = GP_PMAP(gp)

	# Determine the marker size in world coordinates.  Marksizes 1:4 are
	# "standard size" markers.  A marksize of [0-1) is an explicit marker
	# size in NDC coordinates, while a negative marksize is an explicit
	# marker size in world coordinates.

	size[1] = xsize
	size[2] = ysize
	scale_unset = true

	do i = 1, 2
	    if (size[i] > 0) {
		if (size[i] - 1.0 > -EPSILON) {
		    # Use a default marker size.
		    index = min (MAX_SZMARKER, int(size[i]))
		    ndc_size[i] = GP_SZMARKER (gp, index)

		    # Correct for the aspect ratio.
		    if (i == 1)
			ndc_size[1] = ndc_size[1] * GP_DEVASPECT(gp)
		} else
		    ndc_size[i] = size[i]

		# Convert to size in world coords.
		if (scale_unset) {
		    # Get the scale in wcs units per ndc unit at (x,y).
		    call ggscale (gp, xcen, ycen, dx, dy)
		    scale_unset = false
		}
		if (i == 1)
		    wcs_size[1] = ndc_size[1] * abs(dx)
		else
		    wcs_size[2] = ndc_size[2] * abs(dy)

	    } else
		wcs_size[i] = -size[i]

	# Set fill area instruction type if filling, otherwise set linetype
	# if marker will be drawn as a polyline.  Do nothing if polymarker
	# linetype is same as polyline linetype.

	if (fill == YES)
	    call gpl_settype (gp, FILLAREA)
	else {
	    save_linetype = PL_LTYPE(plap)
	    if (save_linetype != PM_LTYPE(pmap)) {
		call gpl_flush()
		PL_LTYPE(plap) = PM_LTYPE(pmap)
		PL_STATE(plap) = MODIFIED
	    }
	}

	# Draw the marker, scaling as necessary to fit the mark window.  Final
	# mark need not have the same aspect ratio as the normalized mark.
	# Leave the pen positioned to the center of the marker.

	xs = wcs_size[1]
	ys = wcs_size[2]
	x1 = xcen - (xs / 2.0)
	y1 = ycen - (ys / 2.0)

	call gamove (gp, x[1] * xs + x1, y[1] * ys + y1)
	do i = 2, npts
	    call gadraw (gp, x[i] * xs + x1, y[i] * ys + y1)
	call gamove (gp, xcen, ycen)

	# If the polyline linetype was modified restore the original value.
	# Do not need to do anything if polymarker linetype was same as
	# polyline linetype.

	if (fill == YES)
	    call gpl_settype (gp, POLYLINE)
	else if (save_linetype != PM_LTYPE(pmap)) {
	    call gpl_flush()
	    PL_LTYPE(plap) = save_linetype
	    PL_STATE(plap) = MODIFIED
	}
end
