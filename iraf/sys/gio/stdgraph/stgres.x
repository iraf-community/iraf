# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gki.h>
include	"stdgraph.h"

# STG_RESOLUTION -- Set the "soft" device resolution.  When plotting GKI
# coordinates are transformed into a space with the indicated resolution and
# unresolved points are discarded, before transforming to device coordinates.
# We must set up both the transformation to resolution space and the
# transformation to device space.

procedure stg_resolution (xres, yres)

int	xres				# device X resolution
int	yres				# device Y resolution
int	nx, ny
int	ttygeti()
include	"stdgraph.com"

begin
	if (g_tty == NULL) {
	    g_xres = xres
	    g_yres = yres
	    return
	}

	# Set the resolution value in the stdgraph common only if a nonzero
	# value is given.  A value of zero does not change the resolution.

	if (xres > 0)
	    g_xres = xres
	if (yres > 0)
	    g_yres = yres


	# If we still have a zero resolution then we use the full resolution
	# of the device.  The 3/4 reduction in resolution is needed to clip
	# points that would be unresolved due to integer truncation effects.

	if (g_xres <= 0) {
	    g_xres = ttygeti (g_tty, "xr")
	    if (g_xres <= 0)
		g_xres = 1024
	    g_xres = max (2, g_xres * 3 / 4)
	}
	if (g_yres <= 0) {
	    g_yres = ttygeti (g_tty, "yr")
	    if (g_yres <= 0)
		g_yres = 1024
	    g_yres = max (2, g_yres * 3 / 4)
	}

	# Set up coordinate transformations.  The first transformation is from
	# GKI coordinates to device resolution coordinates (0:xres-1,0:yres-1)
	# and is defined by xres, yres, and GKI_MAXNDC.  Clipping of unresolved
	# points is performed after this first transformation.  The second
	# transformation maps resolved points into the device window.

	# GKI -> resolution coords.
	g_dxres = max (1, (GKI_MAXNDC + 1) / g_xres)
	g_dyres = max (1, (GKI_MAXNDC + 1) / g_yres)

	g_x1 = ttygeti (g_tty, "X1")
	g_y1 = ttygeti (g_tty, "Y1")
	g_x2 = ttygeti (g_tty, "X2")
	g_y2 = ttygeti (g_tty, "Y2")
	nx   = g_x2 - g_x1 + 1
	ny   = g_y2 - g_y1 + 1

	if (nx <= 1 || ny <= 1) {
	    call eprintf ("openws: illegal graphics device window\n")
	    nx = g_xres
	    ny = g_yres
	}

	# GKI -> window coords.
	g_dx = real (nx - 1) / GKI_MAXNDC
	g_dy = real (ny - 1) / GKI_MAXNDC

	# The last point in resolution coords is used to clip unresolved
	# points when drawing polylines.

	g_lastx = -1
	g_lasty = -1
end
