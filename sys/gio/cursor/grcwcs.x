# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<gio.h>
include	<gki.h>
include	"gtr.h"
include	"grc.h"

# GRC_SCRTOWCS -- Transform screen coordinates (raw cursor coordinates) to
# world coordinates.  This is not terribly efficient, but it does not matter
# for cursor mode applications which do not involve many coordinate
# transformations.

procedure grc_scrtowcs (stream, sx, sy, raster, rx, ry, wx, wy, wcs)

int	stream			#I graphics stream
real	sx, sy			#I screen coordinates
int	raster			#I raster number
real	rx, ry			#I raster coordinates
real	wx, wy			#O world coordinates
int	wcs			#O world coordinate system

pointer	w, tr
real	mx, my
real	ct[LEN_CT]
int	grc_selectwcs()
pointer	gtr_init()
errchk	gtr_init

begin
	tr = gtr_init (stream)

	# Convert screen (raster 0) to NDC coordinates, undoing the effects
	# of the workstation transformation.  This is not done for raster
	# coordinates since these are already raster-normalized coordinates
	# as returned by the server.

	if (raster == 0)
	    call grc_scrtondc (rx, ry, mx, my)
	else {
	    mx = rx
	    my = ry
	}

	# Select a WCS.  The TR_WCS variable is set only if the user
	# explicitly fixes the WCS to override automatic selection.  The
	# best WCS for the raster is used if there is one, otherwise the
	# best screen WCS is used.

	if (TR_WCS(tr) == NULL) {
	    wcs = grc_selectwcs (tr, raster, mx, my)
	    if (wcs == 0) {
		call grc_scrtondc (sx, sy, mx, my)
		wcs = grc_selectwcs (tr, 0, mx, my)
	    }
	} else
	    wcs = TR_WCS(tr)

	# Set up the coordinate transformation.
	w = TR_WCSPTR(tr,wcs)
	call grc_settran (w, ct)

	# Transform NDC coordinates to WCS coordinates.
	call grc_ndctowcs (ct, mx, my, wx, wy)
end


# GRC_SETTRAN -- Set up the coordinate transformation parameters for a given
# world coordinate system.

procedure grc_settran (w, ct)

pointer	w				# window descriptor
real	ct[LEN_CT]			# transformation descriptor

real	worigin, scale
real	m1, m2, w1, w2
int	transformation, ax
bool	fp_equalr()
real	elogr()

begin
	# Compute world -> NDC coordinate transformation.

	do ax = 1, 2 {
	    if (ax == 1) {
		transformation = WCS_XTRAN(w)
		w1 = WCS_WX1(w)
		w2 = WCS_WX2(w)
		m1 = WCS_SX1(w)
		m2 = WCS_SX2(w)
	    } else {
		transformation = WCS_YTRAN(w)
		w1 = WCS_WY1(w)
		w2 = WCS_WY2(w)
		m1 = WCS_SY1(w)
		m2 = WCS_SY2(w)
	    }

	    if (transformation == LINEAR) {
		worigin = w1
		if (fp_equalr (w1, w2))
		    scale = 1.0
		else
		    scale = (m2 - m1) / (w2 - w1)
	    } else if (transformation == LOG && w1 > 0 && w2 > 0) {
		worigin = log10 (w1)
		if (fp_equalr (log10(w2), worigin))
		    scale = 1.0
		else
		    scale = (m2 - m1) / (log10(w2) - worigin)
	    } else {
		worigin = elogr (w1)
		if (fp_equalr (elogr(w2), worigin))
		    scale = 1.0
		else
		    scale = (m2 - m1) / (elogr(w2) - worigin)
	    }

	    ct[ax,CT_TRAN]    = transformation
	    ct[ax,CT_SCALE]   = scale
	    ct[ax,CT_WORIGIN] = worigin
	    ct[ax,CT_MORIGIN] = m1
	}
end


# GRC_WCSTONDC -- Transform world coordinates to NDC coordinates using the
# computed transformation parameters.

procedure grc_wcstondc (ct, wx, wy, mx, my)

real	ct[LEN_CT]		# coordinate transformation descriptor
real	wx, wy			# world coordinates of point
real	mx, my			# ndc coordinates of point

real	v
int	transformation, ax
real	elogr()

begin
	do ax = 1, 2 {
	    transformation = nint (ct[ax,CT_TRAN])
	    if (ax == 1)
		v = wx
	    else
		v = wy

	    if (transformation == LINEAR)
		;
	    else if (transformation == LOG)
		v = log10 (v)
	    else
		v = elogr (v)

	    v = ((v - ct[ax,CT_WORIGIN]) * ct[ax,CT_SCALE]) + ct[ax,CT_MORIGIN]
	    if (ax == 1)
		mx = v
	    else
		my = v
	}
end


# GRC_NDCTOWCS -- Transform NDC coordinates to world coordinates using the
# computed transformation parameters.

procedure grc_ndctowcs (ct, mx, my, wx, wy)

real	ct[LEN_CT]		# coordinate transformation descriptor
real	mx, my			# ndc coordinates of point
real	wx, wy			# world coordinates of point

real	v
int	transformation, ax
real	aelogr()

begin
	do ax = 1, 2 {
	    transformation = nint (ct[ax,CT_TRAN])
	    if (ax == 1)
		v = mx
	    else
		v = my

	    v = ((v - ct[ax,CT_MORIGIN]) / ct[ax,CT_SCALE]) + ct[ax,CT_WORIGIN]
	    if (transformation == LINEAR)
		;
	    else if (transformation == LOG)
		v = 10.0 ** v
	    else
		v = aelogr (v)

	    if (ax == 1)
		wx = v
	    else
		wy = v
	}
end


# GRC_SELECTWCS -- Select the WCS nearest to the given position in NDC
# coordinates.  If the point falls within a single WCS then that WCS is
# selected.  If the point falls within multiple WCS then the closest WCS
# is selected.  If multiple (non unitary) WCS are defined at the same
# distance, e.g., when the WCS share the same viewport, then the highest
# numbered WCS is selected.

int procedure grc_selectwcs (tr, raster, mx, my)

pointer	tr			#I GTR descriptor
int	raster			#I raster number
real	mx, my			#I NDC coordinates of point

pointer	w
int	wcs, closest_wcs, flags
real	tol, sx1, sx2, sy1, sy2
real	distance, old_distance, xcen, ycen
int	nin, in[MAX_WCS]

begin
	nin = 0
	closest_wcs = 0
	old_distance = 1.0
	tol = EPSILON * 10.0

	# Inspect each WCS.  All WCS are passed even though only one or two
	# WCS will be set to nonunitary values for a given plot.  Omitting
	# the unitary WCS, determine the closest WCS and make a list of the
	# WCS containing the given point.

	do wcs = 1, MAX_WCS {
	    w = TR_WCSPTR(tr,wcs)

	    # Cache WCS params in local storage.
	    sx1 = WCS_SX1(w)
	    sx2 = WCS_SX2(w)
	    sy1 = WCS_SY1(w)
	    sy2 = WCS_SY2(w)
	    flags = WCS_FLAGS(w)
	    xcen = (sx1 + sx2) / 2.0
	    ycen = (sy1 + sy2) / 2.0

	    # Skip to next WCS if the raster number doesn't match.
	    if (WF_RASTER(flags) != raster)
		next

	    # Skip to next WCS if this one is not defined.
	    if (and (flags, WF_NEWFORMAT) == 0) {
		# Preserve old semantics if passed old format WCS.
		if (sx1 == 0 && sx2 == 0 || sy1 == 0 && sy2 == 0)
		    next
		if (abs ((sx2-sx1) - 1.0) < tol && abs ((sy2-sy1) - 1.0) < tol)
		    next
	    } else if (and (flags, WF_DEFINED) == 0)
		next

	    # Determine closest WCS to point (mx,my).
	    distance = ((mx - xcen) ** 2) + ((my - ycen) ** 2)
	    if (distance <= old_distance) {
		closest_wcs = wcs
		old_distance = distance
	    }

	    # Check if point is inside this WCS.
	    if (mx >= sx1 && mx <= sx2 && my >= sy1 && my <= sy2) {
		nin = nin + 1
		in[nin] = wcs
	    }
	}

	# If point is inside exactly one non-unitary WCS then select that WCS.
	if (nin == 1)
	    return (in[1])

	# If point is inside more than one WCS, or if point is not inside any
	# WCS, select the closest WCS.  If multiple WCS are at the same
	# distance we have already selected the higher numbered WCS due to
	# the way the distance test is conducted, above.

	return (closest_wcs)
end
