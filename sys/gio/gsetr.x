# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<mach.h>
include	<gset.h>
include	<gio.h>

# GSETR -- Set any GIO parameter of type integer or real.  Real values are
# silently coerced to integer if the actual parameter value is integer.

procedure gsetr (gp, param, rval)

pointer	gp			# graphics descriptor
int	param			# parameter to be set
real	rval			# new value for parameter

real	char_height
int	wcs, axes, field, ax[2], wflags, i
pointer	w, p, pl, pm, tx, fa
real	ggetr()

begin
	# Compute pointers to substructures once, here, to save space later.
	wcs = GP_WCS(gp)
	w = GP_WCSPTR(gp,wcs)
	wflags = WCS_FLAGS(w)

	pl = GP_PLAP(gp)
	pm = GP_PMAP(gp)
	tx = GP_TXAP(gp)
	fa = GP_FAAP(gp)

	switch (param) {

	# General GIO parameters.

	case G_FD:
	    GP_FD(gp) = nint(rval)
	case G_TTY:
	    GP_TTY(gp) = nint(rval)
	case G_WCS:
	    GP_WCS(gp) = nint(rval)
	case G_CURSOR:
	    GP_CURSOR(gp) = nint(rval)

	# These parameters affect the current WCS.

	case G_XTRAN:
	    WCS_XTRAN(w) = nint(rval)
	    GP_WCSSTATE(gp) = MODIFIED
	    call gpl_reset()
	case G_YTRAN:
	    WCS_YTRAN(w) = nint(rval)
	    GP_WCSSTATE(gp) = MODIFIED
	    call gpl_reset()
	case G_CLIP:
	    if (nint(rval) == 0)
		WCS_FLAGS(w) = and (wflags, not(WF_CLIP))
	    else
		WCS_FLAGS(w) = or (wflags, WF_CLIP)
	    GP_WCSSTATE(gp) = MODIFIED
	    call gpl_reset()
	case G_RASTER:
	    WCS_FLAGS(w) = WF_SETRASTER (wflags, nint(rval))
	    GP_WCSSTATE(gp) = MODIFIED
	    call gpl_reset()

	# Default marker sizes (NDC coords).

	case G_SZMARKER1:
	    GP_SZMARKER(gp,1) = rval
	case G_SZMARKER2:
	    GP_SZMARKER(gp,2) = rval
	case G_SZMARKER3:
	    GP_SZMARKER(gp,3) = rval
	case G_SZMARKER4:
	    GP_SZMARKER(gp,4) = rval

	# Polyline attributes.

	case G_PLTYPE:
	    call gst_set_attribute_i (nint(rval), PL_LTYPE(pl), PL_STATE(pl))
	case G_PLWIDTH:
	    call gst_set_attribute_r (rval, PL_WIDTH(pl), PL_STATE(pl))
	case G_PLCOLOR:
	    call gst_set_attribute_i (nint(rval), PL_COLOR(pl), PL_STATE(pl))

	# Polymarker attributes.

	case G_PMLTYPE:
	    call gst_set_attribute_i (nint(rval), PM_LTYPE(pm), PM_STATE(pm))
	case G_PMWIDTH:
	    call gst_set_attribute_r (rval, PM_WIDTH(pm), PM_STATE(pm))
	case G_PMCOLOR:
	    call gst_set_attribute_i (nint(rval), PM_COLOR(pm), PM_STATE(pm))

	# Text drawing attributes.

	case G_TXUP:
	    call gst_set_attribute_i (nint(rval), TX_UP(tx), TX_STATE(tx))
	case G_TXSIZE:
	    call gst_set_attribute_r (rval, TX_SIZE(tx), TX_STATE(tx))
	case G_TXPATH:
	    call gst_set_attribute_i (nint(rval), TX_PATH(tx), TX_STATE(tx))
	case G_TXSPACING:
	    call gst_set_attribute_r (rval, TX_SPACING(tx), TX_STATE(tx))
	case G_TXHJUSTIFY:
	    call gst_set_attribute_i (nint(rval), TX_HJUSTIFY(tx), TX_STATE(tx))
	case G_TXVJUSTIFY:
	    call gst_set_attribute_i (nint(rval), TX_VJUSTIFY(tx), TX_STATE(tx))
	case G_TXFONT:
	    call gst_set_attribute_i (nint(rval), TX_FONT(tx), TX_STATE(tx))
	case G_TXQUALITY:
	    call gst_set_attribute_i (nint(rval), TX_QUALITY(tx), TX_STATE(tx))
	case G_TXCOLOR:
	    call gst_set_attribute_i (nint(rval), TX_COLOR(tx), TX_STATE(tx))

	# Fill area attributes.

	case G_FASTYLE:
	    call gst_set_attribute_i (nint(rval), FA_STYLE(fa), FA_STATE(fa))
	case G_FACOLOR:
	    call gst_set_attribute_i (nint(rval), FA_COLOR(fa), FA_STATE(fa))

	# Axis labelling parameters affecting more than one axis.

	case G_DRAWTITLE:
	    GP_DRAWTITLE(gp) = nint(rval)
	case G_TITLESIZE:
	    GP_TITLESIZE(gp) = rval
	case G_TITLECOLOR:
	    GP_TITLECOLOR(gp) = nint(rval)
	case G_TITLEJUST:
	    GP_TITLEJUST(gp) = nint(rval)
	case G_NTITLELINES:
	    GP_NTITLELINES(gp) = nint(rval)
	case G_FRAMECOLOR:
	    GP_FRAMECOLOR(gp) = nint(rval)
	case G_ASPECT:
	    GP_ASPECT(gp) = rval

	case G_CHARSIZE:
	    # Set the character size (height) in NDC units.  This can also be
	    # done by querying for "ch" and setting the relative size, but the
	    # function is fundamental enough to be worth implementing as a
	    # single call.

	    char_height = ggetr (gp, "ch")
	    if (char_height < EPSILON)
		char_height = DEF_CHARHEIGHT
	    call gst_set_attribute_r (rval / char_height, TX_SIZE(tx),
		TX_STATE(tx))

	default:
	    # The GLABAX parameters for the X and Y axes may be set separately
	    # for each axis or simultaneously for both.  The parameter codes
	    # are encoded as 100 (X only) 200 (Y only) or 300 (both) plus the
	    # code for the field in the lower digits.

	    if (param < FIRST_GLABAX_PARAM || param > LAST_GLABAX_PARAM)
		call syserr (SYS_GSET)

	    axes = param / 100
	    field = mod (param, 100) + 300

	    ax[1] = 0
	    ax[2] = 0
	    if (axes == 1 || axes == 3)
		ax[1] = YES
	    if (axes == 2 || axes == 3)
		ax[2] = YES

	    do i = 1, 2 {
		if (ax[i] == YES) {
		    if (i == 1)
			p = GP_XAP(gp)
		    else
			p = GP_YAP(gp)

		    switch (field) {
		    case G_DRAWAXES:
			GL_DRAWAXES(p) = nint(rval)
		    case G_SETAXISPOS:
			GL_SETAXISPOS(p) = nint(rval)
		    case G_AXISPOS1:
			GL_AXISPOS1(p) = rval
		    case G_AXISPOS2:
			GL_AXISPOS2(p) = rval
		    case G_DRAWGRID:
			GL_DRAWGRID(p) = nint(rval)
		    case G_GRIDCOLOR:
			GL_GRIDCOLOR(p) = nint(rval)
		    case G_ROUND:
			GL_ROUND(p) = nint(rval)
		    case G_LABELAXIS:
			GL_LABELAXIS(p) = nint(rval)
		    case G_AXISLABELSIZE:
			GL_AXISLABELSIZE(p) = rval
		    case G_AXISLABELCOLOR:
			GL_AXISLABELCOLOR(p) = nint(rval)
		    case G_DRAWTICKS:
			GL_DRAWTICKS(p) = nint(rval)
		    case G_LABELTICKS:
			GL_LABELTICKS(p) = nint(rval)
		    case G_NMAJOR:
			GL_NMAJOR(p) = nint(rval)
		    case G_NMINOR:
			GL_NMINOR(p) = nint(rval)
		    case G_MAJORLENGTH:
			GL_MAJORLENGTH(p) = rval
		    case G_MINORLENGTH:
			GL_MINORLENGTH(p) = rval
		    case G_MAJORWIDTH:
			GL_MAJORWIDTH(p) = rval
		    case G_MINORWIDTH:
			GL_MINORWIDTH(p) = rval
		    case G_AXISWIDTH:
			GL_AXISWIDTH(p) = rval
		    case G_AXISCOLOR:
			GL_AXISCOLOR(p) = nint(rval)
		    case G_TICKLABELSIZE:
			GL_TICKLABELSIZE(p) = rval
		    case G_TICKLABELCOLOR:
			GL_TICKLABELCOLOR(p) = nint(rval)
		    case G_TICKCOLOR:
			GL_TICKCOLOR(p) = nint(rval)
		    # case G_TICKFORMAT:
			# not a real parameter
		    default:
			call syserr (SYS_GSET)
		    }
		}
	    }
	}
end


# GST_SET_ATTRIBUTE_I -- Compare the new value of an attribute to the current
# value.  If the new value is not different, exit without modifying the
# attribute packet, making no-op GSET calls efficient.  If the packet must
# be modified, flush any buffered polyline output first else it will be
# written using the new attribute (this is not necessary for text attributes,
# but is harmless and it is unlikely that GSET will be called to modify a
# text attribute while in the midst of building a polyline).  Set the
# parameter and flag the attribute packet as modified.

procedure gst_set_attribute_i (new_value, value, state)

int	new_value		# value in GSET argument list
int	value			# current value in GP struct
int	state			# packet state

begin
	if (new_value != value) {
	    call gpl_flush()
	    value = new_value
	    state = MODIFIED
	}
end


# GST_SET_ATTRIBUTE_R -- Ditto, for real valued parameters.

procedure gst_set_attribute_r (new_value, value, state)

real	new_value		# value in GSET argument list
real	value			# current value in GP struct
int	state			# packet state

begin
	if (abs (new_value - value) > EPSILON) {
	    call gpl_flush()
	    value = new_value
	    state = MODIFIED
	}
end
