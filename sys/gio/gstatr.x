# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<mach.h>
include	<gset.h>
include	<gio.h>

# GSTATR -- Get any GIO parameter of type integer or real.  Integer values are
# silently coerced to real if the actual parameter value is integer.

real procedure gstatr (gp, param)

pointer	gp			# graphics descriptor
int	param			# parameter to be set

real	char_height
int	wcs, axes, field, ax[2], i
pointer	w, p, pl, pm, tx, fa
real	ggetr()

begin
	# Compute pointers to substructures once, here, to save space later.
	wcs = GP_WCS(gp)
	w   = GP_WCSPTR(gp,wcs)
	pl  = GP_PLAP(gp)
	pm  = GP_PMAP(gp)
	tx  = GP_TXAP(gp)
	fa  = GP_FAAP(gp)

	switch (param) {

	# General GIO parameters.

	case G_FD:
	    return (GP_FD(gp))
	case G_TTY:
	    return (GP_TTY(gp))
	case G_WCS:
	    return (GP_WCS(gp))
	case G_CURSOR:
	    return (GP_CURSOR(gp))

	# These parameters affect the current WCS.

	case G_XTRAN:
	    return (WCS_XTRAN(w))
	case G_YTRAN:
	    return (WCS_YTRAN(w))
	case G_CLIP:
	    return (and (WCS_FLAGS(w), WF_CLIP))
	case G_RASTER:
	    return (WF_RASTER (WCS_FLAGS(w)))

	# Default marker sizes (NDC coords).

	case G_SZMARKER1:
	    return (GP_SZMARKER(gp,1))
	case G_SZMARKER2:
	    return (GP_SZMARKER(gp,2))
	case G_SZMARKER3:
	    return (GP_SZMARKER(gp,3))
	case G_SZMARKER4:
	    return (GP_SZMARKER(gp,4))

	# Polyline attributes.

	case G_PLTYPE:
	    return (PL_LTYPE(pl))
	case G_PLWIDTH:
	    return (PL_WIDTH(pl))
	case G_PLCOLOR:
	    return (PL_COLOR(pl))

	# Polymarker attributes.

	case G_PMLTYPE:
	    return (PM_LTYPE(pm))
	case G_PMWIDTH:
	    return (PM_WIDTH(pm))
	case G_PMCOLOR:
	    return (PM_COLOR(pm))

	# Text drawing attributes.

	case G_TXUP:
	    return (TX_UP(tx))
	case G_TXSIZE:
	    return (TX_SIZE(tx))
	case G_TXPATH:
	    return (TX_PATH(tx))
	case G_TXSPACING:
	    return (TX_SPACING(tx))
	case G_TXHJUSTIFY:
	    return (TX_HJUSTIFY(tx))
	case G_TXVJUSTIFY:
	    return (TX_VJUSTIFY(tx))
	case G_TXFONT:
	    return (TX_FONT(tx))
	case G_TXQUALITY:
	    return (TX_QUALITY(tx))
	case G_TXCOLOR:
	    return (TX_COLOR(tx))

	# Fill area attributes.

	case G_FASTYLE:
	    return (FA_STYLE(fa))
	case G_FACOLOR:
	    return (FA_COLOR(fa))

	# Axis labelling parameters affecting more than one axis.

	case G_DRAWTITLE:
	    return (GP_DRAWTITLE(gp))
	case G_TITLESIZE:
	    return (GP_TITLESIZE(gp))
	case G_TITLECOLOR:
	    return (GP_TITLECOLOR(gp))
	case G_NTITLELINES:
	    return (GP_NTITLELINES(gp))
	case G_FRAMECOLOR:
	    return (GP_FRAMECOLOR(gp))
	case G_ASPECT:
	    return (GP_ASPECT(gp))

	case G_CHARSIZE:
	    # Return the current character size in NDC units.

	    char_height = ggetr (gp, "ch")
	    if (char_height < EPSILON)
		char_height = DEF_CHARHEIGHT
	    return (char_height * TX_SIZE(tx))

	default:
	    # The GLABAX parameters for the X and Y axes may be set separately
	    # for each axis or simultaneously for both.  The parameter codes
	    # are encoded as 100 (X only) 200 (Y only) or 300 (both) plus the
	    # code for the field in the lower digits.

	    if (param < FIRST_GLABAX_PARAM || param > LAST_GLABAX_PARAM)
		call syserr (SYS_GSTAT)

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
			return (GL_DRAWAXES(p))
		    case G_SETAXISPOS:
			return (GL_SETAXISPOS(p))
		    case G_AXISPOS1:
			return (GL_AXISPOS1(p))
		    case G_AXISPOS2:
			return (GL_AXISPOS2(p))
		    case G_DRAWGRID:
			return (GL_DRAWGRID(p))
		    case G_GRIDCOLOR:
			return (GL_GRIDCOLOR(p))
		    case G_ROUND:
			return (GL_ROUND(p))
		    case G_LABELAXIS:
			return (GL_LABELAXIS(p))
		    case G_AXISLABELSIZE:
			return (GL_AXISLABELSIZE(p))
		    case G_AXISLABELCOLOR:
			return (GL_AXISLABELCOLOR(p))
		    case G_DRAWTICKS:
			return (GL_DRAWTICKS(p))
		    case G_LABELTICKS:
			return (GL_LABELTICKS(p))
		    case G_NMAJOR:
			return (GL_NMAJOR(p))
		    case G_NMINOR:
			return (GL_NMINOR(p))
		    case G_MAJORLENGTH:
			return (GL_MAJORLENGTH(p))
		    case G_MINORLENGTH:
			return (GL_MINORLENGTH(p))
		    case G_MAJORWIDTH:
			return (GL_MAJORWIDTH(p))
		    case G_MINORWIDTH:
			return (GL_MINORWIDTH(p))
		    case G_AXISWIDTH:
			return (GL_AXISWIDTH(p))
		    case G_AXISCOLOR:
			return (GL_AXISCOLOR(p))
		    case G_TICKLABELSIZE:
			return (GL_TICKLABELSIZE(p))
		    case G_TICKLABELCOLOR:
			return (GL_TICKLABELCOLOR(p))
		    case G_TICKCOLOR:
			return (GL_TICKCOLOR(p))
		    # case G_TICKFORMAT:
			# not a real parameter
		    default:
			call syserr (SYS_GSTAT)
		    }
		}
	    }
	}
end
