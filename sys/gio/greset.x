# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<gset.h>
include	<gio.h>

# GRESET -- Initialize the internal state variables of GIO to their default
# values.  Called upon startup and by GCANCEL and GCLEAR.

procedure greset (gp, flags)

pointer gp			# graphics descriptor
int	flags			# flags indicating what to reset

int	i
bool	reset_wcs, reset_gio, reset_glabax
real	char_height, aspect
pointer	w, ap, ax
real	ggetr()
int	and()
errchk	ggetr

begin
	# Initialize for a new frame; this is always done.
	call gfrinit (gp)

	reset_glabax = (and (flags, GR_RESETGLABAX) != 0)
	reset_wcs    = (and (flags, GR_RESETWCS)    != 0)
	reset_gio    = (and (flags, GR_RESETGIO)    != 0)

	# Reset general GIO device and drawing parameters?
	if (reset_gio) {
	    GP_CURSOR(gp) = 1

	    # All default sizes in NDC units are scaled to the height of a
	    # device character.

	    char_height = ggetr (gp, "ch")
	    if (char_height < EPSILON)
		char_height = DEF_CHARHEIGHT
	    aspect = ggetr (gp, "ar")
	    if (aspect < EPSILON)
		aspect = 1.0
	    GP_DEVASPECT(gp) = aspect

	    # Set default marker sizes.
	    do i = 1, 4
		GP_SZMARKER(gp,i) = (char_height * i) / 4.0

	    # Set polyline attributes.
	    ap = GP_PLAP(gp)
	    PL_LTYPE(ap) = 1
	    PL_WIDTH(ap) = 1.0
	    PL_COLOR(ap) = 1

	    # Set polymarker attributes.
	    ap = GP_PMAP(gp)
	    PM_LTYPE(ap) = 1
	    PM_WIDTH(ap) = 1.0
	    PM_COLOR(ap) = 1

	    # Set fill area attributes.
	    ap = GP_FAAP(gp)
	    FA_STYLE(ap) = 1
	    FA_COLOR(ap) = 1

	    # Set default text attributes.
	    ap = GP_TXAP(gp)
	    TX_UP(ap) = 90
	    TX_SIZE(ap) = 1.0
	    TX_PATH(ap) = GT_RIGHT
	    TX_SPACING(ap) = 0.0
	    TX_HJUSTIFY(ap) = GT_LEFT
	    TX_VJUSTIFY(ap) = GT_DOWN
	    TX_FONT(ap) = GT_ROMAN
	    TX_QUALITY(ap) = GT_NORMAL
	    TX_COLOR(ap) = 1
	}

	# Reset GLABAX parameters?
	if (reset_glabax) {
	    # Set general GLABAX parameters.
	    GP_DRAWTITLE(gp) = YES
	    GP_TITLESIZE(gp) = 1.0
	    GP_TITLEJUST(gp) = GT_CENTER
	    GP_NTITLELINES(gp) = 0

	    # Set GLABAX parameters for the X and Y axes.
	    do i = 1, 2 {
		if (i == 1)
		    ax = GP_XAP(gp)
		else
		    ax = GP_YAP(gp)

		GL_DRAWAXES(ax) = 3
		GL_SETAXISPOS(ax) = NO 
		GL_AXISPOS1(ax) = 0.0
		GL_AXISPOS2(ax) = 0.0
		GL_DRAWGRID(ax) = NO
		GL_ROUND(ax) = NO
		GL_LABELAXIS(ax) = YES
		GL_AXISLABELSIZE(ax) = 1.0
		GL_DRAWTICKS(ax) = YES
		GL_LABELTICKS(ax) = YES
		GL_NMAJOR(ax) = 6
		GL_NMINOR(ax) = 4
		GL_MAJORLENGTH(ax) = 0.6 * char_height
		GL_MINORLENGTH(ax) = 0.3 * char_height
		GL_MAJORWIDTH(ax) = 2.0
		GL_MINORWIDTH(ax) = 2.0
		GL_AXISWIDTH(ax) = 2.0
		GL_TICKLABELSIZE(ax) = 1.0
		GL_TICKFORMAT(ax) = EOS
	    }

	    # Correct the default tick length for the aspect ratio.
	    ax = GP_XAP(gp)
	    GL_MAJORLENGTH(ax) = GL_MAJORLENGTH(ax) / aspect
	    GL_MINORLENGTH(ax) = GL_MINORLENGTH(ax) / aspect
	}

	# Reset the WCS?
	if (reset_wcs) {
	    GP_WCS(gp) = 1

	    # Initialize the WCS to NDC coordinates.
	    do i = 0, MAX_WCS {
		w = GP_WCSPTR(gp,i)
		WCS_WX1(w) = 0.0
		WCS_WX2(w) = 1.0
		WCS_WY1(w) = 0.0
		WCS_WY2(w) = 1.0
		WCS_SX1(w) = 0.0
		WCS_SX2(w) = 1.0
		WCS_SY1(w) = 0.0
		WCS_SY2(w) = 1.0
		WCS_XTRAN(w) = LINEAR
		WCS_YTRAN(w) = LINEAR
		WCS_CLIP(w) = YES
	    }
	}
end
