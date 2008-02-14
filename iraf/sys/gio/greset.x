# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<gset.h>
include	<gio.h>

# GRESET -- Initialize the internal state variables of GIO to their default
# values.  Called upon startup and by GCANCEL and GCLEAR.

procedure greset (gp, flags)

pointer gp			#I graphics descriptor
int	flags			#I flags indicating what to reset

int	color, ch, i
real	char_height, aspect
bool	reset_wcs, reset_gio, reset_glabax
pointer	sp, glbcolor, param, w, ap, ax, ax1, ax2, ip, op

bool	streq()
real	ggetr()
int	envfind(), ctoi(), strncmp()
define	next_ 91
errchk	ggetr

begin
	call smark (sp)
	call salloc (glbcolor, SZ_LINE, TY_CHAR)
	call salloc (param, SZ_FNAME, TY_CHAR)

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
	    GP_TITLECOLOR(gp) = 1
	    GP_TITLEJUST(gp) = GT_CENTER
	    GP_NTITLELINES(gp) = 0
	    GP_FRAMECOLOR(gp) = 0
	    GP_FRAMEDRAWN(gp) = 0

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
		GL_GRIDCOLOR(ax) = 1
		GL_ROUND(ax) = NO
		GL_LABELAXIS(ax) = YES
		GL_AXISLABELSIZE(ax) = 1.0
		GL_AXISLABELCOLOR(ax) = 1
		GL_DRAWTICKS(ax) = YES
		GL_LABELTICKS(ax) = YES
		GL_NMAJOR(ax) = 6
		GL_NMINOR(ax) = 4
		GL_MAJORLENGTH(ax) = 0.6 * char_height
		GL_MINORLENGTH(ax) = 0.3 * char_height
		GL_MAJORWIDTH(ax) = 2.0
		GL_MINORWIDTH(ax) = 2.0
		GL_AXISWIDTH(ax) = 2.0
		GL_AXISCOLOR(ax) = 1
		GL_TICKLABELSIZE(ax) = 1.0
		GL_TICKLABELCOLOR(ax) = 1
		GL_TICKCOLOR(ax) = 1
		GL_TICKFORMAT(ax) = EOS
	    }

	    # Correct the default tick length for the aspect ratio.
	    ax = GP_XAP(gp)
	    GL_MAJORLENGTH(ax) = GL_MAJORLENGTH(ax) / aspect
	    GL_MINORLENGTH(ax) = GL_MINORLENGTH(ax) / aspect

	    # Set user color defaults if specified.  This is a simple string
	    # parameter of the form "pt=i,fr=i,ax=i,..." where I is the color
	    # index.  The actual color corresponding to this index is defined
	    # externally, e.g. by the graphics server.

	    if (envfind ("glbcolor", Memc[glbcolor], SZ_LINE) > 0) {
		ax1 = GP_XAP(gp)
		ax2 = GP_YAP(gp)

		for (ip=glbcolor;  Memc[ip] != EOS;  ) {
		    # Get color parameter code.
		    for (op=param;  Memc[ip] != EOS &&
			Memc[ip] != '=' && Memc[ip] != ':';  ip=ip+1) {
			Memc[op] = Memc[ip]
			op = op + 1
		    }
		    Memc[op] = EOS
		    ch = Memc[param+2]

		    # Get color index.
		    if (Memc[ip] == '=' || Memc[ip] == ':')
			ip = ip + 1
		    if (ctoi (Memc, ip, color) <= 0)
			goto next_

		    # Set parameter.  The two character parameter name may
		    # have an "x" or "y" appended to set only one axis.  For
		    # example, "pt=4,fr=3,ax=1,tk=1,al=5,tl=6".  The color
		    # parameter code names are as follows:
		    #
		    #   pt	plot title
		    #   fr	viewport frame
		    #   gr[xy]	grid between tick marks
		    #	ax[xy]	axis
		    #	al[xy]	axis label
		    #	tk[xy]	tick
		    #	tl[xy]	tick label
		    #
		    # The color codes are simple integers corresponding to
		    # graphics device color codes, e.g. 0, 1, 2, and so on.

		    if (streq (Memc[param], "pt")) {
			GP_TITLECOLOR(gp) = color
		    } else if (streq (Memc[param], "fr")) {
			GP_FRAMECOLOR(gp) = color
		    } else if (strncmp (Memc[param], "gr", 2) == 0) {
			if (ch == EOS || ch == 'x')
			    GL_GRIDCOLOR(ax1) = color
			if (ch == EOS || ch == 'y')
			    GL_GRIDCOLOR(ax2) = color
		    } else if (strncmp (Memc[param], "ax", 2) == 0) {
			if (ch == EOS || ch == 'x')
			    GL_AXISCOLOR(ax1) = color
			if (ch == EOS || ch == 'y')
			    GL_AXISCOLOR(ax2) = color
		    } else if (strncmp (Memc[param], "al", 2) == 0) {
			if (ch == EOS || ch == 'x')
			    GL_AXISLABELCOLOR(ax1) = color
			if (ch == EOS || ch == 'y')
			    GL_AXISLABELCOLOR(ax2) = color
		    } else if (strncmp (Memc[param], "tk", 2) == 0) {
			if (ch == EOS || ch == 'x')
			    GL_TICKCOLOR(ax1) = color
			if (ch == EOS || ch == 'y')
			    GL_TICKCOLOR(ax2) = color
		    } else if (strncmp (Memc[param], "tl", 2) == 0) {
			if (ch == EOS || ch == 'x')
			    GL_TICKLABELCOLOR(ax1) = color
			if (ch == EOS || ch == 'y')
			    GL_TICKLABELCOLOR(ax2) = color
		    }
next_
		while (Memc[ip] != EOS && Memc[ip] != ',')
		    ip = ip + 1
		if (Memc[ip] == ',')
		    ip = ip + 1
		}
	    }
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
		WCS_FLAGS(w) = WF_NEWFORMAT+WF_CLIP
	    }
	}

	call sfree (sp)
end
