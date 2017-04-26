include	<fset.h>
include	<mach.h>
include	<error.h>
include	<gset.h>
include	<config.h>
include	<xwhen.h>
include "../lib/daophotdef.h"
include "../lib/psfdef.h"

define	DUMMY	6

# DP_SURFPSF -- Draw a perspective view of a subraster with a given altitude
# and azimuth of the viewing angle.  Floor and ceiling constraints may be
# applied to the data before plotting.

procedure dp_surfpsf (dao, subras, ncols, nlines, title, gd)

pointer	dao				# pointer to DAOPHOT structure
real	subras[ncols,nlines]		# pointer to subraster
int	ncols, nlines			# dimensions of subraster
char	title[ARB]			# title string
pointer	gd				# pointer to graphics stream

char	sysidstr[SZ_LINE]
int	first, wkid, epa, status, old_onint, tsujmp[LEN_JUMPBUF]
pointer	sp, temp, work, psf
real	angh, angv, imcols, imlines, floor, ceiling, vpx1, vpx2, vpy1, vpy2

extern	dp_sonint()
common	/tsucom/ tsujmp
common  /noaovp/ vpx1, vpx2, vpy1, vpy2
common  /frstfg/ first

begin
	# Get the psf fitting substructure pointer.
	psf = DP_PSF(dao)

	# Initialize surface common blocks before changing any parameters.
	first = 1
	call srfabd ()

	# Set local variables.
	angh = DP_MANGH (psf)
	angv = DP_MANGV (psf)
	floor = DP_MFLOOR (psf)
	ceiling = DP_MCEILING (psf)
	floor = min (floor, ceiling)
	ceiling = max (floor, ceiling)

	# Allow room for axes and labels.
	vpx1 = 0.10
	vpx2 = 0.90
	vpy1 = 0.10
	vpy2 = 0.90

	# Make a copy of the subraster so we can subtract the zero level.
	imcols = real (ncols)
	imlines = real (nlines)
	call smark (sp)
	call salloc (temp, ncols * nlines, TY_REAL)
	call amovr (subras, Memr[temp], nlines * ncols)

	# Allocate the working storage needed by EZSRFC.
	call malloc (work, 2 * (2 * ncols * nlines + ncols + nlines), TY_REAL)

	# Take off floor and ceiling if enabled (nonzero).
	call dp_slimits (Memr[temp], ncols, nlines, floor, ceiling)

	# Set up the titles and the viewport.
	call gopks (STDERR)
	wkid = 1
	call gclear (gd)
	call gopwk (wkid, DUMMY, gd)
	call gacwk (wkid)
	call gtext (gd, 0.5, .96, title, "s=0.8;f=b;h=c")
	call sysid (sysidstr, SZ_LINE)
	call gtext (gd, 0.5, .04, sysidstr, "h=c;v=b;s=.5")
	call set (vpx1, vpx2, vpy1, vpy2, 1.0, 1024., 1.0, 1024., 1)

	# Install interrupt exception handler.
	call zlocpr (dp_sonint, epa)
	call xwhen (X_INT, epa, old_onint)

	# Plot the surface.
	call zsvjmp (tsujmp, status)
	if (status == OK)
	    call ezsrfc (Memr[temp], ncols, nlines, angh, angv, Memr[work])
	else {
	    call gcancel (gd)
	    call fseti (STDOUT, F_CANCEL, OK)
	}
	
	# Draw the perimeter.
	call gswind (gd, 1.0, imcols, 1.0, imlines)
	call gseti (gd, G_CLIP, NO)
	call dp_sperimeter (gd, Memr[temp], ncols, nlines, angh, angv)

	# Clean up.
	call gdawk (wkid)
	call gclks ()
	call mfree (work, TY_REAL)
	call sfree (sp)
end


# DP_SONINT -- Interrupt handler for the task surface.  Branches back to ZSVJMP
# in the main routine to permit shutdown without an error message.

procedure dp_sonint (vex, next_handler)

int	vex		# virtual exception
int	next_handler	# not used

int	tsujmp[LEN_JUMPBUF]
common	/tsucom/ tsujmp

begin
	call xer_reset()
	call zdojmp (tsujmp, vex)
end


# DP_SLIMITS -- Apply the floor and ceiling constraints to the subraster.
# If either value is exactly zero, it is not applied.

procedure dp_slimits (ras, m, n, floor, ceiling)

real	ras[m,n]
int	m, n
real	floor, ceiling
int	i

begin
	do i = 1, n {
	    if (floor != 0)
		call amaxkr (ras[1,i], floor, ras[1,i], m)
	    if (ceiling != 0)
		call aminkr (ras[1,i], ceiling, ras[1,i], m)
	}
end


define	SZ_TLABEL	10

# DP_SPERIMETER -- draw and label axes around the surface plot.

procedure dp_sperimeter (gp, z, ncols, nlines, angh, angv)

pointer	gp			# Graphics pointer
int	ncols			# Number of image columns
int	nlines			# Number of image lines
real	z[ncols, nlines]	# Array of intensity values
real	angh			# Angle of horizontal inclination
real	angv			# Angle of vertical inclination

pointer	sp, x_val, y_val, kvec
char	tlabel[SZ_TLABEL]
real	xmin, ymin, delta, fact1, flo, hi, xcen, ycen
real	x1_perim, x2_perim, y1_perim, y2_perim, z1, z2
real	wc1, wc2, wl1, wl2, del
int	i, j
int	itoc()
data  	fact1 /2.0/
real	vpx1, vpx2, vpy1, vpy2
common	/noaovp/ vpx1, vpx2, vpy1, vpy2

begin
	call smark (sp)
	call salloc (x_val,   ncols + 2,  TY_REAL)
	call salloc (y_val,  nlines + 2, TY_REAL)
	call salloc (kvec, max (ncols, nlines) + 2, TY_REAL)

	# Get window coordinates set up in calling procedure.
	call ggwind (gp, wc1, wc2, wl1, wl2)

	# Set up window, viewport for output.  The coordinates returned
	# from trn32s are in the range [1-1024].
	call set (vpx1, vpx2, vpy1, vpy2, 1.0, 1024., 1.0, 1024., 1)

	# Find range of z for determining perspective.
	flo = MAX_REAL
	hi = -flo
	do j = 1, nlines {
	    call alimr (z[1,j], ncols, z1, z2)
	    flo = min (flo, z1)
	     hi = max (hi, z2)
	}

	# Set up linear endpoints and spacing as used in surface.

        delta = (hi-flo) / (max (ncols,nlines) -1.) * fact1
        xmin = -(real (ncols/2)  * delta + real (mod (ncols+1, 2))  * delta)
        ymin = -(real (nlines/2) * delta + real (mod (nlines+1, 2)) * delta)
	del = 2.0 * delta

	# The perimeter is separated from the surface plot by the 
	# width of delta.  

	x1_perim = xmin - delta
	y1_perim = ymin - delta
	x2_perim = xmin + (real (ncols)  * delta)
	y2_perim = ymin + (real (nlines) * delta)

	# Set up linear arrays over full perimeter range
	do i = 1, ncols + 2
	    Memr[x_val+i-1] = x1_perim + (i-1) * delta
	do i = 1, nlines + 2
	    Memr[y_val+i-1] = y1_perim + (i-1) * delta

	# Draw and label axes and tick marks.
	# It is important that frame has not been called after calling srface.
	# First to draw the perimeter.  Which axes get drawn depends on the
	# values of angh and angv.  Get angles in the range [-180, 180].

	if (angh > 180.)
	    angh = angh - 360.
	else if (angh < -180.)
	    angh = angh + 360.

	if (angv > 180.)
	    angv = angv - 360.
	else if (angv < -180.)
	    angv = angv + 360.

	# Calculate positions for the axis labels
	xcen = 0.5 * (x1_perim + x2_perim)
	ycen = 0.5 * (y1_perim + y2_perim)

	if (angh >= 0) {
	    if (angv >= 0) {
		# Case 1: xy rotation positive, looking down from above mid z.

		# First draw x axis.
		call amovkr (y2_perim, Memr[kvec], ncols + 2)
		call dp_draw_axis (Memr[x_val+1], Memr[kvec], flo, ncols + 1)
		call dp_label_axis (xcen, y2_perim+del, flo, "X-AXIS", -1, -2)
		call dp_draw_ticksx (Memr[x_val+1], y2_perim, y2_perim+delta, 
		    flo, ncols)
		call dp_label_axis (xmin, y2_perim+del, flo, "1", -1, -2)
		if (itoc (int (wc2), tlabel, SZ_TLABEL) <= 0)
		    tlabel[1] = EOS
		call dp_label_axis (Memr[x_val+ncols], y2_perim+del, flo, 
		    tlabel, -1, -2)

		# Now draw y axis.
		call amovkr (x2_perim, Memr[kvec], nlines + 2)
		call dp_draw_axis (Memr[kvec], Memr[y_val+1], flo, nlines + 1)
		call dp_label_axis (x2_perim+del, ycen, flo, "Y-AXIS", 2, -1)
		call dp_draw_ticksy (x2_perim, x2_perim+delta, Memr[y_val+1],
		    flo, nlines)
		call dp_label_axis (x2_perim+del, ymin, flo, "1", 2, -1)
		if (itoc (int (wl2), tlabel, SZ_TLABEL) <= 0)
		    tlabel[1] = EOS
		call dp_label_axis (x2_perim+del, Memr[y_val+nlines], flo, 
		    tlabel, 2, -1)

	    } else {
		# Case 2: xy rotation positive, looking up from below mid z.

		# First draw x axis.
		call amovkr (y1_perim, Memr[kvec], ncols + 2)
		call dp_draw_axis (Memr[x_val], Memr[kvec], flo, ncols + 1)
		call dp_label_axis (xcen, y1_perim-del, flo, "X-AXIS", -1, 2)
		call dp_draw_ticksx (Memr[x_val+1], y1_perim, y1_perim-delta, 
		    flo, ncols)
		call dp_label_axis (xmin, y1_perim-del, flo, "1", -1, 2)
		if (itoc (int (wc2), tlabel, SZ_TLABEL) <= 0)
		    tlabel[1] = EOS
		call dp_label_axis (Memr[x_val+ncols], y1_perim-del, flo, 
		    tlabel, -1, 2)

		# Now draw y axis.
		call amovkr (x1_perim, Memr[kvec], nlines + 2)
		call dp_draw_axis (Memr[kvec], Memr[y_val], flo, nlines + 1)
		call dp_label_axis (x1_perim-del, ycen, flo, "Y-AXIS", 2, 1)
		call dp_draw_ticksy (x1_perim, x1_perim-delta, Memr[y_val+1],
		    flo, nlines)
		call dp_label_axis (x1_perim-del, ymin, flo, "1", 2, 1)
		if (itoc (int (wl2), tlabel, SZ_TLABEL) <= 0)
		    tlabel[1] = EOS
		call dp_label_axis (x1_perim-del, Memr[y_val+nlines], flo, 
		    tlabel, 2, 1)
	    }
	}

	if (angh < 0) {
	    if (angv > 0) {

		# Case 3: xy rotation negative, looking down from above  mid z 
		# (default).  First draw x axis.
		call amovkr (y1_perim, Memr[kvec], ncols + 2)
		call dp_draw_axis (Memr[x_val+1], Memr[kvec], flo, ncols + 1)
		call dp_label_axis (xcen, y1_perim-del, flo, "X-AXIS", 1, 2)
		call dp_draw_ticksx (Memr[x_val+1], y1_perim, y1_perim-delta, 
		    flo, ncols)
		call dp_label_axis (xmin, y1_perim-del, flo, "1", 1, 2)
		if (itoc (int (wc2), tlabel, SZ_TLABEL) <= 0)
		    tlabel[1] = EOS
		call dp_label_axis (Memr[x_val+ncols], y1_perim-del, flo, 
		    tlabel, 1, 2)

		# Now draw y axis.
		call amovkr (x2_perim, Memr[kvec], nlines + 2)
		call dp_draw_axis (Memr[kvec], Memr[y_val], flo, nlines + 1)
		call dp_label_axis (x2_perim+del, ycen, flo, "Y-AXIS", 2, -1)
		call dp_draw_ticksy (x2_perim, x2_perim+delta, Memr[y_val+1],
		    flo, nlines)
		call dp_label_axis (x2_perim+del, ymin, flo, "1", 2, -1)
		if (itoc (int (wl2), tlabel, SZ_TLABEL) <= 0)
		    tlabel[1] = EOS
		call dp_label_axis (x2_perim+del, Memr[y_val+nlines], flo, 
		    tlabel, 2, -1)
	    } else {

		# Case 4: xy rotation negative, looking up from below mid z.
		# First draw x axis.
		call amovkr (y2_perim, Memr[kvec], ncols + 2)
		call dp_draw_axis (Memr[x_val], Memr[kvec], flo, ncols + 1)
		call dp_label_axis (xcen, y2_perim+del, flo, "X-AXIS", 1, -2)
		call dp_draw_ticksx (Memr[x_val+1], y2_perim, y2_perim+delta,
		    flo, ncols)
		call dp_label_axis (xmin, y2_perim+del, flo, "1", 1, -2)
		if (itoc (int (wc2), tlabel, SZ_TLABEL) <= 0)
		    tlabel[1] = EOS
		call dp_label_axis (Memr[x_val+ncols], y2_perim+del, flo, 
		    tlabel, 1, -2)

		# Now draw y axis.
		call amovkr (x1_perim, Memr[kvec], nlines + 2)
		call dp_draw_axis (Memr[kvec], Memr[y_val+1], flo, nlines + 1)
		call dp_label_axis (x1_perim-del, ycen, flo, "Y-AXIS", 2, 1)
		call dp_draw_ticksy (x1_perim, x1_perim-delta, Memr[y_val+1],
		    flo, nlines)
		call dp_label_axis (x1_perim-del, ymin, flo, "1", 2, 1)
		if (itoc (int (wl2), tlabel, SZ_TLABEL) <= 0)
		    tlabel[1] = EOS
		call dp_label_axis (x1_perim-del, Memr[y_val+nlines], flo, 
		    tlabel, 2, 1)
	    }
	}

	# Flush plotit buffer before returning.
	call plotit (0, 0, 2)
	call sfree (sp)
end


# DP_DRAW_AXIS -- Draw the axes around the surface plot.

procedure dp_draw_axis (xvals, yvals, zval, nvals)

int	nvals
real	xvals[nvals]
real	yvals[nvals]
real	zval
pointer	sp, xt, yt
int	i
real	dum

begin
	call smark (sp)
	call salloc (xt, nvals, TY_REAL)
	call salloc (yt, nvals, TY_REAL)

	do i = 1, nvals 
	    call trn32s (xvals[i], yvals[i], zval, Memr[xt+i-1], Memr[yt+i-1], 
		dum, 1)
	call gpl (nvals, Memr[xt], Memr[yt])
	call sfree (sp)
end


define	CSIZE		24


# DP_LABEL_AXIS -- Draw the axes labels.

procedure dp_label_axis (xval, yval, zval, sppstr, path, up)

real	xval
real	yval
real	zval
char	sppstr[SZ_LINE]
int	path
int	up

int	nchars
int	strlen()
%	character*64 fstr

begin
	nchars = strlen (sppstr)
%	call f77pak (sppstr, fstr, 64)
    	call pwrzs (xval, yval, zval, fstr, nchars, CSIZE, path, up, 0)
end


# DP_DRAW_TICKS -- Draw the x tick marks.

procedure dp_draw_ticksx (x, y1, y2, zval, nvals)

int	nvals
real	x[nvals]
real	y1, y2
real	zval

int	i
real	tkx[2], tky[2], dum

begin
	do i = 1, nvals {
	    call trn32s (x[i], y1, zval, tkx[1], tky[1], dum, 1)
	    call trn32s (x[i], y2, zval, tkx[2], tky[2], dum, 1)
	    call gpl (2, tkx[1], tky[1])
	}
end


# DP_DRAW_TICKSY -- Draw the y tick marks.

procedure dp_draw_ticksy (x1, x2, y, zval, nvals)

int	nvals
real	x1, x2
real	y[nvals]
real	zval

int	i
real	tkx[2], tky[2], dum

begin
	do i = 1, nvals {
	    call trn32s (x1, y[i], zval, tkx[1], tky[1], dum, 1)
	    call trn32s (x2, y[i], zval, tkx[2], tky[2], dum, 1)
	    call gpl (2, tkx[1], tky[1])
	}
end
