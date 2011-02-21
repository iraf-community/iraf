# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<error.h>
include	<imhdr.h>
include	<gset.h>
include	<mach.h>
include	"imexam.h"

define	CSIZE		24


# IE_SIMEXAM -- Draw a perspective view of a surface.  The altitude
# and azimuth of the viewing angle are variable.
 
procedure ie_simexam (gp, mode, ie, x, y)
 
pointer	gp			# GIO pointer
int	mode			# Mode
pointer	ie			# IMEXAM pointer
real	x, y			# Center

real	angh, angv		# Orientation of surface (degrees)
real	floor, ceiling		# Range limits
 
int	wkid
int	x1, x2, y1, y2, nx, ny, npts
pointer	pp, sp, title, str, sdata, work, im, data, ie_gimage(), ie_gdata()

bool	clgpsetb()
int	clgpseti()
real	clgpsetr()
pointer	clopset()
 
int	first
real	vpx1, vpx2, vpy1, vpy2
common  /frstfg/ first
common  /noaovp/ vpx1, vpx2, vpy1, vpy2
 
begin
	iferr (im = ie_gimage (ie, NO)) {
	    call erract (EA_WARN)
	    return
	}

	pp = IE_PP(ie)
	if (pp != NULL)
	    call clcpset (pp)
	pp = clopset ("simexam")
	IE_PP(ie) = pp

	nx = clgpseti (pp, "ncolumns")
	ny = clgpseti (pp, "nlines")
	angh = clgpsetr (pp, "angh")
	angv = clgpsetr (pp, "angv")
	floor = clgpsetr (pp, "floor")
	ceiling = clgpsetr (pp, "ceiling")

	if (!IS_INDEF(x))
	    IE_X1(ie) = x
	if (!IS_INDEF(y))
	    IE_Y1(ie) = y

	x1 = IE_X1(ie) - (nx - 1) / 2 + 0.5
	x2 = IE_X1(ie) + nx / 2 + 0.5
	y1 = IE_Y1(ie) - (ny - 1) / 2 + 0.5
	y2 = IE_Y1(ie) + ny / 2 + 0.5
	iferr (data = ie_gdata (im, x1, x2, y1, y2)) {
	    call erract (EA_WARN)
	    return
	}

	nx = x2 - x1 + 1
	ny = y2 - y1 + 1
	npts = nx * ny

	call smark (sp)

	# Take floor and ceiling if enabled (nonzero).
	if (IS_INDEF (floor) && IS_INDEF (ceiling))
	    sdata = data
	else {
	    call salloc (sdata, npts, TY_REAL)
	    call amovr (Memr[data], Memr[sdata], npts)
	    if (!IS_INDEF (floor) && !IS_INDEF (ceiling)) {
		floor = min (floor, ceiling)
		ceiling = max (floor, ceiling)
	    }
	}
	iferr (call ie_surf_limits (Memr[sdata], npts, floor, ceiling)) {
	    call sfree (sp)
	    call erract (EA_WARN)
	    return
	}

	if (mode != APPEND) {
	    call gclear (gp)

	    # Set the viewport.
	    call gsview (gp, 0.1, 0.9, 0.1, 0.9)

	    call salloc (title, IE_SZTITLE, TY_CHAR)
	    call salloc (str, SZ_LINE, TY_CHAR)

	    if (clgpsetb (pp, "banner")) {
		call sysid (Memc[str], SZ_LINE)
		call sprintf (Memc[title], IE_SZTITLE,
		    "%s\n%s: Surface plot of [%d:%d,%d:%d]\n%s")
		    call pargstr (Memc[str])
		    call pargstr (IE_IMNAME(ie))
		    call pargi (x1)
		    call pargi (x2)
		    call pargi (y1)
		    call pargi (y2)
		    call pargstr (IM_TITLE(im))
	    } else
		Memc[title] = EOS

	    call clgpset (pp, "title", Memc[str], SZ_LINE)
	    if (Memc[str] != EOS) {
	        call strcat ("\n", Memc[title], IE_SZTITLE)
	        call strcat (Memc[str], Memc[title], IE_SZTITLE)
	    }

	    call gseti (gp, G_DRAWAXES, NO)
	    call glabax (gp, Memc[title], "", "")
	}

	# Open graphics device and make plot.
	call gopks (STDERR)
	wkid = 1
	call gopwk (wkid, 6, gp)
	call gacwk (wkid)

	first = 1
	call srfabd()
	call ggview (gp, vpx1, vpx2, vpy1, vpy2)
	call set (vpx1, vpx2, vpy1, vpy2, 1.0, 1024., 1.0, 1024., 1)
	call salloc (work, 2 * (2*nx*ny+nx+ny), TY_REAL)
	call ezsrfc (Memr[sdata], nx, ny, angh, angv, Memr[work])

	if (mode != APPEND) {
	    if (clgpsetb (pp, "axes")) {
	        call gswind (gp, real (x1), real (x2), real (y1), real (y2))
	        call gseti (gp, G_CLIP, NO)
	        call ie_perimeter (gp, Memr[sdata], nx, ny, angh, angv)
	    }
	}

	call gdawk (wkid)
	call gclks ()
	call sfree (sp)
end


# IE_PERIMETER -- draw and label axes around the surface plot.

procedure ie_perimeter (gp, z, ncols, nlines, angh, angv)

pointer	gp			# Graphics pointer
int	ncols			# Number of image columns
int	nlines			# Number of image lines
real	z[ncols, nlines]	# Array of intensity values
real	angh			# Angle of horizontal inclination
real	angv			# Angle of vertical inclination

pointer	sp, x_val, y_val, kvec
char	tlabel[10]
real	xmin, ymin, delta, fact1, flo, hi, xcen, ycen
real	x1_perim, x2_perim, y1_perim, y2_perim, z1, z2
real	wc1, wc2, wl1, wl2, del
int	i, j, junk
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

	# Find range of z for determining perspective 
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
		# Case 1: xy rotation positive, looking down from above mid Z

		# First draw x axis
		call amovkr (y2_perim, Memr[kvec], ncols + 2)
		call ie_draw_axis (Memr[x_val+1], Memr[kvec], flo, ncols + 1)
		call ie_label_axis (xcen, y2_perim+del, flo, "X-AXIS", -1, -2)
		call ie_draw_ticksx (Memr[x_val+1], y2_perim, y2_perim+delta, 
		    flo, ncols)
		junk = itoc (int (wc1), tlabel, 10)
		call ie_label_axis (xmin, y2_perim+del, flo, tlabel, -1, -2)
		junk = itoc (int (wc2), tlabel, 10)
		call ie_label_axis (Memr[x_val+ncols], y2_perim+del, flo, 
		    tlabel, -1, -2)

		# Now draw y axis
		call amovkr (x2_perim, Memr[kvec], nlines + 2)
		call ie_draw_axis (Memr[kvec], Memr[y_val+1], flo, nlines + 1)
		call ie_label_axis (x2_perim+del, ycen, flo, "Y-AXIS", 2, -1)
		call ie_draw_ticksy (x2_perim, x2_perim+delta, Memr[y_val+1],
		    flo, nlines)
		junk = itoc (int (wl1), tlabel, 10)
		call ie_label_axis (x2_perim+del, ymin, flo, tlabel, 2, -1)
		junk = itoc (int (wl2), tlabel, 10)
		call ie_label_axis (x2_perim+del, Memr[y_val+nlines], flo, 
		    tlabel, 2, -1)
	    } else {
		# Case 2: xy rotation positive, looking up from below mid Z
		# First draw x axis
		call amovkr (y1_perim, Memr[kvec], ncols + 2)
		call ie_draw_axis (Memr[x_val], Memr[kvec], flo, ncols + 1)
		call ie_label_axis (xcen, y1_perim-del, flo, "X-AXIS", -1, 2)
		call ie_draw_ticksx (Memr[x_val+1], y1_perim, y1_perim-delta, 
		    flo, ncols)
		junk = itoc (int (wc1), tlabel, 10)
		call ie_label_axis (xmin, y1_perim-del, flo, tlabel, -1, 2)
		junk = itoc (int (wc2), tlabel, 10)
		call ie_label_axis (Memr[x_val+ncols], y1_perim-del, flo, 
		    tlabel, -1, 2)

		# Now draw y axis
		call amovkr (x1_perim, Memr[kvec], nlines + 2)
		call ie_draw_axis (Memr[kvec], Memr[y_val], flo, nlines + 1)
		call ie_label_axis (x1_perim-del, ycen, flo, "Y-AXIS", 2, 1)
		call ie_draw_ticksy (x1_perim, x1_perim-delta, Memr[y_val+1],
		    flo, nlines)
		junk = itoc (int (wl1), tlabel, 10)
		call ie_label_axis (x1_perim-del, ymin, flo, tlabel, 2, 1)
		junk = itoc (int (wl2), tlabel, 10)
		call ie_label_axis (x1_perim-del, Memr[y_val+nlines], flo, 
		    tlabel, 2, 1)
	    }
	}

	if (angh < 0) {
	    if (angv > 0) {
		# Case 3: xy rotation negative, looking down from above  mid Z 
		# (default).  First draw x axis
		call amovkr (y1_perim, Memr[kvec], ncols + 2)
		call ie_draw_axis (Memr[x_val+1], Memr[kvec], flo, ncols + 1)
		call ie_label_axis (xcen, y1_perim-del, flo, "X-AXIS", 1, 2)
		call ie_draw_ticksx (Memr[x_val+1], y1_perim, y1_perim-delta, 
		    flo, ncols)
		junk = itoc (int (wc1), tlabel, 10)
		call ie_label_axis (xmin, y1_perim-del, flo, tlabel, 1, 2)
		junk = itoc (int (wc2), tlabel, 10)
		call ie_label_axis (Memr[x_val+ncols], y1_perim-del, flo, 
		    tlabel, 1, 2)

		# Now draw y axis
		call amovkr (x2_perim, Memr[kvec], nlines + 2)
		call ie_draw_axis (Memr[kvec], Memr[y_val], flo, nlines + 1)
		call ie_label_axis (x2_perim+del, ycen, flo, "Y-AXIS", 2, -1)
		call ie_draw_ticksy (x2_perim, x2_perim+delta, Memr[y_val+1],
		    flo, nlines)
		junk = itoc (int (wl1), tlabel, 10)
		call ie_label_axis (x2_perim+del, ymin, flo, tlabel, 2, -1)
		junk = itoc (int (wl2), tlabel, 10)
		call ie_label_axis (x2_perim+del, Memr[y_val+nlines], flo, 
		    tlabel, 2, -1)
	    } else {
		# Case 4: xy rotation negative, looking up from below mid Z
		# First draw x axis
		call amovkr (y2_perim, Memr[kvec], ncols + 2)
		call ie_draw_axis (Memr[x_val], Memr[kvec], flo, ncols + 1)
		call ie_label_axis (xcen, y2_perim+del, flo, "X-AXIS", 1, -2)
		call ie_draw_ticksx (Memr[x_val+1], y2_perim, y2_perim+delta,
		    flo, ncols)
		junk = itoc (int (wc1), tlabel, 10)
		call ie_label_axis (xmin, y2_perim+del, flo, tlabel, 1, -2)
		junk = itoc (int (wc2), tlabel, 10)
		call ie_label_axis (Memr[x_val+ncols], y2_perim+del, flo, 
		    tlabel, 1, -2)

		# Now draw y axis
		call amovkr (x1_perim, Memr[kvec], nlines + 2)
		call ie_draw_axis (Memr[kvec], Memr[y_val+1], flo, nlines + 1)
		call ie_label_axis (x1_perim-del, ycen, flo, "Y-AXIS", 2, 1)
		call ie_draw_ticksy (x1_perim, x1_perim-delta, Memr[y_val+1],
		    flo, nlines)
		junk = itoc (int (wl1), tlabel, 10)
		call ie_label_axis (x1_perim-del, ymin, flo, tlabel, 2, 1)
		junk = itoc (int (wl2), tlabel, 10)
		call ie_label_axis (x1_perim-del, Memr[y_val+nlines], flo, 
		    tlabel, 2, 1)
	    }
	}

	# Flush plotit buffer before returning
	call plotit (0, 0, 2)
	call sfree (sp)
end


# ??

procedure ie_draw_axis (xvals, yvals, zval, nvals)

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


# ??

procedure ie_label_axis (xval, yval, zval, sppstr, path, up)

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


# ??

procedure ie_draw_ticksx (x, y1, y2, zval, nvals)

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


# ??

procedure ie_draw_ticksy (x1, x2, y, zval, nvals)

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


# IE_SURF_LIMITS -- Apply the floor and ceiling constraints to the subraster.
# If either value is exactly zero, it is not applied.

procedure ie_surf_limits (ras, m, floor, ceiling)

real	ras[m]
int	m
real	floor, ceiling
real	val1_1			# value at ras[1]
int	k
bool	const_val		# true if data are constant
bool	bad_floor		# true if no value is above floor
bool	bad_ceiling		# true if no value is below ceiling

begin
	const_val = true		# initial values
	bad_floor = true
	bad_ceiling = true
	val1_1 = ras[1]

	do k = 1, m
	    if (ras[k] != val1_1) {
		const_val = false
		break
	    }
	if (!IS_INDEF(floor)) {
	    do k = 1, m {
		if (ras[k] <= floor)
		    ras[k] = floor
		else
		    bad_floor = false
	    }
	}
	if (!IS_INDEF(ceiling)) {
	    do k = 1, m {
		if (ras[k] >= ceiling)
		    ras[k] = ceiling
		else
		    bad_ceiling = false
	    }
	}

	if (bad_floor && !IS_INDEF(floor))
	    call error (1, "entire image is below (or at) specified floor")
	if (bad_ceiling && !IS_INDEF(ceiling))
	    call error (1, "entire image is above (or at) specified ceiling")
	if (const_val)
	    call error (1, "all data values are the same; can't plot it")
end
