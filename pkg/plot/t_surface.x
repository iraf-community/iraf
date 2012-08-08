include	<error.h>
include	<mach.h>
include	<gset.h>
include	<config.h>
include	<xwhen.h>
include	<imhdr.h>
include	<fset.h>

define	DUMMY		6
define	SZ_TLABEL	10
define	CSIZE		24


# SURFACE -- Draw a perspective view of an image section.  The altitude
# and azimuth of the viewing angle are variable.  Floor and ceiling
# constraints may be applied to the image data before plotting if desired.

procedure t_surface()

char	imsect[SZ_FNAME]
char	device[SZ_FNAME], title[SZ_LINE]
bool	label, sub, pre
pointer	im, subras, work
int	ncols, nlines, mode, wkid, nx, ny, npix
int	epa, status, old_onint, tsujmp[LEN_JUMPBUF]
int	xres, yres, first
real	angh, angv, imcols, imlines
real	floor, ceiling, vpx1, vpx2, vpy1, vpy2

pointer gp, gopen()
bool	clgetb(), streq()
int	clgeti(), surf_limits()
real	clgetr()
extern	tsu_onint()
pointer	immap(), plt_getdata()
common	/tsucom/ tsujmp
common  /noaovp/ vpx1, vpx2, vpy1, vpy2
common  /frstfg/ first

define	exit_ 91
begin
	# First initialize srface common blocks before changing any parameters
	first = 1
	call srfabd

	# Get image section string and output device.
	call clgstr ("image", imsect, SZ_FNAME)
	call clgstr ("device", device, SZ_FNAME)

	# Map image and open graphics device.
	im = immap (imsect, READ_ONLY, 0)

	angh = clgetr ("angh")
	angv = clgetr ("angv")
	floor = clgetr ("floor")
	ceiling = clgetr ("ceiling")
	label = clgetb ("label")

	call clgstr ("title", title, SZ_LINE)
	if (streq (title, "imtitle")) {
	    call strcpy (imsect, title, SZ_LINE)
	    call strcat (": ", title, SZ_LINE)
	    call strcat (IM_TITLE(im), title, SZ_LINE)
	}

	# If a label is to be drawn, don't use the full device viewport for
	# the surface plot.  This doesn't allow room for the axes and labels.

	if (label) {
	    vpx1 = 0.10
	    vpx2 = 0.90
	    vpy1 = 0.10
	    vpy2 = 0.90
	} 

	mode = NEW_FILE
	if (clgetb ("append"))
	    mode = APPEND

	ncols  = IM_LEN(im,1)
	nlines = IM_LEN(im,2)
	imcols = real (ncols)
	imlines = real (nlines)

	xres = clgeti ("xres")
	yres = clgeti ("yres")
	sub = clgetb ("subsample")
	pre = clgetb ("preserve")
	
	# Get data with proper resolution.  Procedure plt_getdata returns a
	# pointer to the data matrix to be contoured.  The resolution is
	# decreased by the specified method in this procedure.  The image
	# header pointer can be unmapped after plt_getdata is called.

	nx = 0
	ny = 0
	subras = plt_getdata (im, sub, pre, xres, yres, nx, ny)
	call imunmap (im)

	# Allocate the working storage needed by EZSRFC.
	#call malloc (work, (2 * nx * ny) + nx + ny, TY_REAL)
	call malloc (work, 2 * ((2 * nx * ny) + nx + ny), TY_REAL)

	# Take floor and ceiling if enabled (nonzero).
	npix = nx * ny
	if (surf_limits (Memr[subras], npix, floor, ceiling) == ERR)
	    goto exit_

	# Open graphics device and make plot.
	call gopks (STDERR)
	wkid = 1
	gp = gopen (device, mode, STDGRAPH)
	call gopwk (wkid, DUMMY, gp)
	call gacwk (wkid)
	call gtext (gp, 0.5, .96, title, "s=0.8;f=b;h=c")
	call set (vpx1, vpx2, vpy1, vpy2, 1.0, 1024., 1.0, 1024., 1)

	# Install interrupt exception handler.
	call zlocpr (tsu_onint, epa)
	call xwhen (X_INT, epa, old_onint)

	call zsvjmp (tsujmp, status)
	if (status == OK)
	    call ezsrfc (Memr[subras], nx, ny, angh, angv, Memr[work])
	else {
	    call gcancel (gp)
	    call fseti (STDOUT, F_CANCEL, OK)
	}

	if (label) {
	    # Establish plotting window in full scale image coordinates.
	    call gswind (gp, 1.0, imcols, 1.0, imlines)
	    call gseti (gp, G_CLIP, NO)
	    call srf_perimeter (gp, Memr[subras], nx, ny, angh, angv)
	}

	call gdawk (wkid)
	call gclwk (wkid)
	call gclks ()
exit_
	call mfree (subras, TY_REAL)
	call mfree (work, TY_REAL)

end


# TSU_ONINT -- Interrupt handler for the task surface.  Branches back to ZSVJMP
# in the main routine to permit shutdown without an error message.

procedure tsu_onint (vex, next_handler)

int	vex		# virtual exception
int	next_handler	# not used

int	tsujmp[LEN_JUMPBUF]
common	/tsucom/ tsujmp

begin
	call xer_reset()
	call zdojmp (tsujmp, vex)
end


# SURF_LIMITS -- Apply the floor and ceiling constraints to the subraster.
# If both values are exactly zero, they are not applied.

int procedure surf_limits (ras, npix, floor, ceiling)

real	ras[npix]		# Input array of pixels
int	npix			# npixels in array
real	floor, ceiling		# user specified parameters
int	apply, i
real	tfloor, tceiling, delta, dmin, dmax
bool	fp_equalr()

begin
	tfloor = floor
	tceiling = ceiling
	apply = YES

	call alimr (ras, npix, dmin, dmax)
	if (fp_equalr (dmin, dmax)) {
	    call eprintf ("Constant valued array; no plot drawn\n")
	    return (ERR)
	}

	if (fp_equalr (tfloor, INDEF))
	    tfloor = dmin
	if (fp_equalr (tceiling, INDEF))
	    tceiling = dmax

	delta = tceiling - tfloor
	if (delta < 0.0) {
	    # specified ceiling is lower than floor, flip them
	    floor = tceiling
	    ceiling = tfloor
	} else if (fp_equalr (delta, 0.0)) {
	    # degenerate values
	    apply = NO
	    floor = dmin
	    ceiling = dmax
	    call eprintf (
 "Floor and ceiling are degenerate values and will be ignored\n")
	} else {
	    # Non-degenerate, ceiling exceedes floor as expected
	    floor = tfloor
	    ceiling = tceiling
	}

	if (apply == YES) {
	    # First verify that floor and ceiling are valid
	    if (dmax <= floor) {
	        call eprintf ("Entire image is at or below specified floor\n")
		return (ERR)
	    }
	    if (dmin >= ceiling) {
	        call eprintf ("Entire image is at or above specified ceiling\n")
		return (ERR)
	    }

	    do i = 1, npix {
		# Apply surface limits
		ras[i] = max (floor, ras[i])
		ras[i] = min (ceiling, ras[i])
	    }
	}
	 return (OK)
end


# SRF_PERIMETER -- draw and label axes around the surface plot.

procedure srf_perimeter (gp, z, ncols, nlines, angh, angv)

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
		call srf_draw_axis (Memr[x_val+1], Memr[kvec], flo, ncols + 1)
		call srf_label_axis (xcen, y2_perim+del, flo, "X-AXIS", -1, -2)
		call srf_draw_ticksx (Memr[x_val+1], y2_perim, y2_perim+delta, 
		    flo, ncols)
		call srf_label_axis (xmin, y2_perim+del, flo, "1", -1, -2)
		junk = itoc (int (wc2), tlabel, SZ_TLABEL)
		call srf_label_axis (Memr[x_val+ncols], y2_perim+del, flo, 
		    tlabel, -1, -2)

		# Now draw y axis
		call amovkr (x2_perim, Memr[kvec], nlines + 2)
		call srf_draw_axis (Memr[kvec], Memr[y_val+1], flo, nlines + 1)
		call srf_label_axis (x2_perim+del, ycen, flo, "Y-AXIS", 2, -1)
		call srf_draw_ticksy (x2_perim, x2_perim+delta, Memr[y_val+1],
		    flo, nlines)
		call srf_label_axis (x2_perim+del, ymin, flo, "1", 2, -1)
		junk = itoc (int (wl2), tlabel, SZ_TLABEL)
		call srf_label_axis (x2_perim+del, Memr[y_val+nlines], flo, 
		    tlabel, 2, -1)
	    } else {
		# Case 2: xy rotation positive, looking up from below mid Z
		# First draw x axis
		call amovkr (y1_perim, Memr[kvec], ncols + 2)
		call srf_draw_axis (Memr[x_val], Memr[kvec], flo, ncols + 1)
		call srf_label_axis (xcen, y1_perim-del, flo, "X-AXIS", -1, 2)
		call srf_draw_ticksx (Memr[x_val+1], y1_perim, y1_perim-delta, 
		    flo, ncols)
		call srf_label_axis (xmin, y1_perim-del, flo, "1", -1, 2)
		junk = itoc (int (wc2), tlabel, SZ_TLABEL)
		call srf_label_axis (Memr[x_val+ncols], y1_perim-del, flo, 
		    tlabel, -1, 2)

		# Now draw y axis
		call amovkr (x1_perim, Memr[kvec], nlines + 2)
		call srf_draw_axis (Memr[kvec], Memr[y_val], flo, nlines + 1)
		call srf_label_axis (x1_perim-del, ycen, flo, "Y-AXIS", 2, 1)
		call srf_draw_ticksy (x1_perim, x1_perim-delta, Memr[y_val+1],
		    flo, nlines)
		call srf_label_axis (x1_perim-del, ymin, flo, "1", 2, 1)
		junk = itoc (int (wl2), tlabel, SZ_TLABEL)
		call srf_label_axis (x1_perim-del, Memr[y_val+nlines], flo, 
		    tlabel, 2, 1)
	    }
	}

	if (angh < 0) {
	    if (angv > 0) {
		# Case 3: xy rotation negative, looking down from above  mid Z 
		# (default).  First draw x axis
		call amovkr (y1_perim, Memr[kvec], ncols + 2)
		call srf_draw_axis (Memr[x_val+1], Memr[kvec], flo, ncols + 1)
		call srf_label_axis (xcen, y1_perim-del, flo, "X-AXIS", 1, 2)
		call srf_draw_ticksx (Memr[x_val+1], y1_perim, y1_perim-delta, 
		    flo, ncols)
		call srf_label_axis (xmin, y1_perim-del, flo, "1", 1, 2)
		junk = itoc (int (wc2), tlabel, SZ_TLABEL)
		call srf_label_axis (Memr[x_val+ncols], y1_perim-del, flo, 
		    tlabel, 1, 2)

		# Now draw y axis
		call amovkr (x2_perim, Memr[kvec], nlines + 2)
		call srf_draw_axis (Memr[kvec], Memr[y_val], flo, nlines + 1)
		call srf_label_axis (x2_perim+del, ycen, flo, "Y-AXIS", 2, -1)
		call srf_draw_ticksy (x2_perim, x2_perim+delta, Memr[y_val+1],
		    flo, nlines)
		call srf_label_axis (x2_perim+del, ymin, flo, "1", 2, -1)
		junk = itoc (int (wl2), tlabel, SZ_TLABEL)
		call srf_label_axis (x2_perim+del, Memr[y_val+nlines], flo, 
		    tlabel, 2, -1)
	    } else {
		# Case 4: xy rotation negative, looking up from below mid Z
		# First draw x axis
		call amovkr (y2_perim, Memr[kvec], ncols + 2)
		call srf_draw_axis (Memr[x_val], Memr[kvec], flo, ncols + 1)
		call srf_label_axis (xcen, y2_perim+del, flo, "X-AXIS", 1, -2)
		call srf_draw_ticksx (Memr[x_val+1], y2_perim, y2_perim+delta,
		    flo, ncols)
		call srf_label_axis (xmin, y2_perim+del, flo, "1", 1, -2)
		junk = itoc (int (wc2), tlabel, SZ_TLABEL)
		call srf_label_axis (Memr[x_val+ncols], y2_perim+del, flo, 
		    tlabel, 1, -2)

		# Now draw y axis
		call amovkr (x1_perim, Memr[kvec], nlines + 2)
		call srf_draw_axis (Memr[kvec], Memr[y_val+1], flo, nlines + 1)
		call srf_label_axis (x1_perim-del, ycen, flo, "Y-AXIS", 2, 1)
		call srf_draw_ticksy (x1_perim, x1_perim-delta, Memr[y_val+1],
		    flo, nlines)
		call srf_label_axis (x1_perim-del, ymin, flo, "1", 2, 1)
		junk = itoc (int (wl2), tlabel, SZ_TLABEL)
		call srf_label_axis (x1_perim-del, Memr[y_val+nlines], flo, 
		    tlabel, 2, 1)
	    }
	}

	# Flush plotit buffer before returning
	call plotit (0, 0, 2)
	call sfree (sp)
end


procedure srf_draw_axis (xvals, yvals, zval, nvals)

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


procedure srf_label_axis (xval, yval, zval, sppstr, path, up)

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


procedure srf_draw_ticksx (x, y1, y2, zval, nvals)

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


procedure srf_draw_ticksy (x1, x2, y, zval, nvals)

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
