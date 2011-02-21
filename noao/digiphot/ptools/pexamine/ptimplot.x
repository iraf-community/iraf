include <imhdr.h>
include <gset.h>
include <math.h>
include	<mach.h>


# PT_RPLOT -- Plot the radial profile.

procedure pt_rplot (gd, im, wx, wy)

pointer	gd		# pointer to the graphics stream
pointer	im		# pointer to the input image
real	wx, wy		# radial profile coordinates

int	marker_type, lenbuf, npix
pointer	sp, radius, intensity, marker, longtitle
real	szmarker, rin, rout, x1, x2, y1, y2, dmin, dmax
bool	clgetb()
int	clgeti(), strlen(), pt_radpix()
real	clgetr()

begin
	# Check for undefined graphics stream.
	if (gd == NULL) {
	    call printf ("The graphics device is undefined\n")
	    return
	}

	# Check for undefined input image.
	if (im == NULL) {
	    call printf ("The graphics device is undefined\n")
	    return
	}

	# Check for an undefined center.
	if (IS_INDEFR(wx) || IS_INDEFR(wy)) {
	    call printf ("The radial profile plot center is undefined.\n")
	    return
	}

	# Get the inner and outer radii.
	rin = clgetr ("radplot.rinner")
	rout = clgetr ("radplot.router")
	if (rout <= rin) {
	    call printf (
	   "The outer radius %g is <= the inner radius %g\n")
		call pargr (rin)
		call pargr (rout)
	    return
	}

	# Allocate working space.
	call smark (sp)

	# Get the data.
	lenbuf = PI * (rin + rout + 1.0) * (rout - rin + 0.5)
	call salloc (radius, lenbuf, TY_REAL)
	call salloc (intensity, lenbuf, TY_REAL)
	npix = pt_radpix (im, wx, wy, rin, rout, Memr[radius], Memr[intensity])
	if (npix <= 0) {
	    call printf ("The object at %g %g is off the image\n")
		call pargr (wx)
		call pargr (wy)
	    call sfree (sp)
	    return
	}

	# Clear the plotting strucuture.
	call gclear (gd)

	# Fetch the window and viewport parameters.
	x1 = clgetr ("radplot.x1")
	x2 = clgetr ("radplot.x2")
	y1 = clgetr ("radplot.y1")
	y2 = clgetr ("radplot.y2")
	if (IS_INDEFR(x1) || IS_INDEFR(x2)) {
	    call pt_alimr (Memr[radius], npix, dmin, dmax)
	    if (IS_INDEFR(x1))
		x1 = dmin
	    if (IS_INDEFR(x2))
		x2 = dmax
	}
	if (IS_INDEFR(y1) || IS_INDEFR(y2)) {
	    call pt_alimr (Memr[intensity], npix, dmin, dmax)
	    if (IS_INDEFR(y1))
		y1 = dmin
	    if (IS_INDEFR(y2))
		y2 = dmax
	}

	# Set the scale of the axes.
	call gswind (gd, x1, x2, y1, y2)
	if (clgetb ("radplot.logx"))
	    call gseti (gd, G_XTRAN, GW_LOG)
	else
	    call gseti (gd, G_XTRAN, GW_LINEAR)
	if (clgetb ("radplot.logy"))
	    call gseti (gd, G_YTRAN, GW_LOG)
	else
	    call gseti (gd, G_YTRAN, GW_LINEAR)

	# Get the x and y axes parameters.
	if (! clgetb ("radplot.fill"))
	    call gseti (gd, G_ASPECT, 1)
	if (clgetb ("radplot.round"))
	    call gseti (gd, G_ROUND, YES)

	# Get the axis drawing parameters. 
	if (clgetb ("radplot.box")) {

	    # Get number of major and minor tick marks.
	    call gseti (gd, G_XNMAJOR, clgeti ("radplot.majrx"))
	    call gseti (gd, G_XNMINOR, clgeti ("radplot.minrx"))
	    call gseti (gd, G_YNMAJOR, clgeti ("radplot.majry"))
	    call gseti (gd, G_YNMINOR, clgeti ("radplot.minry"))

	    # Label tick marks on axes.
	    if (clgetb ("radplot.ticklabels"))
	        call gseti (gd, G_LABELTICKS, YES)
	    else
	        call gseti (gd, G_LABELTICKS, NO)

	    # Draw grid.
	    if (clgetb ("radplot.grid"))
	        call gseti (gd, G_DRAWGRID, YES)
	    else
	        call gseti (gd, G_DRAWGRID, NO)

	    # Optionally draw a box around the plot.
	    call salloc (longtitle, 2 * SZ_LINE, TY_CHAR)
	    if (clgetb ("radplot.banner")) {
		call sysid (Memc[longtitle], 2 * SZ_LINE)
	        call sprintf (Memc[longtitle+strlen(Memc[longtitle])],
		    2 * SZ_LINE, "\n%s: xc=%g  yc=%g  rinner=%g  router=%g\n%s")
		    call pargstr (IM_HDRFILE(im))
		    call pargr (wx)
		    call pargr (wy)
		    call pargr (rin)
		    call pargr (rout)
		    call pargstr ("Radial Profile Plot")
	    } else {
	        call sprintf (Memc[longtitle], 2 * SZ_LINE,
	            "%s: xc=%g  yc=%g  rinner=%g  router=%g\n%s")
		    call pargstr (IM_HDRFILE(im))
		    call pargr (wx)
		    call pargr (wy)
		    call pargr (rin)
		    call pargr (rout)
		    call pargstr ("Radial Profile Plot")
	    }
	    call glabax (gd, Memc[longtitle], "Radial distance (pixels)",
	        "Intensity (counts)")
	}

	# Get the marker type, the size of the marker and the linewidth.
	call salloc (marker, SZ_FNAME, TY_CHAR)
	call clgstr ("radplot.marker", Memc[marker], SZ_FNAME)
	call pt_marker (Memc[marker], SZ_FNAME, marker_type)
	if (marker_type != GM_POINT)
	    szmarker = clgetr ("radplot.szmarker")
	else
	    szmarker = 0.0
	call gsetr (gd, G_PLWIDTH, 2.0)

	# Draw the points in using the deletions array.
	call gpmark (gd, Memr[radius], Memr[intensity], npix, marker_type,
	    szmarker, szmarker)
	call gflush (gd)

	call sfree (sp)
end


define	CSIZE		24

# PT_SPLOT -- Draw a perspective view of a surface.  The altitude
# and azimuth of the viewing angle are variable.
 
procedure pt_splot (gd, im, x, y)
 
pointer	gd		# pointer to the graphics stream
pointer	im		# pointer to the image descriptor
real	x, y		# the object center

int	nx, ny, x1, x2, y1, y2, npts, wkid
pointer	data, sp, sdata, work, longtitle
real	floor, ceiling, angv, angh
bool	clgetb()
int 	clgeti(), strlen()
pointer	pt_gdata()
real	clgetr()

int	first
real	vpx1, vpx2, vpy1, vpy2
common  /frstfg/ first
common  /noaovp/ vpx1, vpx2, vpy1, vpy2
 
begin
	# Check for undefined graphics stream.
	if (gd == NULL) {
	    call printf ("The graphics device is undefined\n")
	    return
	}

	# Check for undefined input image.
	if (im == NULL) {
	    call printf ("The graphics device is undefined\n")
	    return
	}

	# Check for an undefined center.
	if (IS_INDEFR(x) || IS_INDEFR(y)) {
	    call printf ("The surface plot center is undefined\n")
	    return
	}

	# Get the data.
	ny = clgeti ("surfplot.nlines")
	nx = clgeti ("surfplot.ncolumns")
	x1 = x - (nx - 1) / 2 + 0.5
	x2 = x + nx / 2 + 0.5
	y1 = y - (ny - 1) / 2 + 0.5
	y2 = y + ny / 2 + 0.5
	data = pt_gdata (im, x1, x2, y1, y2)
	if (data == NULL) {
	    call printf ("The requested image section if off the image\n")
	    return
	}

	call smark (sp)

	# Set the title.
	call salloc (longtitle, 2 * SZ_LINE, TY_CHAR)
	if (clgetb ("surfplot.banner")) {
	    Memc[longtitle] = '\n'
	    call sysid (Memc[longtitle+1], 2 * SZ_LINE)
	    call sprintf (Memc[longtitle+strlen(Memc[longtitle])], 2 * SZ_LINE,
	        "\nObject at x: %g  y: %g\nSurface plot of %s[%d:%d,%d:%d]")
	        call pargr (x)
	        call pargr (y)
	        call pargstr (IM_HDRFILE(im))
	        call pargi (x1)
	        call pargi (x2)
	        call pargi (y1)
	        call pargi (y2)
	} else {
	    call sprintf (Memc[longtitle], 2 * SZ_LINE,
	        "\nObject at x: %g  y: %g\nSurface plot of %s[%d:%d,%d:%d]")
	        call pargr (x)
	        call pargr (y)
	        call pargstr (IM_HDRFILE(im))
	        call pargi (x1)
	        call pargi (x2)
	        call pargi (y1)
	        call pargi (y2)
	}

	# Initialize the plot.
	call gclear (gd)

	# Set the viewport, turn off axes drawing.
	call gsview (gd, 0.1, 0.9, 0.1, 0.9)
	call gseti (gd, G_DRAWAXES, NO)
	call glabax (gd, Memc[longtitle], "", "")

	nx = x2 - x1 + 1
	ny = y2 - y1 + 1
	npts = nx * ny

	# Take floor and ceiling if enabled (nonzero).
	floor = clgetr ("surfplot.floor")
	ceiling = clgetr ("surfplot.ceiling")
	if (IS_INDEFR (floor) && IS_INDEFR (ceiling))
	    sdata = data
	else {
	    call salloc (sdata, npts, TY_REAL)
	    call amovr (Memr[data], Memr[sdata], npts)
	    if (! IS_INDEFR (floor) && ! IS_INDEFR (ceiling)) {
		floor = min (floor, ceiling)
		ceiling = max (floor, ceiling)
	    }
	    if (! IS_INDEFR (floor))
		call amaxkr (Memr[sdata], floor, Memr[sdata], npts)
	    if (! IS_INDEFR (ceiling))
		call aminkr (Memr[sdata], ceiling, Memr[sdata], npts)
	}

	# Open graphics device and make plot.
	call gopks (STDERR)
	wkid = 1
	call gopwk (wkid, 6, gd)
	call gacwk (wkid)

	first = 1
	call srfabd()
	call ggview (gd, vpx1, vpx2, vpy1, vpy2)
	call set (vpx1, vpx2, vpy1, vpy2, 1.0, 1024., 1.0, 1024., 1)

	angh = clgetr ("surfplot.angh")
	angv = clgetr ("surfplot.angv")
	call salloc (work, 2 * (2*nx*ny+nx+ny), TY_REAL)
	call ezsrfc (Memr[sdata], nx, ny, angh, angv, Memr[work])

	if (clgetb ("surfplot.axes")) {
	    call gswind (gd, real (x1), real (x2), real (y1), real (y2))
	    call gseti (gd, G_CLIP, NO)
	    call pt_perimeter (gd, Memr[sdata], nx, ny, angh, angv)
	}

	call gdawk (wkid)
	call gclks ()
	call sfree (sp)
end


# PT_CPLOT -- Contour map
# This is an interface to the NCAR CONREC routine.

procedure pt_cplot (gd, im, x, y)
 
pointer	gd		# pointer to the graphics stream
pointer	im		# pointer to the input image
real	x, y		# center of the contour plot

int	nx, ny, x1, x2, y1, y2, nhi, dashpat, npts, ncontours, wkid, nset
pointer	data, sp, longtitle, data1
real	xs, xe, ys, ye, vx1, vx2, vy1, vy2
real	zero, floor, ceiling, zmin, zmax, interval, finc
bool	clgetb(), fp_equalr()
int	clgeti(), btoi(), strlen()
pointer	pt_gdata()
real	clgetr()

int	isizel, isizem, isizep, nrep, ncrt, ilab, nulbll, ioffd
int	ioffm, isolid, nla, nlm, first
real	xlt, ybt, side, ext, hold[5]
common  /conflg/ first
common  /conre4/ isizel, isizem , isizep, nrep, ncrt, ilab, nulbll, 
         ioffd, ext, ioffm, isolid, nla, nlm, xlt, ybt, side
common  /noaolb/ hold

begin
	# Check for undefined graphics stream.
	if (gd == NULL) {
	    call printf ("The graphics device is undefined\n")
	    return
	}

	# Check for undefined input image.
	if (im == NULL) {
	    call printf ("The graphics device is undefined\n")
	    return
	}

	# Check fo an undefined center.
	if (IS_INDEFR(x) || IS_INDEFR(y)) {
	    call printf  ("The center of the contour plot is undefined\n")
	    return
	}

	# Get the data.
	ny = clgeti ("cntrplot.nlines")
	nx = clgeti ("cntrplot.ncolumns")
	x1 = x - (nx - 1) / 2 + 0.5
	x2 = x + nx / 2 + 0.5
	y1 = y - (ny - 1) / 2 + 0.5
	y2 = y + ny / 2 + 0.5
	data = pt_gdata (im, x1, x2, y1, y2)
	if (data == NULL) {
	    call printf ("The image section to be contoured is off the image\n")
	    return
	}

	call smark (sp)

	# Intialize the plot
	call gclear (gd)

	# Set the WCS.
	xs = x1
	xe = x2
	ys = y1
	ye = y2
	call gswind (gd, xs, xe, ys, ye)

	nx = x2 - x1 + 1
	ny = y2 - y1 + 1
	if (! clgetb ("cntrplot.fill"))
	    call gsetr (gd, G_ASPECT, real (ny-1) / real (nx-1))
	call gseti (gd, G_ROUND, btoi (clgetb ("cntrplot.round")))

	if (clgetb ("cntrplot.box")) {

	    # Get number of major and minor tick marks.
	    call gseti (gd, G_XNMAJOR, clgeti ("cntrplot.majrx"))
	    call gseti (gd, G_XNMINOR, clgeti ("cntrplot.minrx"))
	    call gseti (gd, G_YNMAJOR, clgeti ("cntrplot.majry"))
	    call gseti (gd, G_YNMINOR, clgeti ("cntrplot.minry"))

	    # Label tick marks on axes ?
	    call gseti (gd, G_LABELTICKS, btoi (clgetb ("cntrplot.ticklabels")))

	    # Construct the title.
	    call salloc (longtitle, 2 * SZ_LINE, TY_CHAR)
	    if (clgetb ("cntrplot.banner")) {
		call sysid (Memc[longtitle], 2 * SZ_LINE)
	        call sprintf (Memc[longtitle+strlen(Memc[longtitle])],
		2 * SZ_LINE,
	     "\nObject at x: %g  y: %g\n\nContour plot of %s[%d:%d,%d:%d]\n")
		    call pargr (x)
		    call pargr (y)
		    call pargstr (IM_HDRFILE(im))
		    call pargi (x1)
		    call pargi (x2)
		    call pargi (y1)
		    call pargi (y2)
	    } else {
	        call sprintf (Memc[longtitle], 2 * SZ_LINE,
		"\nObject at x: %g  y: %g\n\nContour plot of %s[%d:%d,%d:%d]\n")
		    call pargr (x)
		    call pargr (y)
		    call pargstr (IM_HDRFILE(im))
		    call pargi (x1)
		    call pargi (x2)
		    call pargi (y1)
		    call pargi (y2)
	    }

	    call glabax (gd, Memc[longtitle], "", "")
	}

	# First of all, intialize conrec's block data before altering any
	# parameters in common.
	first = 1
	call conbd

	# Set the contouring parameters.
	zero = clgetr ("cntrplot.zero")
	floor = clgetr ("cntrplot.floor")
	ceiling	= clgetr ("cntrplot.ceiling")
	nhi = clgeti ("cntrplot.nhi")
	dashpat	= clgeti ("cntrplot.dashpat")

	# Resolve INDEF limits.
	npts = nx * ny
	if (IS_INDEFR (floor) || IS_INDEFR (ceiling)) {
	    call alimr (Memr[data], npts, zmin, zmax)
	    if (IS_INDEFR (floor))
	        floor = zmin
	    if (IS_INDEFR (ceiling))
	        ceiling = zmax
	}

	# Apply the zero point shift.
	if (abs (zero) > EPSILON) {
	    call salloc (data1, npts, TY_REAL)
	    call asubkr (Memr[data], zero, Memr[data1], npts)
	    floor = floor - zero
	    ceiling = ceiling - zero
	} else
	    data1 = data

	# Avoid conrec's automatic scaling.
	if (fp_equalr (floor, 0.0))
	    floor = EPSILON
	if (fp_equalr (ceiling, 0.0))
	    ceiling = EPSILON

	# The user can suppress the contour labelling by setting the common
	# parameter "ilab" to zero.
	if (btoi (clgetb ("cntrplot.label")) == NO)
	    ilab = 0
	else
	    ilab = 1

	# User can specify either the number of contours or the contour
	# interval, or let conrec pick a nice number.  Get params and
	# encode the FINC param expected by conrec.

	ncontours = clgeti ("cntrplot.ncontours")
	if (ncontours <= 0) {
	    interval = clgetr ("cntrplot.interval")
	    if (interval <= 0.0)
		finc = 0
	    else
		finc = interval
	} else
	    finc = - abs (ncontours)

	# Open device and make contour plot.
	call gopks (STDERR)
	wkid = 1
	call gopwk (wkid, 6, gd)
	call gacwk (wkid)

	# Make the contour plot.
	nset = 1	# No conrec viewport
	ioffm = 1	# No conrec box
	call gswind (gd, 1., real (nx), 1., real (ny))
	call ggview (gd, vx1, vx2, vy1, vy2)
	call set (vx1, vx2, vy1, vy2, 1.0, real (nx), 1.0, real (ny), 1)
	call conrec (Memr[data1], nx, nx, ny, floor, ceiling, finc, nset,
	    nhi, -dashpat)

	call gdawk (wkid)
	call gclks ()

	call gswind (gd, xs, xe, ys, ye)
	if (fp_equalr (hold[5], 1.0)) {
	    call sprintf (Memc[longtitle], 2 * SZ_LINE, 
                "\n\nContoured from %g to %g, interval = %g\n\n")
	        call pargr (hold[1])
	        call pargr (hold[2])
	        call pargr (hold[3])
	} else {
	    call sprintf (Memc[longtitle], 2 * SZ_LINE,
        "\n\nContoured from %g to %g, interval = %g, labels scaled by %g\n\n")
	            call pargr (hold[1])
	            call pargr (hold[2])
	            call pargr (hold[3])
		    call pargr (hold[5])
	}

	call gseti (gd, G_DRAWAXES, NO)
	call glabax (gd, Memc[longtitle], "", "")

	call sfree (sp)
end


# PT_PERIMETER -- Draw and label axes around the surface plot.

procedure pt_perimeter (gd, z, ncols, nlines, angh, angv)

pointer	gd			# graphics pointer
int	ncols			# number of image columns
int	nlines			# number of image lines
real	z[ncols, nlines]	# array of intensity values
real	angh			# angle of horizontal inclination
real	angv			# angle of vertical inclination

char	tlabel[10]
int	i, j
pointer	sp, x_val, y_val, kvec
real	xmin, ymin, delta, fact1, flo, hi, xcen, ycen
real	x1_perim, x2_perim, y1_perim, y2_perim, z1, z2
real	wc1, wc2, wl1, wl2, del
int	itoc()

data  	fact1 /2.0/
real	vpx1, vpx2, vpy1, vpy2
common	/noaovp/ vpx1, vpx2, vpy1, vpy2

begin
	call smark (sp)
	call salloc (x_val, ncols + 2, TY_REAL)
	call salloc (y_val, nlines + 2, TY_REAL)
	call salloc (kvec, max (ncols, nlines) + 2, TY_REAL)

	# Get window coordinates set up calling procedure.
	call ggwind (gd, wc1, wc2, wl1, wl2)

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
        delta = (hi-flo) / (max (ncols, nlines) -1.) * fact1
        xmin = -(real (ncols/2)  * delta + real (mod (ncols+1, 2))  * delta)
        ymin = -(real (nlines/2) * delta + real (mod (nlines+1, 2)) * delta)
	del = 2.0 * delta

	# The perimeter is separated from the surface plot by the 
	# width of delta.  
	x1_perim = xmin - delta
	y1_perim = ymin - delta
	x2_perim = xmin + (real (ncols)  * delta)
	y2_perim = ymin + (real (nlines) * delta)

	# Set up linear arrays over full perimeter range.
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

	# Calculate positions for the axis labels.
	xcen = 0.5 * (x1_perim + x2_perim)
	ycen = 0.5 * (y1_perim + y2_perim)

	if (angh >= 0.0) {

	    # Case 1: xy rotation positive, looking down from above mid z.
	    if (angv >= 0.0) {

		# First draw x axis.
		call amovkr (y2_perim, Memr[kvec], ncols + 2)
		call pt_draw_axis (Memr[x_val+1], Memr[kvec], flo, ncols + 1)
		call pt_label_axis (xcen, y2_perim+del, flo, "X-AXIS", -1, -2)
		call pt_draw_ticksx (Memr[x_val+1], y2_perim, y2_perim+delta, 
		    flo, ncols)
		if (itoc (int (wc1), tlabel, 10) <= 0)
		    tlabel[1] = EOS
		call pt_label_axis (xmin, y2_perim+del, flo, tlabel, -1, -2)
		if (itoc (int (wc2), tlabel, 10) <= 0)
		    tlabel[1] = EOS
		call pt_label_axis (Memr[x_val+ncols], y2_perim+del, flo, 
		    tlabel, -1, -2)

		# Now draw y axis.
		call amovkr (x2_perim, Memr[kvec], nlines + 2)
		call pt_draw_axis (Memr[kvec], Memr[y_val+1], flo, nlines + 1)
		call pt_label_axis (x2_perim+del, ycen, flo, "Y-AXIS", 2, -1)
		call pt_draw_ticksy (x2_perim, x2_perim+delta, Memr[y_val+1],
		    flo, nlines)
		if (itoc (int (wl1), tlabel, 10) <= 0)
		    tlabel[1] = EOS
		call pt_label_axis (x2_perim+del, ymin, flo, tlabel, 2, -1)
		if (itoc (int (wl2), tlabel, 10) <= 0)
		    tlabel[1] = EOS
		call pt_label_axis (x2_perim+del, Memr[y_val+nlines], flo, 
		    tlabel, 2, -1)

	    # Case 2: xy rotation positive, looking up from below mid z.
	    } else {

		# First draw x axis.
		call amovkr (y1_perim, Memr[kvec], ncols + 2)
		call pt_draw_axis (Memr[x_val], Memr[kvec], flo, ncols + 1)
		call pt_label_axis (xcen, y1_perim-del, flo, "X-AXIS", -1, 2)
		call pt_draw_ticksx (Memr[x_val+1], y1_perim, y1_perim-delta, 
		    flo, ncols)
		if (itoc (int (wc1), tlabel, 10) <= 0)
		    tlabel[1] = EOS
		call pt_label_axis (xmin, y1_perim-del, flo, tlabel, -1, 2)
		if (itoc (int (wc2), tlabel, 10) <= 0)
		    tlabel[1] = EOS
		call pt_label_axis (Memr[x_val+ncols], y1_perim-del, flo, 
		    tlabel, -1, 2)

		# Now draw y axis.
		call amovkr (x1_perim, Memr[kvec], nlines + 2)
		call pt_draw_axis (Memr[kvec], Memr[y_val], flo, nlines + 1)
		call pt_label_axis (x1_perim-del, ycen, flo, "Y-AXIS", 2, 1)
		call pt_draw_ticksy (x1_perim, x1_perim-delta, Memr[y_val+1],
		    flo, nlines)
		if (itoc (int (wl1), tlabel, 10) <= 0)
		    tlabel[1] = EOS
		call pt_label_axis (x1_perim-del, ymin, flo, tlabel, 2, 1)
		if (itoc (int (wl2), tlabel, 10) <= 0)
		    tlabel[1] = EOS
		call pt_label_axis (x1_perim-del, Memr[y_val+nlines], flo, 
		    tlabel, 2, 1)
	    }
	}

	if (angh < 0.0) {

	    # Case 3: xy rotation negative, looking down from above  mid z 
	    # (default).
	    if (angv > 0.0) {

		# First draw x axis.
		call amovkr (y1_perim, Memr[kvec], ncols + 2)
		call pt_draw_axis (Memr[x_val+1], Memr[kvec], flo, ncols + 1)
		call pt_label_axis (xcen, y1_perim-del, flo, "X-AXIS", 1, 2)
		call pt_draw_ticksx (Memr[x_val+1], y1_perim, y1_perim-delta, 
		    flo, ncols)
		if (itoc (int (wc1), tlabel, 10) <= 0)
		    tlabel[1] = EOS
		call pt_label_axis (xmin, y1_perim-del, flo, tlabel, 1, 2)
		if (itoc (int (wc2), tlabel, 10) <= 0)
		    tlabel[1] = EOS
		call pt_label_axis (Memr[x_val+ncols], y1_perim-del, flo, 
		    tlabel, 1, 2)

		# Now draw y axis.
		call amovkr (x2_perim, Memr[kvec], nlines + 2)
		call pt_draw_axis (Memr[kvec], Memr[y_val], flo, nlines + 1)
		call pt_label_axis (x2_perim+del, ycen, flo, "Y-AXIS", 2, -1)
		call pt_draw_ticksy (x2_perim, x2_perim+delta, Memr[y_val+1],
		    flo, nlines)
		if (itoc (int (wl1), tlabel, 10) <= 0)
		    tlabel[1] = EOS
		call pt_label_axis (x2_perim+del, ymin, flo, tlabel, 2, -1)
		if (itoc (int (wl2), tlabel, 10) <= 0)
		    tlabel[1] = EOS
		call pt_label_axis (x2_perim+del, Memr[y_val+nlines], flo, 
		    tlabel, 2, -1)

	    # Case 4: xy rotation negative, looking up from below mid Z.
	    } else {

		# First draw x axis.
		call amovkr (y2_perim, Memr[kvec], ncols + 2)
		call pt_draw_axis (Memr[x_val], Memr[kvec], flo, ncols + 1)
		call pt_label_axis (xcen, y2_perim+del, flo, "X-AXIS", 1, -2)
		call pt_draw_ticksx (Memr[x_val+1], y2_perim, y2_perim+delta,
		    flo, ncols)
		if (itoc (int (wc1), tlabel, 10) <= 0)
		    tlabel[1] = EOS
		call pt_label_axis (xmin, y2_perim+del, flo, tlabel, 1, -2)
		if (itoc (int (wc2), tlabel, 10) <= 0)
		    tlabel[1] = EOS
		call pt_label_axis (Memr[x_val+ncols], y2_perim+del, flo, 
		    tlabel, 1, -2)

		# Now draw y axis.
		call amovkr (x1_perim, Memr[kvec], nlines + 2)
		call pt_draw_axis (Memr[kvec], Memr[y_val+1], flo, nlines + 1)
		call pt_label_axis (x1_perim-del, ycen, flo, "Y-AXIS", 2, 1)
		call pt_draw_ticksy (x1_perim, x1_perim-delta, Memr[y_val+1],
		    flo, nlines)
		if (itoc (int (wl1), tlabel, 10) <= 0)
		    tlabel[1] = EOS
		call pt_label_axis (x1_perim-del, ymin, flo, tlabel, 2, 1)
		if (itoc (int (wl2), tlabel, 10) <= 0)
		    tlabel[1] = EOS
		call pt_label_axis (x1_perim-del, Memr[y_val+nlines], flo, 
		    tlabel, 2, 1)
	    }
	}

	# Flush plotit buffer before returning.
	call plotit (0, 0, 2)
	call sfree (sp)
end


# PT_DRAW_AXIS -- Draw the axes around the plot.

procedure pt_draw_axis (xvals, yvals, zval, nvals)

real	xvals[nvals]
real	yvals[nvals]
real	zval
int	nvals

int	i
pointer	sp, xt, yt
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


# PT_LABEL_AXIS -- Label the axes.

procedure pt_label_axis (xval, yval, zval, sppstr, path, up)

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


# PT_DRAW_TICKSX -- Draw the x tick marks.

procedure pt_draw_ticksx (x, y1, y2, zval, nvals)

real	x[nvals]
real	y1, y2
real	zval
int	nvals

int	i
real	tkx[2], tky[2], dum

begin
	do i = 1, nvals {
	    call trn32s (x[i], y1, zval, tkx[1], tky[1], dum, 1)
	    call trn32s (x[i], y2, zval, tkx[2], tky[2], dum, 1)
	    call gpl (2, tkx[1], tky[1])
	}
end


# PT_DRAW_TICKSY -- Draw the y tick marks.

procedure pt_draw_ticksy (x1, x2, y, zval, nvals)

real	x1, x2
real	y[nvals]
real	zval
int	nvals

int	i
real	tkx[2], tky[2], dum

begin
	do i = 1, nvals {
	    call trn32s (x1, y[i], zval, tkx[1], tky[1], dum, 1)
	    call trn32s (x2, y[i], zval, tkx[2], tky[2], dum, 1)
	    call gpl (2, tkx[1], tky[1])
	}
end

 
# PT_GDATA -- Get image data with boundary checking.
 
pointer procedure pt_gdata (im, x1, x2, y1, y2)
 
pointer	im			# pointer to the input image
int	x1, x2, y1, y2		# subraster limits both input and output
 
int	i, nc, nl
pointer	imgs2r()
errchk	imgs2r
 
begin
	nc = IM_LEN(im,1)
	nl = IM_LEN(im,2)
	if (IS_INDEFI (x1))
	    x1 = 1
	if (IS_INDEFI (x2))
	    x2 = nc
	if (IS_INDEFI (y1))
	    y1 = 1
	if (IS_INDEFI (y2))
	    y2 = nl
 
	i = max (x1, x2)
	x1 = min (x1, x2)
	x2 = i
	i = max (y1, y2)
	y1 = min (y1, y2)
	y2 = i

	if (x2 < 1 || x1 > nc || y2 < 1 || y1 > nl)
	    return (NULL)
	
	x1 = max (1, x1)
	x2 = min (nc, x2)
	y1 = max (1, y1)
	y2 = min (nl, y2)

	return (imgs2r (im, x1, x2, y1, y2))
end


# PT_RADPIX -- Procedure to fetch the image pixels in an annulus around
# a given center.

int procedure pt_radpix (im, wx, wy, rin, rout, rcoords, pix)

pointer	im		# pointer to IRAF image
real	wx, wy		# center of sky annulus
real	rin, rout	# inner and outer radius of sky annulus
real	rcoords[ARB]	# radial coordinate array
real	pix[ARB]	# pixel array

int	i, j, ncols, nlines, c1, c2, l1, l2, npix
pointer	buf
real	xc1, xc2, xl1, xl2, rin2, rout2, rj2, r2
pointer	imgs2r()

begin
	if (rout <= rin)
	    return (0)

	# Test for out of bounds sky regions.
	ncols = IM_LEN(im,1)
	nlines = IM_LEN(im,2)
	xc1 = wx - rout
	xc2 = wx + rout
	xl1 = wy - rout
	xl2 = wy + rout
	if (xc2 < 1.0 || xc1 > real (ncols) || xl2 < 1.0 || xl1 > real (nlines))
	    return (0)

	# Compute the column and line limits.
	c1 = max (1.0, min (real (ncols), wx - rout)) + 0.5
	c2 = min (real (ncols), max (1.0, wx + rout)) + 0.5
	l1 = max (1.0, min (real (nlines), wy - rout)) + 0.5
	l2 = min (real (nlines), max (1.0, wy + rout)) + 0.5

	# Fetch the sky pixels.
	rin2 = rin ** 2
	rout2 = rout ** 2
	npix = 0

	do j = l1, l2 {
	    buf = imgs2r (im, c1, c2, j, j)
	    rj2 = (wy - j) ** 2
	    do i = c1, c2 {
	        r2 = (wx - i) ** 2 + rj2
		if (r2 > rin2 && r2 <= rout2) {
		    rcoords[npix+1] = sqrt (r2)
		    pix[npix+1] = Memr[buf+i-c1]
		    npix = npix + 1
		}
	    }
	}

	return (npix)
end
