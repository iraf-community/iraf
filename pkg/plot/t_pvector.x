# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gset.h>
include	<mach.h>
include <math.h>
include	<imhdr.h>
include <imset.h>
include <math/iminterp.h>

define	BTYPES		"|constant|nearest|reflect|wrap|project|"
define	SZ_BTYPE	8	# Length of boundary type string
define	NLINES		16	# Number of image  lines in the buffer

# T_PVECTOR -- Plot the vector of image data between two pixels.

procedure t_pvector()

pointer	image, boundary, output, outtype
pointer	sp, im, x_vec, y_vec
int	wrt_image, wrt_text
int	btype, ndim, nxvals, nyvals, nzvals, width
real	xc, yc, x1, y1, x2, y2, theta, length, zmin, zmax, bconstant

bool	streq(), fp_equalr()
int	clgeti(), clgwrd(), nowhite()
pointer	immap()
real	clgetr()

begin
	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (boundary, SZ_BTYPE, TY_CHAR)
	call salloc (output, SZ_FNAME, TY_CHAR)
	call salloc (outtype, SZ_FNAME, TY_CHAR)

	# Get boundary extension parameters.
	btype  = clgwrd ("boundary", Memc[boundary], SZ_BTYPE, BTYPES)
	bconstant = clgetr ("constant")

	# Open the image.
	call clgstr ("image", Memc[image], SZ_FNAME)
	im = immap (Memc[image], READ_ONLY, 0)
	ndim = IM_NDIM(im)
	if (ndim > 2)
	    call error (0, "The number of image dimensions is greater then 2.")

	# See if we're going to output the vector
	call clgstr ("vec_output", Memc[output], SZ_FNAME)
	call clgstr ("out_type", Memc[outtype], SZ_FNAME)

	wrt_text = NO
	wrt_image = NO
	if (nowhite (Memc[output], Memc[output], SZ_FNAME) > 0) {
	    if (streq("image",Memc[outtype]))
		wrt_image = YES
	    else if (streq("text",Memc[outtype]))
		wrt_text = YES
	}

	# Store the maximum coordinate values in the parameter file.
	nxvals = IM_LEN(im,1)
	if (ndim == 1)
	    nyvals = 1
	else
	    nyvals = IM_LEN(im,2)
	call clputi ("x1.p_maximum", nxvals)
	call clputi ("x2.p_maximum", nxvals)
	call clputi ("y1.p_maximum", nyvals)
	call clputi ("y2.p_maximum", nyvals)

	# Get the beginning and ending coordinates and width of the strip.
	theta = clgetr ("theta")
	if (IS_INDEFR(theta)) {
	    x1 = clgetr ("x1")
	    y1 = clgetr ("y1")
	    x2 = clgetr ("x2")
	    y2 = clgetr ("y2")
	} else {
	    xc = clgetr ("xc")
	    yc = clgetr ("yc")
	    length = clgetr ("length")
	    call pv_get_bound (xc, yc, length, theta, nxvals, nyvals, x1, y1,
	        x2, y2)
	}
	width = clgeti ("width")

	# Check the boundary and compute the length of the output vector.
	x1 = max (1.0, min (x1, real (nxvals)))
	x2 = min (real(nxvals), max (1.0, x2))
	y1 = max (1.0, min (y1, real (nyvals)))
	y2 = min (real(nyvals), max (1.0, y2))
	nzvals = int (sqrt ((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1))) + 1

	# Check for cases which should be handled by pcols or prows.
	call malloc (x_vec, nzvals, TY_REAL)
	call malloc (y_vec, nzvals, TY_REAL)

	if (fp_equalr (x1, x2)) {
	    call pv_get_col (im, x1, y1, x2, y2, nzvals, width, btype,
	        bconstant, Memr[x_vec], Memr[y_vec], zmin, zmax)
	} else if (fp_equalr (y1, y2)) {
	    if (ndim == 1) {
		call pv_get_row1 (im, x1, x2, nzvals, btype, bconstant,
		    Memr[x_vec], Memr[y_vec], zmin, zmax)
	    } else {
	        call pv_get_row (im, x1, y1, x2, y2, nzvals, width, btype,
	            bconstant, Memr[x_vec], Memr[y_vec], zmin, zmax)
	    }
	} else {
	    call pv_get_vector (im, x1, y1, x2, y2, nzvals, width, btype,
	        bconstant, Memr[x_vec], Memr[y_vec], zmin, zmax)
	}

	# Output the plot, via the graphics stream, or as a textfile or image.
	if (wrt_image == YES) {
	    call pv_wrt_image (im, Memc[image], Memc[output],
		Memr[x_vec], Memr[y_vec], nzvals, x1, x2, y1, y2, width)
	} else if (wrt_text == YES) {
	    call pv_wrt_pixels (Memc[output],
		Memr[x_vec], Memr[y_vec], nzvals)
	} else {
	    call pv_draw_vector (Memr[x_vec], Memr[y_vec], nzvals,
		x1, x2, y1, y2, zmin, zmax, width, Memc[image])
	}

        # Free resources.
	call mfree (x_vec, TY_REAL)
	call mfree (y_vec, TY_REAL)
	call imunmap (im)
	call sfree (sp)
end


# PV_DRAW_VECTOR - Draw the vector to the specified output device.

procedure pv_draw_vector (xvec, yvec, nzvals,
	x1, x2, y1, y2, zmin, zmax, width, image)

real	xvec[nzvals], yvec[nzvals]			#I Vectors to draw
int	nzvals, width					#I Plot parameters
real	x1, x2, y1, y2, zmin, zmax			#I Plot parameters
char	image[SZ_FNAME]					#I Image name

pointer	sp, gp
int	mode, imark
pointer	device, marker, xlabel, ylabel, title, suffix, hostid
real	wx1, wx2, wy1, wy2, vx1, vx2, vy1, vy2, szm, tol
bool	pointmode

bool	clgetb(), streq()
int	clgeti(), btoi()
pointer	gopen()
real	clgetr()
errchk 	gopen

begin
	call smark (sp)
	call salloc (device, SZ_FNAME, TY_CHAR)
	call salloc (marker, SZ_FNAME, TY_CHAR)
	call salloc (xlabel, SZ_LINE, TY_CHAR)
	call salloc (ylabel, SZ_LINE, TY_CHAR)
	call salloc (hostid, 2 * SZ_LINE, TY_CHAR)
	call salloc (title, SZ_LINE, TY_CHAR)
	call salloc (suffix, SZ_FNAME, TY_CHAR)

	# Open the graphics stream.
	call clgstr ("device", Memc[device], SZ_FNAME)
	if (clgetb ("append"))
	    mode = APPEND
	else
	    mode = NEW_FILE
	iferr (gp = gopen (Memc[device], mode, STDGRAPH))
	    call error (0, "Error opening graphics device.")

	tol = 10. * EPSILONR

	if (mode != APPEND) {
	    # Establish window.
	    wx1 = clgetr ("wx1")
	    wx2 = clgetr ("wx2")
	    wy1 = clgetr ("wy1")
	    wy2 = clgetr ("wy2")

	    # Set window limits to defaults if not specified by user.
	    if (abs(wx2 - wx1) < tol) {
	        wx1 = 1.0
	        wx2 = real (nzvals)
	    }
	    if (abs(wy2 - wy1) < tol) {
	        wy1 = zmin
	        wy2 = zmax
	    }
	    call gswind (gp, wx1, wx2, wy1, wy2)
    
	    # Establish viewport.
	    vx1 = clgetr ("vx1")
	    vx2 = clgetr ("vx2")
	    vy1 = clgetr ("vy1")
	    vy2 = clgetr ("vy2")

	    # Set viewport only if specified by user.
	    if ((vx2 - vx1) > tol && (vy2 - vy1) > tol)
	        call gsview (gp, vx1, vx2, vy1, vy2)
	    else {
		if (!clgetb ("fill"))
		    call gseti (gp, G_ASPECT, 1)
	    }
    
	    call clgstr ("xlabel", Memc[xlabel], SZ_LINE)
	    call clgstr ("ylabel", Memc[ylabel], SZ_LINE)
	    call clgstr ("title",  Memc[title],  SZ_LINE)
	    call sysid (Memc[hostid], SZ_LINE)
	    call strcat ("\n", Memc[hostid], SZ_LINE)
	    if (streq (Memc[title], "imtitle")) {
	        call strcpy (image, Memc[title], SZ_LINE)
		call sprintf (Memc[suffix], SZ_FNAME, 
		    ": vector %.1f,%.1f to %.1f,%.1f width: %d") {
		    call pargr (x1)
		    call pargr (y1)
		    call pargr (x2)
		    call pargr (y2)
		    call pargi (width)
		}
	        call strcat (Memc[suffix], Memc[title], SZ_LINE)
	    }
	    call strcat (Memc[title], Memc[hostid], 2 * SZ_LINE)
    
	    call gseti (gp, G_XNMAJOR, clgeti ("majrx"))
	    call gseti (gp, G_XNMINOR, clgeti ("minrx"))
	    call gseti (gp, G_YNMAJOR, clgeti ("majry"))
	    call gseti (gp, G_YNMINOR, clgeti ("minry"))
	    call gseti (gp, G_ROUND, btoi (clgetb ("round")))

	    if (clgetb ("logx"))
	        call gseti (gp, G_XTRAN, GW_LOG)
	    if (clgetb ("logy"))
	        call gseti (gp, G_YTRAN, GW_LOG)

	    # Draw axes using all this information
	    call glabax (gp, Memc[hostid], Memc[xlabel], Memc[ylabel])
	}
    
	pointmode = clgetb ("pointmode")
        if (pointmode) {
            call clgstr ("marker", Memc[marker], SZ_FNAME)
            szm= clgetr ("szmarker")
            call init_marker (Memc[marker], imark)
        } else
	    call clgstr ("marker", Memc[marker], SZ_FNAME)

        # Now to actually draw the plot.
        if (pointmode)
            call gpmark (gp, x_vec, y_vec, nzvals, imark, szm, szm)
        else
            call hgpline (gp, x_vec, y_vec, nzvals, Memc[marker])
       
        # Close up graphics and image.
        call gclose (gp)
	call sfree (sp)
end


# PV_WRT_PIXELS - Write out the vector to the specified file.  File may be
# specified as STDOUT.  Behaves much like LISTPIX.

procedure pv_wrt_pixels (file, x, y, npts)

char	file[SZ_FNAME]				#I Output file name
real	x[npts], y[npts]			#I Vector to write
int	npts					#I Npts in vector

int	i
pointer	fd, open()
bool	streq()
errchk	open

begin
	if (streq("STDOUT", file))
	    fd = STDOUT
	else if (streq("STDERR", file))
	    fd = STDERR
	else
	    iferr (fd = open (file, APPEND, TEXT_FILE))
		call error (0, "Error opening output file.")

	do i = 1, npts {
	    call fprintf (fd, "%.1f  %.4f\n")
		call pargr (x[i])
		call pargr (y[i])
	}

	call flush (fd)
	if (fd != STDOUT && fd != STDERR)
	    call close (fd)
end


# PV_WRT_IMAGE - Write out the vector to the specified image name.  The original
# image header is coptired to the new image and a acomment added describing the
# computed vector

procedure pv_wrt_image (im, image, file, x, y, npts, x1, x2, y1, y2, width)

pointer	im					#I Parent image pointer
char	image[SZ_FNAME]				#I Name of original image
char	file[SZ_FNAME]				#I Ouput image name
real	x[npts], y[npts]			#I Vector to write
int	npts					#I Npts in vector
real	x1, x2, y1, y2				#I Endpoints of vector
int	width					#I Width of sampled points

pointer	sp, comment, imo
pointer	immap(), impl2r()
bool	streq()
errchk	immap, impl2r

begin
	if (streq(file,"STDOUT") || streq(file,"STDERR"))
	    call error (0, "Illegal filename for output image.")

	# Open a (new) image
	iferr (imo = immap(file, NEW_COPY, im)) 
	    call error (0, "Error opening output image.")

	call smark (sp)
	call salloc (comment, SZ_LINE, TY_CHAR)

	# Do some header manipulations
	IM_NDIM(imo) = 1
	IM_LEN(imo,1) = npts
	call sprintf (Memc[comment], SZ_LINE,
	    "%s: vector %.1f,%.1f to %.1f,%.1f  width: %d")
		call pargstr (image)
		call pargr (x1)
		call pargr (x2)
		call pargr (y1)
		call pargr (y2)
		call pargi (width)
	call imastr (imo, "VSLICE", Memc[comment])

	# Now dump it into the image
	call amovr (y, Memr[impl2r(imo,1)], npts)

	# Do some housecleaning
	call imunmap (imo)
	call sfree (sp)
end


# PV_GET_BOUND -- Find the point where a vector, defined by it's starting
# point and an theta (ccw from +x), intersects the image boundary. The
# image is defined from 1 - nxvals; 1 - nyvals.

procedure pv_get_bound (xc, yc, length, theta, nxvals, nyvals, x1, y1, x2, y2)

real	xc, yc			# x and y center points
real	length			# length of the vector
real	theta			# angle of vector (ccw from +x)
int	nxvals, nyvals		# image dimensions
real	x1, y1			# starting point of vector
real	x2, y2			# point where vector intersects boundary

real	half_length, angle, dx, dy

begin
	if (IS_INDEFR(length))
	    half_length = sqrt (real (nxvals ** 2 + nyvals ** 2)) / 2.0
	else
	    half_length = length / 2.0
	dx = cos (DEGTORAD (theta))
	dy = sin (DEGTORAD (theta))

	# Compute the coordinates of the end of the vector
	x1 = xc - dx * half_length
	y1 = yc - dy * half_length
	x2 = xc + dx * half_length
	y2 = yc + dy * half_length

	if (x2 < 1.0 || x2 > nxvals || y2 < 1.0 || y2 > nyvals)
	    call pv_limits (xc, yc, theta, nxvals, nyvals, x2, y2)

	angle = theta + 180.0
	if (angle > 360.0)
	    angle = angle - 360.0
	if (x1 < 1.0 || x1 > nxvals || y1 < 1.0 || y1 > nyvals)
	    call pv_limits (xc, yc, angle, nxvals, nyvals, x1, y1)

end


# PV_LIMITS -- Find the point where a vector, defined by it's starting
# point and an theta (ccw from +x), intersects the image boundary. The
# image is defined from 1 - nxvals; 1 - nyvals.

procedure pv_limits (x1, y1, theta, nxvals, nyvals, x2, y2)

real	x1, y1			# starting point of vector
real	theta			# angle of vector (ccw from +x)
int	nxvals, nyvals		# size of image
real	x2, y2			# point where vector intersects boundary

real	tan_theta, xx
bool	fp_equalr()

begin
	tan_theta = tan (DEGTORAD (theta))

	if (fp_equalr (theta, 0.0)) {
	    x2 = nxvals
	    y2 = y1
	} else if (fp_equalr (theta, 90.0)) {
	    x2 = x1
	    y2 = nyvals
	} else if (fp_equalr (theta, 180.0)) {
	    x2 = 1
	    y2 = y1
	} else if (fp_equalr (theta, 270.0)) {
	    x2 = x1
	    y2 = 1
	} else if (fp_equalr (theta, 360.0)) {
	    x2 = nxvals
	    y2 = y1

	# Assume it intersects y = nyvals boundary.
	} else if (theta > 0.0 && theta < 180.0) {

    	    xx = (nyvals - y1) / tan_theta + x1
	    if (xx > nxvals || xx < 1.0) {
	        if (theta < 90.)
	    	    x2 = nxvals
		else
		    x2 = 1.0
		y2 = y1 + (x2 - x1) * tan_theta
	    } else {
		y2 = nyvals
		x2 = (y2 - y1) / tan_theta + x1
	    }

	# Assume it intersects y = 1.0 boundary.
	} else if (theta > 180.0 && theta < 360.0) {

	    xx = (1.0 - y1) / tan_theta + x1
	    if (xx > nxvals || xx < 1.0) {
	        if (theta < 270.)
		    x2 = 1.0
		else
		    x2 = nxvals
		y2 = y1 + (x2 - x1) * tan_theta
	    } else {
		y2 = 1.0
	        x2 = (y2 - y1) / tan_theta + x1
	    }
	}
end


# PV_GET_VECTOR -- Average a strip perpendicular to a given vector and return
# vectors of point number and average pixel value. Also returned is the min
# and max value in the data vector.

procedure pv_get_vector (im, x1, y1, x2, y2, nvals, width, btype,
        bconstant, x_vector, y_vector, zmin, zmax)

pointer im		# pointer to image header
real	x1, y1		# starting pixel of vector
real	x2, y2		# ending pixel of pixel
real	bconstant	# Boundary extension constant
int	btype		# Boundary extension type
int	nvals		# number of samples along the vector
int	width		# width of strip to average over
real	x_vector[ARB]	# Pixel numbers
real	y_vector[ARB]	# Average pixel values (returned)
real	zmin, zmax 	# min, max of data vector

double	dx, dy, dpx, dpy, ratio, xoff, yoff, noff, xv, yv
int	i, j, k, nedge, col1, col2, line1, line2
int	colb, colc, line, linea, lineb, linec
pointer sp, oxs, oys, xs, ys, yvals, msi, buf
real	sum , lim1, lim2, lim3, lim4
pointer	imgs2r()

begin
	call smark (sp)
	call salloc (oxs, width, TY_REAL)
	call salloc (oys, width, TY_REAL)
	call salloc (xs, width, TY_REAL)
	call salloc (ys, width, TY_REAL)
	call salloc (yvals, width, TY_REAL)

	# Determine sampling perpendicular to vector.
	dx = (x2 - x1) / (nvals - 1)
	dy = (y2 - y1) / (nvals - 1)
	if (x1 < x2) {
	    dpx = -dy
	    dpy =  dx
	} else {
	    dpx =  dy
	    dpy = -dx
	}

	# Compute offset from the nominal vector to the first sample point.
	ratio = dx / dy
	nedge  = width + 1
	noff = (real (width) - 1.0) / 2.0
	xoff = noff * dpx
	yoff = noff * dpy

	# Initialize the interpolator and the image data buffer.
	call msiinit (msi, II_BILINEAR]
	buf = NULL

	# Set the boundary.
	col1 = int (min (x1, x2)) - nedge
	col2 = nint (max (x1, x2)) + nedge
	line1 = int (min (y1, y2)) - nedge
	line2 = nint (max (y2, y1)) + nedge
	call pv_setboundary (im, col1, col2, line1, line2, btype, bconstant)

	# Initialize.
	xv = x1 - xoff
	yv = y1 - yoff
	do j = 1, width { 
	    Memr[oxs+j-1] = double (j - 1) * dpx
	    Memr[oys+j-1] = double (j - 1) * dpy
	} 

	# Loop over the output image lines.
	do i = 1, nvals { 
	    x_vector[i] = real (i)
	    line = yv

	    # Get the input image data and fit an interpolator to the data.
	    # The input data is buffered in a section of size NLINES + 2 *
	    # NEDGE.

	    if (dy >= 0.0 && (buf == NULL || line > linea)) {
		linea = min (line2, line + NLINES - 1)
		lineb = max (line1, line - nedge)
		linec = min (line2, linea + nedge)
		lim1 = xv
		lim2 = lim1 + double (width - 1) * dpx
		lim3 = xv + double (linea - line + 1) * ratio
		lim4 = lim3 + double (width - 1) * dpx
		colb = max (col1, int (min (lim1, lim2, lim3, lim4)) - 1)
		colc = min (col2, nint (max (lim1, lim2, lim3, lim4)) + 1)
		buf = imgs2r (im, colb, colc, lineb, linec)
		call msifit (msi, Memr[buf], colc - colb + 1, linec - lineb +
		    1, colc - colb + 1)
	    } else if (dy < 0.0 && (buf == NULL || line < linea)) {
		linea = max (line1, line - NLINES + 1)
		lineb = max (line1, linea - nedge)
		linec = min (line2, line + nedge)
		lim1 = xv
		lim2 = lim1 + double (width - 1) * dpx
		lim3 = xv + double (linea - line - 1) * ratio
		lim4 = lim3 + double (width - 1) * dpx
		colb = max (col1, int (min (lim1, lim2, lim3, lim4)) - 1)
		colc = min (col2, nint (max (lim1, lim2, lim3, lim4)) + 1)
		buf = imgs2r (im, colb, colc, lineb, linec)
		call msifit (msi, Memr[buf], colc - colb + 1, linec - lineb +
		    1, colc - colb + 1)
	    }

	    # Evaluate the interpolant.
	    call aaddkr (Memr[oxs], real (xv - colb + 1), Memr[xs], width)
	    call aaddkr (Memr[oys], real (yv - lineb + 1), Memr[ys], width)
	    call msivector (msi, Memr[xs], Memr[ys], Memr[yvals], width)

	    if (width == 1)
		y_vector[i] = Memr[yvals]
	    else {
		sum = 0.0
		do k = 1, width
		    sum = sum + Memr[yvals+k-1]
		y_vector[i] = sum / width
	    }

	    xv = xv + dx 
	    yv = yv + dy
	}	

	# Compute min and max values.
	call alimr (y_vector, nvals, zmin, zmax)
	 
	# Free memory .
	call msifree (msi)
	call sfree (sp)
end


# PV_GET_COL -- Average a strip perpendicular to a column vector and return
# vectors of point number and average pixel value. Also returned is the min
# and max value in the data vector.

procedure pv_get_col (im, x1, y1, x2, y2, nvals, width, btype,
        bconstant, x_vector, y_vector, zmin, zmax)

pointer im		# pointer to image header
real	x1, y1		# starting pixel of vector
real	x2, y2		# ending pixel of pixel
int	nvals		# number of samples along the vector
int	width		# width of strip to average over
int	btype		# Boundary extension type
real	bconstant	# Boundary extension constant
real	x_vector[ARB]	# Pixel numbers
real	y_vector[ARB]	# Average pixel values (returned)
real	zmin, zmax 	# min, max of data vector

real	sum
int	line, linea, lineb, linec
pointer sp, xs, ys, msi, yvals, buf
double	dx, dy, xoff, noff, xv, yv
int	i, j, k, nedge, col1, col2, line1, line2
pointer	imgs2r()

begin
	call smark (sp)
	call salloc (xs, width, TY_REAL)
	call salloc (ys, width, TY_REAL)
	call salloc (yvals, width, TY_REAL)

	# Initialize the interpolator and the image data buffer.
	call msiinit (msi, II_BILINEAR]
	buf = NULL

	# Set the boundary.
	nedge  = max (2, width / 2 + 1)
	col1 = int (x1) - nedge
	col2 = nint (x1) + nedge
	line1  = int (min (y1, y2)) - nedge
	line2 =  nint (max (y1, y2)) + nedge
	call pv_setboundary (im, col1, col2, line1, line2, btype, bconstant)

	# Determine sampling perpendicular to vector.
	dx = 1.0d0
	if (nvals == 1)
	    dy = 0.0d0
	else
	    dy = (y2 - y1) / (nvals - 1)

	# Compute offset from the nominal vector to the first sample point.
	noff = (real (width) - 1.0) / 2.0
	xoff = noff * dx
	xv = x1 - xoff
	do j = 1, width
	    Memr[xs+j-1] = xv + double (j - col1)
	yv = y1

	# Loop over the output image lines.
	do i = 1, nvals { 
	    x_vector[i] = real (i)
	    line = yv

	    # Get the input image data and fit an interpolator to the data.
	    # The input data is buffered in a section of size NLINES + 2 *
	    # NEDGE.

	    if (dy >= 0.0 && (buf == NULL || line > (linea))) {
		linea = min (line2, line + NLINES - 1)
		lineb = max (line1, line - nedge)
		linec = min (line2, linea + nedge)
		buf = imgs2r (im, col1, col2, lineb, linec)
		call msifit (msi, Memr[buf], col2 - col1 + 1, linec - lineb +
		    1, col2 - col1 + 1)
	    } else if (dy < 0.0 && (buf == NULL || line < linea)) {
		linea = max (line1, line - NLINES + 1)
		lineb = max (line1, linea - nedge)
		linec = min (line2, line + nedge)
		buf = imgs2r (im, col1, col2, lineb, linec)
		call msifit (msi, Memr[buf], col2 - col1 + 1, linec - lineb +
		    1, col2 - col1 + 1)
	    }

	    # Evaluate the interpolant.
	    call amovkr (real (yv - lineb + 1), Memr[ys], width)
	    call msivector (msi, Memr[xs], Memr[ys], Memr[yvals], width)

	    if (width == 1)
		y_vector[i] = Memr[yvals]
	    else {
		sum = 0.0
		do k = 1, width
		    sum = sum + Memr[yvals+k-1]
		y_vector[i] = sum / width
	    }

	    yv = yv + dy
	}	

	# Compute min and max values.
	call alimr (y_vector, nvals, zmin, zmax)
	 
	# Free memory .
	call msifree (msi)
	call sfree (sp)
end


# PV_GET_ROW -- Average a strip parallel to a row vector and return
# vectors of point number and average pixel value. Also returned is the min
# and max value in the data vector.

procedure pv_get_row (im, x1, y1, x2, y2, nvals, width, btype, bconstant,
	x_vector, y_vector, zmin, zmax)

pointer im		# pointer to image header
real	x1, y1		# starting pixel of vector
real	x2, y2		# ending pixel of pixel
int	nvals		# number of samples along the vector
int	width		# width of strip to average over
int	btype		# Boundary extension type
real	bconstant	# Boundary extension constant
real	x_vector[ARB]	# Pixel numbers
real	y_vector[ARB]	# Average pixel values (returned)
real	zmin, zmax 	# min, max of data vector

double	dx, dy, yoff, noff, xv, yv
int	i, j, nedge, col1, col2, line1, line2
int	line, linea, lineb, linec
pointer sp, oys, xs, ys, yvals, msi, buf
pointer	imgs2r()
errchk	imgs2r, msifit

begin
	call smark (sp)
	call salloc (oys, width, TY_REAL)
	call salloc (xs, nvals, TY_REAL)
	call salloc (ys, nvals, TY_REAL)
	call salloc (yvals, nvals, TY_REAL)

	# Initialize the interpolator and the image data buffer.
	call msiinit (msi, II_BILINEAR]
	buf = NULL

	# Set the boundary.
	nedge  = max (2, width / 2 + 1)
	col1 = int (min (x1, x2)) - nedge
	col2 = nint (max (x1, x2)) + nedge
	line1 = int (y1) - nedge
	line2 = nint (y1) + nedge
	call pv_setboundary (im, col1, col2, line1, line2, btype, bconstant)

	# Determine sampling perpendicular to vector.
	if (nvals == 1)
	    dx = 0.0d0
	else
	    dx = (x2 - x1) / (nvals - 1)
	dy = 1.0

	# Compute offset from the nominal vector to the first sample point.
	noff = (real (width) - 1.0) / 2.0
	xv = x1 - col1 + 1
	do i = 1, nvals {
	    Memr[xs+i-1] = xv
	    xv = xv + dx
	}
	yoff = noff * dy
	yv = y1 - yoff
	do j = 1, width
	    Memr[oys+j-1] = yv + double (j - 1)

	# Clear the accululator.
	call aclrr (y_vector, nvals)

	# Loop over the output image lines.
	do i = 1, width { 
	    line = yv

	    # Get the input image data and fit an interpolator to the data.
	    # The input data is buffered in a section of size NLINES + 2 *
	    # NEDGE.

	    if (dy >= 0.0 && (buf == NULL || line > (linea))) {
		linea = min (line2, line + NLINES - 1)
		lineb = max (line1, line - nedge)
		linec = min (line2, linea + nedge)
		buf = imgs2r (im, col1, col2, lineb, linec)
		if (buf == NULL)
		    call error (0, "Error reading input image.")
		call msifit (msi, Memr[buf], col2 - col1 + 1, linec - lineb +
		    1, col2 - col1 + 1)
	    } else if (dy < 0.0 && (buf == NULL || line < linea)) {
		linea = max (line1, line - NLINES + 1)
		lineb = max (line1, linea - nedge)
		linec = min (line2, line + nedge)
		buf = imgs2r (im, col1, col2, lineb, linec)
		if (buf == NULL)
		    call error (0, "Error reading input image.")
		call msifit (msi, Memr[buf], col2 - col1 + 1, linec - lineb +
		    1, col2 - col1 + 1)
	    }

	    # Evaluate the interpolant.
	    call amovkr (real (Memr[oys+i-1] - lineb + 1), Memr[ys], nvals)
	    call msivector (msi, Memr[xs], Memr[ys], Memr[yvals], nvals)

	    if (width == 1)
		call amovr (Memr[yvals], y_vector, nvals)
	    else 
		call aaddr (Memr[yvals], y_vector, y_vector, nvals)

	    yv = yv + dy
	}	

	# Compute the x and y vectors.
	do i = 1, nvals
	    x_vector[i] = real (i)
	if (width > 1)
	    call adivkr (y_vector, real (width), y_vector, nvals)

	# Compute min and max values.
	call alimr (y_vector, nvals, zmin, zmax)
	 
	# Free memory .
	call msifree (msi)
	call sfree (sp)
end


# PV_GET_ROW1 -- Average a strip parallel to a row vector and return
# vectors of point number and average pixel value. Also returned is the min
# and max value in the data vector.

procedure pv_get_row1 (im, x1, x2, nvals, btype, bconstant, x_vector,
	y_vector, zmin, zmax)

pointer im		# pointer to image header
real	x1		# starting pixel of vector
real	x2 		# ending pixel of pixel
int	nvals		# number of samples along the vector
int	btype		# Boundary extension type
real	bconstant	# Boundary extension constant
real	x_vector[ARB]	# Pixel numbers
real	y_vector[ARB]	# Average pixel values (returned)
real	zmin, zmax 	# min, max of data vector

double	dx, xv
int	i, nedge, col1, col2
pointer sp, xs,  asi, buf
pointer	imgs1r()
errchk	imgs1r

begin
	call smark (sp)
	call salloc (xs, nvals, TY_REAL)

	# Initialize the interpolator.
	call asiinit (asi, II_LINEAR]

	# Set the boundary.
	nedge  = 2
	col1 = int (min (x1, x2)) - nedge
	col2 = nint (max (x1, x2)) + nedge
	call pv_setboundary (im, col1, col2, 1, 1, btype, bconstant)

	# Compute the x vector.
	if (nvals == 1)
	    dx = 0.0d0
	else
	    dx = (x2 - x1) / (nvals - 1)
	xv = x1 - col1 + 1
	do i = 1, nvals {
	    Memr[xs+i-1] = xv
	    xv = xv + dx
	}

	# Get the image data, fit and evaluate  the interpolant.
	buf = imgs1r (im, col1, col2)
	if (buf == NULL)
	    call error (0, "Error reading input image.")
	call asifit (asi, Memr[buf], col2 - col1 + 1)
	call asivector (asi, Memr[xs], y_vector, nvals)

	# Compute the output x vector.
	do i = 1, nvals
	    x_vector[i] = real (i)

	# Compute min and max values.
	call alimr (y_vector, nvals, zmin, zmax)
	 
	# Free memory .
	call asifree (asi)
	call sfree (sp)
end


# PV_SETBOUNDARY -- Set boundary extension.

procedure pv_setboundary (im, col1, col2, line1, line2, btype, bconstant)

pointer	im			# IMIO pointer
int	col1, col2		# Range of columns
int	line1, line2		# Range of lines
int	btype			# Boundary extension type
real	bconstant		# Constant for constant boundary extension

int	btypes[5]
int	nbndrypix
data	btypes /BT_CONSTANT, BT_NEAREST, BT_REFLECT, BT_WRAP, BT_PROJECT/

begin
	nbndrypix = 0
	nbndrypix = max (nbndrypix, 1 - col1)
	nbndrypix = max (nbndrypix, col2 - IM_LEN(im, 1))
	nbndrypix = max (nbndrypix, 1 - line1)
	nbndrypix = max (nbndrypix, line2 - IM_LEN(im, 2))

	call imseti (im, IM_TYBNDRY, btypes[btype])
	call imseti (im, IM_NBNDRYPIX, nbndrypix + 1)
	if (btypes[btype] == BT_CONSTANT)
	    call imsetr (im, IM_BNDRYPIXVAL, bconstant)
end


# PV_BUFL2R -- Maintain buffer of image lines.  A new buffer is created when
# the buffer pointer is null or if the number of lines requested is changed.
# The minimum number of image reads is used.

procedure pv_bufl2r (im, col1, col2, line1, line2, buf)

pointer	im		# Image pointer
int	col1		# First image column of buffer
int	col2		# Last image column of buffer
int	line1		# First image line of buffer
int	line2		# Last image line of buffer
pointer	buf		# Buffer

int	i, ncols, nlines, nclast, llast1, llast2, nllast
pointer	buf1, buf2
pointer	imgs2r()

begin
	ncols = col2 - col1 + 1
	nlines = line2 - line1 + 1

	# If the buffer pointer is undefined then allocate memory for the
	# buffer.  If the number of columns or lines requested changes
	# reallocate the buffer.  Initialize the last line values to force
	# a full buffer image read.

	if (buf == NULL) {
	    call malloc (buf, ncols * nlines, TY_REAL)
	    llast1 = line1 - nlines
	    llast2 = line2 - nlines
	} else if ((nlines != nllast) || (ncols != nclast)) {
	    call realloc (buf, ncols * nlines, TY_REAL)
	    llast1 = line1 - nlines
	    llast2 = line2 - nlines
	}

	# Read only the image lines with are different from the last buffer.
	if (line1 < llast1) {
	    do i = line2, line1, -1 {
		if (i > llast1)
		    buf1 = buf + (i - llast1) * ncols
		else
		    buf1 = imgs2r (im, col1, col2, i, i)
		    
		buf2 = buf + (i - line1) * ncols
		call amovr (Memr[buf1], Memr[buf2], ncols)
	    }
	} else if (line2 > llast2) {
	    do i = line1, line2 {
		if (i < llast2)
		    buf1 = buf + (i - llast1) * ncols
		else
		    buf1 = imgs2r (im, col1, col2, i, i)
		    
		buf2 = buf + (i - line1) * ncols
		call amovr (Memr[buf1], Memr[buf2], ncols)
	    }
	}

	# Save the buffer parameters.
	llast1 = line1
	llast2 = line2
	nclast = ncols
	nllast = nlines
end
