include	<math.h>
include	<imhdr.h>
include	<math/gsurfit.h>
include	<math/iminterp.h>
include	<units.h>

define	ITYPES	"|nearest|linear|poly3|poly5|spline3|"

# T_TRANSFORM -- Transform longslit images.
# Input consists of images to be transformed, the user coordinate surfaces
# describing the output coordinates in terms of the input coordinates,
# and the desired coordinates for the output images.  The type of image
# interpolation is also input.  There is a log output as well as the
# transformed images.  The output image may replace the input image.

procedure t_transform ()

int	input			# List of input images
int	output			# List of output images
int	fitnames		# List of user coordinate fits
pointer	database		# Database
char	interp[10]		# Interpolation type
int	logfiles		# List of log files

int	itypes[II_NTYPES2D], logfd
pointer	in, out
pointer	un[2], xmsi, ymsi, jmsi, xout, yout, dxout, dyout
pointer	sp, image1, image2, image3, str

int	clpopnu(), clgfil(), clgeti(), clgwrd(), open()
int	imtopenp(), imtlen(), imtgetim()
bool	clgetb()
real	clgetr()
pointer	immap()
errchk	open()

data	itypes /II_BINEAREST, II_BILINEAR, II_BIPOLY3, II_BIPOLY5, II_BISPLINE3/

include	"transform.com"

begin
	call smark (sp)
	call salloc (database, SZ_FNAME, TY_CHAR)
	call salloc (image1, SZ_FNAME, TY_CHAR)
	call salloc (image2, SZ_FNAME, TY_CHAR)
	call salloc (image3, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Get and error check the input and output image lists and the other
	# task parameters.

	input = imtopenp ("input")
	output = imtopenp ("output")
	if (imtlen (input) != imtlen (output)) {
	    call imtclose (input)
	    call imtclose (output)
	    call error (0, "Number of input and output images differ")
	}

	fitnames = clpopnu ("fitnames")
	call clgstr ("database", Memc[database], SZ_FNAME)
	itype = itypes[clgwrd ("interptype", interp, 10, ITYPES)]
	logfiles = clpopnu ("logfiles")

	u1 = clgetr ("x1")
	u2 = clgetr ("x2")
	du = clgetr ("dx")
	nu = clgeti ("nx")
	v1 = clgetr ("y1")
	v2 = clgetr ("y2")
	dv = clgetr ("dy")
	nv = clgeti ("ny")

	ulog = clgetb ("xlog")
	vlog = clgetb ("ylog")
	flux = clgetb ("flux")

	# Transform each input image to the output image.
	xmsi = NULL
	while ((imtgetim (input, Memc[image1], SZ_FNAME) != EOF)  &&
	    (imtgetim (output, Memc[image2], SZ_FNAME) != EOF)) {

	    # Map the input and output images.
	    call xt_mkimtemp (Memc[image1], Memc[image2], Memc[image3],SZ_FNAME)
	    in = immap (Memc[image1], READ_ONLY, 0)
	    out = immap (Memc[image2], NEW_COPY, in)

	    # Get the coordinate transformation surfaces from the database.
	    # Do this only on the first pass.

	    if (xmsi == NULL)
		call tr_setup (Memc[database], fitnames, un, xmsi, ymsi, jmsi,
		    xout, yout, dxout, dyout)

	    # Write log information.
	    while (clgfil (logfiles, Memc[str], SZ_LINE) != EOF) {
		logfd = open (Memc[str], APPEND, TEXT_FILE)
		call sysid (Memc[str], SZ_LINE)
		call fprintf (logfd, "\n%s\n")
		    call pargstr (Memc[str])
		call fprintf (logfd, "  Transform %s to %s.\n")
		    call pargstr (Memc[image1])
		    call pargstr (Memc[image3])
		if (flux)
		    call fprintf (logfd, "  Conserve flux per pixel.\n")
		call fprintf (logfd, "  User coordinate transformations:\n")
		while (clgfil (fitnames, Memc[str], SZ_LINE) != EOF) {
		    call fprintf (logfd, "    %s\n")
			call pargstr (Memc[str])
		}
		call fprintf (logfd, "  Interpolation is %s.\n")
		    call pargstr (interp)
		call fprintf (logfd, "  Output coordinate parameters are:\n")
		call fprintf (logfd,
	    "    x1 = %10.4g, x2 = %10.4g, dx = %10.4g, nx = %4d, xlog = %b\n")
		    call pargr (u1)
		    call pargr (u2)
		    call pargr (du)
		    call pargi (nu)
		    call pargb (ulog)
		call fprintf (logfd,
	    "    y1 = %10.4g, y2 = %10.4g, dy = %10.4g, ny = %4d, ylog = %b\n")
		    call pargr (v1)
		    call pargr (v2)
		    call pargr (dv)
		    call pargi (nv)
		    call pargb (vlog)
		call close (logfd)
	    }
	    call clprew (logfiles)

	    call tr_transform (in, out, un, xmsi, ymsi, jmsi, Memr[xout],
		Memr[yout], Memr[dxout], Memr[dyout])

	    call imunmap (in)
	    call imunmap (out)
	    call xt_delimtemp (Memc[image2], Memc[image3])
	}

	call mfree (xout, TY_REAL)
	call mfree (yout, TY_REAL)
	call mfree (dxout, TY_REAL)
	call mfree (dyout, TY_REAL)
	call msifree (xmsi)
	call msifree (ymsi)
	if (jmsi != NULL)
	    call msifree (jmsi)
	if (un[1] != NULL)
	    call un_close (un[1])
	if (un[2] != NULL)
	    call un_close (un[2])
	call imtclose (input)
	call imtclose (output)
	call clpcls (fitnames)
	call clpcls (logfiles)
	call sfree (sp)
end


define	MAXSAMPLE	50	# Maximum sample points

# TR_SETUP -- Setup the transformation interpolation.
# At each point (U,V) in the output image we need to know the coordinate
# (X,Y) of the input images to be interpolated.  This means we need to
# determine X(U,V) and Y(U,V).  The input user coordinate surfaces,
# however, are U(X,Y) and V(X,Y) (a missing surface implies a one to on
# mapping of U=X or V=Y).  This requires simultaneously inverting the
# user coordinate surfaces.  This is a slow process using a gradient
# following iterative technique.  Therefore, the inverted coordinates are
# determined on a evenly subsampled grid of linear output coordinates.  A
# linear interpolation surface can then be fit to this grid which is much
# faster to evaluate at each output coordinate.  These interpolation
# surfaces are returned.  If flux is to be conserved a similar
# interpolation surface for the Jacobian, J(U,V) is also returned.  There
# may also be a mapping of the output image into logrithmic intervals
# which maps to the linearly sampled interpolation surfaces.  The
# mappings of the output U and V intervals to the subsampled
# interpolation coordinates are also returned.
# 
# 1. Get the coordinate surfaces from the database.
# 2. Set the output coordinate system based on the ranges of X, Y, U, and V.
# 3. Determine X(U,V), Y(U,V), and J(U,V) on a evenly subsampled grid of
#    U and V.
# 4. Fit linear interpolation surfaces to these data.
# 5. Compute the mapping between output coordinates along each axis, which
#    may be logrithmic, into the subsampling interpolation coordinates.

procedure tr_setup (database, sflist, un, xmsi, ymsi, jmsi, uout, vout,
	duout, dvout)

char	database[SZ_FNAME]	# Database containing coordinate surfaces
int	sflist			# List of user coordinate surfaces
pointer	un[2]			# Units pointers
pointer	xmsi, ymsi, jmsi	# Surface interpolators for X, Y and Jacobian
pointer	uout, vout		# Output coordinates relative to interpolator 
pointer	duout, dvout		# Output coordinate intervals

int	i, j, nsf, nusf, nvsf, nu1, nv1
real	xmin, xmax, ymin, ymax, umin, umax, vmin, vmax
real	u, v, x, y, du1, dv1, der[8]
pointer	sp, sfname, usf, vsf, un1, sf, xgrid, ygrid, zgrid, ptr1, ptr2, ptr3

bool	un_compare()
int	clgfil(), clplen()
real	xgsgetr(), xgseval()

include	"transform.com"

begin
	# Get the user coordinate surfaces and separate them into U and V.
	# Check that all surfaces have the same range of X and Y and determine
	# the range of U and V.

	nsf = max (1, clplen (sflist))

	call smark (sp)
	call salloc (sfname, SZ_FNAME, TY_CHAR)
	call salloc (usf, nsf, TY_INT)
	call salloc (vsf, nsf, TY_INT)

	un[1] = NULL
	un[2] = NULL
	Memi[usf] = NULL
	Memi[vsf] = NULL
	nusf = 0
	nvsf = 0
	while (clgfil (sflist, Memc[sfname], SZ_FNAME) != EOF) {
	    call lm_dbread (database, Memc[sfname], j, un1, sf)
	    if (un1 != NULL) {
		if (un[j] == NULL)
		    un[j] = un1
		else if (un_compare (un1, un[j]))
		    call un_close (un1)
		else {
		    call un_close (un1)
		    call un_close (un[j])
		    call sfree (sp)
		    call error (1, "Input units disagree")
		}
	    }

	    if (sf != NULL) {
		if (j == 1) {
		    nusf = nusf+1
		    Memi[usf+nusf-1] = sf
		} else if (j == 2) {
		    nvsf = nvsf+1
		    Memi[vsf+nvsf-1] = sf
		}
	    }
	}
	call clprew (sflist)

	if (nusf + nvsf == 0)
	    call error (0, "No user coordinates")

	xmin = INDEF
	xmax = INDEF
	ymin = INDEF
	ymax = INDEF
	umin = INDEF
	umax = INDEF
	vmin = INDEF
	vmax = INDEF
	do i = 1, nusf {
	    if (IS_INDEF (xmin)) {
		xmin = xgsgetr (Memi[usf+i-1], GSXMIN)
		xmax = xgsgetr (Memi[usf+i-1], GSXMAX)
		ymin = xgsgetr (Memi[usf+i-1], GSYMIN)
		ymax = xgsgetr (Memi[usf+i-1], GSYMAX)
	    } else {
		if ((xmin != xgsgetr (Memi[usf+i-1], GSXMIN)) ||
		    (xmax != xgsgetr (Memi[usf+i-1], GSXMAX)) ||
		    (ymin != xgsgetr (Memi[usf+i-1], GSYMIN)) ||
		    (ymax != xgsgetr (Memi[usf+i-1], GSYMAX)))
		    call error (0, "tr_setup: Inconsistent coordinate fits")
	    }

	    if (IS_INDEF (umin)) {
	        umin = xgseval (Memi[usf+i-1], xmin, ymin)
	        umax = umin
	    }
	    u = xgseval (Memi[usf+i-1], xmin, ymin)
	    umin = min (u, umin)
	    umax = max (u, umax)
	    u = xgseval (Memi[usf+i-1], xmax, ymin)
	    umin = min (u, umin)
	    umax = max (u, umax)
	    u = xgseval (Memi[usf+i-1], xmin, ymax)
	    umin = min (u, umin)
	    umax = max (u, umax)
	    u = xgseval (Memi[usf+i-1], xmax, ymax)
	    umin = min (u, umin)
	    umax = max (u, umax)
	}
	do i = 1, nvsf {
	    if (IS_INDEF (xmin)) {
		xmin = xgsgetr (Memi[vsf+i-1], GSXMIN)
		xmax = xgsgetr (Memi[vsf+i-1], GSXMAX)
		ymin = xgsgetr (Memi[vsf+i-1], GSYMIN)
		ymax = xgsgetr (Memi[vsf+i-1], GSYMAX)
	    } else {
		if ((xmin != xgsgetr (Memi[vsf+i-1], GSXMIN)) ||
		    (xmax != xgsgetr (Memi[vsf+i-1], GSXMAX)) ||
		    (ymin != xgsgetr (Memi[vsf+i-1], GSYMIN)) ||
		    (ymax != xgsgetr (Memi[vsf+i-1], GSYMAX)))
		    call error (0, "tr_setup: Inconsistent coordinate fits")
	    }

	    if (IS_INDEF (vmin)) {
	        vmin = xgseval (Memi[vsf+i-1], xmin, ymin)
	        vmax = vmin
	    }
	    v = xgseval (Memi[vsf+i-1], xmin, ymin)
	    vmin = min (v, vmin)
	    vmax = max (v, vmax)
	    v = xgseval (Memi[vsf+i-1], xmax, ymin)
	    vmin = min (v, vmin)
	    vmax = max (v, vmax)
	    v = xgseval (Memi[vsf+i-1], xmin, ymax)
	    vmin = min (v, vmin)
	    vmax = max (v, vmax)
	    v = xgseval (Memi[vsf+i-1], xmax, ymax)
	    vmin = min (v, vmin)
	    vmax = max (v, vmax)
	}
	if (IS_INDEF (umin)) {
	    umin = xmin
	    umax = xmax
	}
	if (IS_INDEF (vmin)) {
	    vmin = ymin
	    vmax = ymax
	}

	# Set the output coordinate system which is in a common block.
	call tr_setoutput (xmin, xmax, ymin, ymax, umin, umax, vmin, vmax)

	# Subsample the inverted coordinates and fit an interpolation
	# surface.  The grid is evaluated in a back and forth pattern to
	# use the last point evaluated and the starting point for the next
	# point.  This allows the interative inversion routine to work most
	# efficiently with typically only two evaluations per step.

	nu1 = nu / ((nu + MAXSAMPLE - 1) / MAXSAMPLE)
	nv1 = nv / ((nv + MAXSAMPLE - 1) / MAXSAMPLE)
	du1 = (u2 - u1) / (nu1 - 1)
	dv1 = (v2 - v1) / (nv1 - 1)

	call malloc (xgrid, nu1 * nv1, TY_REAL)
	call malloc (ygrid, nu1 * nv1, TY_REAL)
	call malloc (zgrid, nu1 * nv1, TY_REAL)

	call tr_init (Memi[usf], nusf, Memi[vsf], nvsf, xmin, ymin, der)
	do i = 1, nv1, 2 {
	    # Do this line from left to right.
	    ptr1 = xgrid + (i - 1) * nu1 - 1
	    ptr2 = ygrid + (i - 1) * nu1 - 1
	    ptr3 = zgrid + (i - 1) * nu1 - 1
	    v = v1 + (i - 1) * dv1
	    do j = 1, nu1 {
		u = u1 + (j - 1) * du1
		call tr_invert (Memi[usf], nusf, Memi[vsf], nvsf, u, v, x, y,
		    der, xmin, xmax, ymin, ymax)
		# V2.10.2
		#Memr[ptr1+j] = der[1]
		#Memr[ptr2+j] = der[2]
		# After V2.10.3
		Memr[ptr1+j] = x
		Memr[ptr2+j] = y

		Memr[ptr3+j] = 1. / abs (der[4] * der[8] - der[5] * der[7])
	    }
	    if (i == nv1)
		break

	    # Do the next line from right to left.
	    ptr1 = xgrid + i * nu1 - 1
	    ptr2 = ygrid + i * nu1 - 1
	    ptr3 = zgrid + i * nu1 - 1
	    v = v1 + i * dv1
	    do j = nu1, 1, -1 {
		u = u1 + (j - 1) * du1
		call tr_invert (Memi[usf], nusf, Memi[vsf], nvsf, u, v, x, y,
		    der, xmin, xmax, ymin, ymax)
		# V2.10.2
		#Memr[ptr1+j] = der[1]
		#Memr[ptr2+j] = der[2]
		# V2.10.3
		Memr[ptr1+j] = x
		Memr[ptr2+j] = y
		Memr[ptr3+j] = 1. / abs (der[4] * der[8] - der[5] * der[7])
	    }
	}

	# Free the surfaces since we are no done with them.
	for (i=1; i<=nusf; i=i+1)
	    call xgsfree (Memi[usf+i-1])
	for (i=1; i<=nvsf; i=i+1)
	    call xgsfree (Memi[vsf+i-1])

	# Fit a linear interpolator to the subsampled grids of X(U,V), Y(U,V),
	# and J(U,V) to avoid having to evaluate the inverse at each point in
	# the output image.  The inversion is slow because of the many
	# evaluations of the surfaces coordinates.  Also compute an return
	# arrays mapping the output coordinates to the subsampled coordinates.
	# This may include a transformation to logrithmic intervals.

	call msiinit (xmsi, II_BILINEAR)
	call msifit (xmsi, Memr[xgrid], nu1, nv1, nu1)
	call mfree (xgrid, TY_REAL)

	call msiinit (ymsi, II_BILINEAR)
	call msifit (ymsi, Memr[ygrid], nu1, nv1, nu1)
	call mfree (ygrid, TY_REAL)

	if (flux) {
	    call msiinit (jmsi, II_BILINEAR)
	    call msifit (jmsi, Memr[zgrid], nu1, nv1, nu1)
	}
	call mfree (zgrid, TY_REAL)

	# Compute the mapping between output coordinates and the subsampled
	# interpolation surface.  Also compute the intervals used to define
	# the pixel areas for conserving flux.

	call malloc (uout, nu, TY_REAL)
	call malloc (duout, nu, TY_REAL)
	if (ulog) {
	    v = log10 (u1)
	    do i = 0, nu - 1
		Memr[uout+i] = 10. ** (v + i * du)
	    call amulkr (Memr[uout], du * LN_10, Memr[duout], nu)
	} else {
	    do i = 0, nu - 1
		Memr[uout+i] = u1 + i * du
	    call amovkr (du, Memr[duout], nu)
	}
	u2 = Memr[uout+nu-1]

	call malloc (vout, nv, TY_REAL)
	call malloc (dvout, nv, TY_REAL)
	if (vlog) {
	    v = log10 (v1)
	    do i = 0, nv - 1
		Memr[vout+i] = 10. ** (v + i * dv)
	    call amulkr (Memr[vout], dv * LN_10, Memr[dvout], nv)
	} else {
	    do i = 0, nv - 1
		Memr[vout+i] = v1 + i * dv
	    call amovkr (dv, Memr[dvout], nv)
	}
	v2 = Memr[vout+nv-1]

	# Convert to interpolation coordinates.
	call asubkr (Memr[uout], u1, Memr[uout], nu)
	call adivkr (Memr[uout], du1, Memr[uout], nu)
	call aaddkr (Memr[uout], 1., Memr[uout], nu)
	call asubkr (Memr[vout], v1, Memr[vout], nv)
	call adivkr (Memr[vout], dv1, Memr[vout], nv)
	call aaddkr (Memr[vout], 1., Memr[vout], nv)

	call sfree (sp)
end


# TR_SETOUTPUT -- Set the output coordinates in the common block.
# This procedure allows the user to specifying a part of the output
# coordinates and let the rest default based on the full limits of
# the user coordinate surfaces.

procedure tr_setoutput (xmin, xmax, ymin, ymax, umin, umax, vmin, vmax)

real	xmin, xmax, ymin, ymax
real	umin, umax, vmin, vmax

int	nua, nva
real	u1a, u2a, dua, v1a, v2a, dva

include	"transform.com"

begin
	# Save the original values of the user parameters.
	u1a = u1
	u2a = u2
	dua = du
	nua = nu
	v1a = v1
	v2a = v2
	dva = dv
	nva = nv

	# If the output coordinate limits are not defined then use the
	# transformation surface limits.

	if (IS_INDEF (u1))
	    u1 = umin
	if (IS_INDEF (u2))
	    u2 = umax
	if (IS_INDEF (v1))
	    v1 = vmin
	if (IS_INDEF (v2))
	    v2 = vmax

	# If the number of output pixels are not defined then use the number
	# of pixels in the input image.

	if (IS_INDEFI (nu))
	    nu = xmax - xmin + 1
	if (IS_INDEFI (nv))
	    nv = ymax - ymin + 1

	# If the coordinate interval is not defined determine it from the
	# number of pixels and the coordinate limits.  If the interval is
	# defined then override the number of pixels.

	if (ulog) {
	    if (IS_INDEF (du))
		du = (log10 (u2) - log10 (u1)) / (nu - 1)
	    else if (IS_INDEFI (nua))
	        nu = nint ((log10 (u2) - log10 (u1)) / du + 1)
	    else if (IS_INDEF (u1a))
		u1 = 10.0 ** (log10 (u2) - du * (nu - 1))
	    else
		u2 = 10.0 ** (log10 (u1) + du * (nu - 1))
	} else {
	    if (IS_INDEF (du))
	        du = (u2 - u1) / (nu - 1)
	    else if (IS_INDEFI (nua))
	        nu = nint ((u2 - u1) / du + 1)
	    else if (IS_INDEF (u1a))
		u1 = u2 - du * (nu - 1)
	    else
		u2 = u1 + du * (nu - 1)
	}

	if (vlog) {
	    if (IS_INDEF (dv))
		dv = (log10 (v2) - log10 (v1)) / (nv - 1)
	    else if (IS_INDEFI (nva))
	        nv = nint ((log10 (v2) - log10 (v1)) / dv + 1)
	    else if (IS_INDEF (v1a))
		v1 = 10.0 ** (log10 (v2) - dv * (nv - 1))
	    else
		v2 = 10.0 ** (log10 (v1) + dv * (nv - 1))
	} else {
	    if (IS_INDEF (dv))
	        dv = (v2 - v1) / (nv - 1)
	    else if (IS_INDEFI (nva))
	        nv = nint ((v2 - v1) / dv + 1)
	    else if (IS_INDEF (v1a))
		v1 = v2 - dv * (nv - 1)
	    else
		v2 = v1 + dv * (nv - 1)
	}
end


define	MAX_ITERATE	10
define	ERROR		0.05
define	FUDGE		0.5

# TR_INVERT -- Given user coordinate surfaces U(X,Y) and V(X,Y)
# (if none use one-to-one mapping and if more than one average)
# corresponding to a given U and V and also the various partial
# derivatives.  This is done using a gradient following interative
# method based on evaluating the partial derivative at each point
# and solving the linear Taylor expansions simultaneously.  The last
# point sampled is used as the starting point.  Thus, if the
# input U and V progress smoothly then the number of iterations
# can be small.  The output is returned in x and y and in the derivative array
# DER.  A point outside of the surfaces is returned as the nearest
# point at the edge of the surfaces in the DER array.

procedure tr_invert (usf, nusf, vsf, nvsf, u, v, x, y, der,
	xmin, xmax, ymin, ymax)

pointer	usf[ARB], vsf[ARB]	# User coordinate surfaces U(X,Y) and V(X,Y)
int	nusf, nvsf		# Number of surfaces for each coordinate
real	u, v			# Input U and V to determine X and Y
real	x, y			# Output X and Y
real	der[8]			# Last result as input, new result as output 
				# 1=X, 2=Y, 3=U, 4=DUDX, 5=DUDY, 6=V,
				# 7=DVDX, 8=DVDY
real	xmin, xmax, ymin, ymax	# Limits of coordinate surfaces.

int	i, j, nedge
real	fudge, du, dv, dx, dy, tmp[3]

begin
	# Use the last result as the starting point for the next position.
	# If this is near the desired value then the interation will converge
	# quickly.  Allow a interation to go off the surface twice.
	# Quit when DX and DY are within ERROR.

	nedge = 0
	do i = 1, MAX_ITERATE {
	    du = u - der[3]
	    dv = v - der[6]
	    dx = (der[8] * du - der[5] * dv) /
		(der[8] * der[4] - der[5] * der[7])
	    dy = (dv - der[7] * dx) / der[8]
	    fudge = 1 - FUDGE / i
	    x = der[1] + fudge * dx
	    y = der[2] + fudge * dy
	    der[1] = max (xmin, min (xmax, x))
	    der[2] = max (ymin, min (ymax, y))
	    if (x < xmin || x > xmax)
	        nedge = nedge + 1
	    if (y < ymin || y > ymax)
	        nedge = nedge + 1
	    if (nedge > 2)
	        break
	    if ((abs (dx) < ERROR) && (abs (dy) < ERROR))
	        break

	    if (nusf == 0)
		der[3] = der[1]
	    else if (nusf == 1) {
	        call xgsder (usf[1], der[1], der[2], der[3], 1, 0, 0)
	        call xgsder (usf[1], der[1], der[2], der[4], 1, 1, 0)
	        call xgsder (usf[1], der[1], der[2], der[5], 1, 0, 1)
	    } else {
	        call xgsder (usf[1], der[1], der[2], der[3], 1, 0, 0)
	        call xgsder (usf[1], der[1], der[2], der[4], 1, 1, 0)
	        call xgsder (usf[1], der[1], der[2], der[5], 1, 0, 1)
		do j = 2, nusf {
	            call xgsder (usf[j], der[1], der[2], tmp[1], 1, 0, 0)
	            call xgsder (usf[j], der[1], der[2], tmp[2], 1, 1, 0)
	            call xgsder (usf[j], der[1], der[2], tmp[3], 1, 0, 1)
		    der[3] = der[3] + tmp[1]
		    der[4] = der[4] + tmp[2]
		    der[5] = der[5] + tmp[3]
		}
		der[3] = der[3] / nusf
		der[4] = der[4] / nusf
		der[5] = der[5] / nusf
	    }

	    if (nvsf == 0)
		der[6] = der[2]
	    else if (nvsf == 1) {
	        call xgsder (vsf[1], der[1], der[2], der[6], 1, 0, 0)
	        call xgsder (vsf[1], der[1], der[2], der[7], 1, 1, 0)
	        call xgsder (vsf[1], der[1], der[2], der[8], 1, 0, 1)
	    } else {
	        call xgsder (vsf[1], der[1], der[2], der[6], 1, 0, 0)
	        call xgsder (vsf[1], der[1], der[2], der[7], 1, 1, 0)
	        call xgsder (vsf[1], der[1], der[2], der[8], 1, 0, 1)
		do j = 2, nvsf {
	            call xgsder (vsf[j], der[1], der[2], tmp[1], 1, 0, 0)
	            call xgsder (vsf[j], der[1], der[2], tmp[2], 1, 1, 0)
	            call xgsder (vsf[j], der[1], der[2], tmp[3], 1, 0, 1)
		    der[6] = der[6] + tmp[1]
		    der[7] = der[7] + tmp[2]
		    der[8] = der[8] + tmp[3]
		}
		der[6] = der[6] / nvsf
		der[7] = der[7] / nvsf
		der[8] = der[8] / nvsf
	    }
	}
end


# TR_INIT -- Since the inversion iteration always begins from the last
# point we need to initialize before the first call to TR_INVERT.

procedure tr_init (usf, nusf, vsf, nvsf, x, y, der)

pointer	usf[ARB], vsf[ARB]	# User coordinate surfaces
int	nusf, nvsf		# Number of surfaces for each coordinate
real	x, y			# Starting X and Y
real	der[8]			# Inversion data

int	j
real	tmp[3]

begin
	der[1] = x
	der[2] = y
	if (nusf == 0) {
	    der[3] = der[1]
	    der[4] = 1.
	    der[5] = 0.
	} else if (nusf == 1) {
	    call xgsder (usf[1], der[1], der[2], der[3], 1, 0, 0)
	    call xgsder (usf[1], der[1], der[2], der[4], 1, 1, 0)
	    call xgsder (usf[1], der[1], der[2], der[5], 1, 0, 1)
	} else {
	    call xgsder (usf[1], der[1], der[2], der[3], 1, 0, 0)
	    call xgsder (usf[1], der[1], der[2], der[4], 1, 1, 0)
	    call xgsder (usf[1], der[1], der[2], der[5], 1, 0, 1)
	    do j = 2, nusf {
	        call xgsder (usf[j], der[1], der[2], tmp[1], 1, 0, 0)
	        call xgsder (usf[j], der[1], der[2], tmp[2], 1, 1, 0)
	        call xgsder (usf[j], der[1], der[2], tmp[3], 1, 0, 1)
		der[3] = der[3] + tmp[1]
		der[4] = der[4] + tmp[2]
		der[5] = der[5] + tmp[3]
	    }
	    der[3] = der[3] / nusf
	    der[4] = der[4] / nusf
	    der[5] = der[5] / nusf
	}

	if (nvsf == 0) {
	    der[6] = der[2]
	    der[7] = 0.
	    der[8] = 1.
	} else if (nvsf == 1) {
	    call xgsder (vsf[1], der[1], der[2], der[6], 1, 0, 0)
	    call xgsder (vsf[1], der[1], der[2], der[7], 1, 1, 0)
	    call xgsder (vsf[1], der[1], der[2], der[8], 1, 0, 1)
	} else {
	    call xgsder (vsf[1], der[1], der[2], der[6], 1, 0, 0)
	    call xgsder (vsf[1], der[1], der[2], der[7], 1, 1, 0)
	    call xgsder (vsf[1], der[1], der[2], der[8], 1, 0, 1)
	    do j = 2, nvsf {
	        call xgsder (vsf[j], der[1], der[2], tmp[1], 1, 0, 0)
	        call xgsder (vsf[j], der[1], der[2], tmp[2], 1, 1, 0)
	        call xgsder (vsf[j], der[1], der[2], tmp[3], 1, 0, 1)
		der[6] = der[6] + tmp[1]
		der[7] = der[7] + tmp[2]
		der[8] = der[8] + tmp[3]
	    }
	    der[6] = der[6] / nvsf
	    der[7] = der[7] / nvsf
	    der[8] = der[8] / nvsf
	}
end


define	NBUF		16	# Additional buffer for interpolation
define	NEDGE		2	# Number of edge lines to add for interpolation

# TR_TRANSFORM -- Perform the image transformation using a user specified
# image interpolator.

procedure tr_transform (in, out, un, xmsi, ymsi, jmsi, xout, yout, dxout, dyout)

pointer	in, out			# IMIO pointers
pointer	un[2]			# Units
pointer xmsi, ymsi		# Coordinate interpolation pointers
pointer	jmsi			# Jacobian interpolation pointer
real	xout[ARB], yout[ARB]	# Output grid relative to interpolation surface
real	dxout[ARB], dyout[ARB]	# Output coordinate intervals

int	i, nxin, nyin, line1, line2, line3, line4, nlines, laxis, paxis, axis[2]
real	a, b, r[2], w[2], cd[2,2]
pointer	zmsi, buf, bufout
pointer	sp, xin, yin, y, mw

pointer	mw_open(), impl2r()
errchk	get_daxis
data	axis/1,2/

include	"transform.com"

begin
	# Initialize the output image header.

	IM_LEN(out, 1) = nu
	IM_LEN(out, 2) = nv

	mw = mw_open (NULL, 2)
	call mw_newsystem (mw, "world", 2)
	do i = 1, 2 {
	    call mw_swtype (mw, i, 1, "linear", "")
	    if (un[i] != NULL) {
		call mw_swattrs (mw, i, "label", UN_LABEL(un[i]))
		call mw_swattrs (mw, i, "units", UN_UNITS(un[i]))
	    }
	}

	r[1] = 1.
	if (ulog)
	    w[1] = log10 (u1)
	else
	    w[1] = u1
	cd[1,1] = du
	cd[1,2] = 0.
	r[2] = 1.
	if (vlog)
	    w[2] = log10 (v1)
	else
	    w[2] = v1
	cd[2,2] = dv
	cd[2,1] = 0.
	call mw_swtermr (mw, r, w, cd, 2)

	# The following image parameters are for compatibility with the
	# ONEDSPEC package.

	call imastr (out, "DCLOG1", "Transform")
	call get_daxis (in, laxis, paxis)
	call imaddi (out, "dispaxis", laxis)
	switch (laxis) {
	case 1:
	    if (ulog)
		call imaddi (out, "dc-flag", 1)
	    else
		call imaddi (out, "dc-flag", 0)
	    if (un[laxis] == NULL) {
		call mw_swattrs (mw, laxis, "label", "Wavelength")
		call mw_swattrs (mw, laxis, "units", "Angstroms")
	    }
	case 2:
	    if (vlog)
		call imaddi (out, "dc-flag", 1)
	    else
		call imaddi (out, "dc-flag", 0)
	    if (un[laxis] == NULL) {
		call mw_swattrs (mw, laxis, "label", "Wavelength")
		call mw_swattrs (mw, laxis, "units", "Angstroms")
	    }
	}
	call mw_saveim (mw, out)
	call mw_close (mw)

	# Allocate memory for the input coordinates and a vector for the
	# output y coordinates.  Also initialize the image data buffer.

	call smark (sp)
	call salloc (xin, nu, TY_REAL)
	call salloc (yin, nu, TY_REAL)
	call salloc (y, nu, TY_REAL)

	buf = NULL
	nlines = 0

	# Initialize the interpolator.

	call msiinit (zmsi, itype)

	# Do each line of the output image.

	nxin = IM_LEN(in, 1)
	nyin = IM_LEN(in, 2)

	do i = 1, nv {

	    # Evaluate the input coordinates at the output grid for a line
	    # of the output image using the interpolation surfaces.

	    call amovkr (yout[i], Memr[y], nu)
	    call msivector (xmsi, xout, Memr[y], Memr[xin], nu)
	    call msivector (ymsi, xout, Memr[y], Memr[yin], nu)

	    # Determine the coordinate ranges and check for out of bounds.

	    call alimr (Memr[xin], nu, a, b)
	    if (a < 1.)
		call arltr (Memr[xin], nu, 1., 1.)
	    if (b > nxin)
		call argtr (Memr[xin], nu, real (nxin), real (nxin))

	    call alimr (Memr[yin], nu, a, b)
	    if (a < 1.) {
		call arltr (Memr[yin], nu, 1., 1.)
		a = 1.
		b = max (a, b)
	    }
	    if (b > nyin) {
		call argtr (Memr[yin], nu, real (nyin), real (nyin))
		b = nyin
		a = min (a, b)
	    }

	    # Get the input image data and fit an interpolator to the data.

	    if ((buf == NULL) || (b > line2) || (a < line1)) {
		nlines = max (nlines, int (b - a + 2 + NBUF))
		if (buf == NULL) {
		    if (a < nyin / 2) {
		        line1 = max (1, int (a))
		        line2 = min (nyin, line1 + nlines - 1)
		    } else {
		        line2 = min (nyin, int (b+1.))
		        line1 = max (1, line2 - nlines + 1)
		    }
		} else if (b > line2) {
		    line1 = max (1, int (a))
		    line2 = min (nyin, line1 + nlines - 1)
		    line1 = max (1, line2 - nlines + 1)
		} else {
		    line2 = min (nyin, int (b+1.))
		    line1 = max (1, line2 - nlines + 1)
		    line2 = min (nyin, line1 + nlines - 1)
		}
		line3 = max (1, line1 - NEDGE)
		line4 = min (nyin, line2 + NEDGE)
	        call tr_bufl2r (in, line3, line4, buf)
	        call msifit (zmsi, Memr[buf], nxin, line4 - line3 + 1, nxin)
	    }

	    # Evaluate the output image pixel values.  The input coordinates
	    # must be offset to the interpolation data grid.

	    bufout = impl2r (out, i)
	    call asubkr (Memr[yin], real (line3 - 1), Memr[yin], nu)
	    call msivector (zmsi, Memr[xin], Memr[yin], Memr[bufout], nu)

	    # Compute Jacobian and apply to the output image.

	    if (flux) {
	        call msivector (jmsi, xout, Memr[y], Memr[xin], nu)
		call amulr (dxout, Memr[xin], Memr[xin], nu)
		call amulkr (Memr[xin], dyout[i], Memr[xin], nu)
	        call amulr (Memr[bufout], Memr[xin], Memr[bufout], nu)
	    }
	}

	# Free memory.

	call mfree (buf, TY_REAL)
	call msifree (zmsi)
	call sfree (sp)
end


# TR_BUFL2R -- Maintain buffer of image lines.  A new buffer is created when
# the buffer pointer is null or if the number of lines requested is changed.
# The minimum number of image reads is used.

procedure tr_bufl2r (im, line1, line2, buf)

pointer	im		# Image pointer
int	line1		# First image line of buffer
int	line2		# Last image line of buffer
pointer	buf		# Buffer

int	i, nlines, nx, last1, last2, nlast
pointer	buf1, buf2

pointer	imgl2r()

begin
	nlines = line2 - line1 + 1

	# If the buffer pointer is undefined then allocate memory for the
	# buffer.  If the number of lines requested changes reallocate
	# the buffer.  Initialize the last line values to force a full
	# buffer image read.

	if (buf == NULL) {
	    nx = IM_LEN(im, 1)
	    call malloc (buf, nx * nlines, TY_REAL)
	    last1 = line1 - nlines
	    last2 = line2 - nlines
	} else if (nlines != nlast) {
	    call realloc (buf, nx * nlines, TY_REAL)
	    last1 = line1 - nlines
	    last2 = line2 - nlines
	}

	# Read only the image lines with are different from the last buffer.

	if (line1 < last1) {
	    do i = line2, line1, -1 {
		if (i > last1)
		    buf1 = buf + (i - last1) * nx
		else
		    buf1 = imgl2r (im, i)
		    
		buf2 = buf + (i - line1) * nx
		call amovr (Memr[buf1], Memr[buf2], nx)
	    }
	} else if (line2 > last2) {
	    do i = line1, line2 {
		if (i < last2)
		    buf1 = buf + (i - last1) * nx
		else
		    buf1 = imgl2r (im, i)
		    
		buf2 = buf + (i - line1) * nx
		call amovr (Memr[buf1], Memr[buf2], nx)
	    }
	}

	# Save the buffer parameters.

	last1 = line1
	last2 = line2
	nlast = nlines
end
