# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<fset.h>
include	<imhdr.h>
include <math/curfit.h>
include	<pkg/gtools.h>
include	"curfit.h"

define	VERBOSE_OUTPUT	1
define	LIST_OUTPUT	2
define	DEFAULT_OUTPUT	3

define	CF_UNIFORM	1
define	CF_USER		2
define	CF_STATISTICAL	3
define	CF_INSTRUMENTAL	4

# CF_FIT -- Called once for each curve to be fit.  


procedure cf_fitr (ic, gt, x, y, wts, nvalues, nmax, device, interactive, ofmt,
    power)

pointer	ic			# ICFIT pointer
pointer	gt			# Graphics tools pointer
real	x[nmax]			# X data values
real	y[nmax]			# Y data values
real	wts[nmax]		# Weights
int	nvalues			# Number of data points
int	nmax			# Maximum number of data points
char	device[SZ_FNAME]	# Output graphics device
int	interactive		# Fit curve interactively?
int	ofmt			# Type of output listing
bool	power			# Convert coeff to power series?

int	ncoeff, i
real	xmin, xmax
pointer	sp, gp, cv, coeff, tty
pointer	gopen(), ttyodes()
int	fstati(), rcvstati()

begin
	# Determine data range and set up curve fitting limits.
	call alimr (x, nvalues, xmin, xmax)
	call ic_putr (ic, "xmin", real (xmin))
	call ic_putr (ic, "xmax", real (xmax))

	if (interactive == YES) {
	    gp = gopen (device, NEW_FILE, STDGRAPH)
	    call icg_fitr (ic, gp, "cursor", gt, cv, x, y, wts, nvalues)
	    call gclose (gp)
	} else 
	    # Do fit non-interactively
	    call ic_fitr (ic, cv, x, y, wts, nvalues, YES, YES, YES, YES)

	# Output answers to STDOUT
	if (ofmt != LIST_OUTPUT) {
 	    if (fstati (STDOUT, F_REDIR) == NO) {
		tty = ttyodes ("terminal")
		call ttyclear (STDOUT, tty)
		call ttycdes (tty)
	    }

	    #call ic_show (ic, "STDOUT", gt)
	    call ic_vshowr (ic, "STDOUT", cv, x, y, wts, nvalues, gt)

	    if (ofmt == VERBOSE_OUTPUT) {
		call printf (
		    "\n# \t    X     \t     Yc   \t     Y    \t     W\n")
	        call cf_listxyr (cv, x, y, wts, nvalues)
	    }
	} else
	    call cf_listxyr (cv, x, y, wts, nvalues)

	# Convert coefficients if requested for legendre or chebyshev
	if (power && ofmt != LIST_OUTPUT) {
 	    # Calculate and print coefficients
	    ncoeff = rcvstati (cv, CVNCOEFF)
	    call smark (sp)
	    call salloc (coeff, ncoeff, TY_REAL)
	    call rcvpower (cv, Memr[coeff], ncoeff)
	    call printf ("# Power series coefficients would be:\n")
	    call printf ("# \t\tcoefficient\n")
	    do i = 1, ncoeff {
		call printf ("# \t%d \t%14.7e\n")
		    call pargi (i)
		    call pargr (Memr[coeff+i-1])
	    }
	    call sfree (sp)
	}

	call cvfree (cv)
	#call ic_close$t (ic)
end


# CF_LISTXY -- Print answers to STDOUT as x,y pairs.

procedure cf_listxyr (cv, xvals, yvals, wts, nvalues)

pointer	cv			# Pointer to curfit structure
int	nvalues			# Number of data values
real	xvals[nvalues]		# Array of x data values
real	yvals[nvalues]		# Array of y data values
real	wts[nvalues]		# Array of weights

int	i
real	rcveval()

begin
	do i = 1, nvalues {
	    call printf ("\t%14.7e \t%14.7e \t%14.7e \t%14.7e\n")
		call pargr (xvals[i])
		call pargr (rcveval (cv, xvals[i]))
		call pargr (yvals[i])
		call pargr (wts[i])
	}
end

# IM_PROJECTION -- Given an image section of arbitrary dimension, compute
# the projection along a single axis by taking the average over the other
# axes.  We do not know about bad pixels.

procedure im_projectionr (im, x, y, w, npix, weighting, axis)

pointer	im			# Pointer to image header structure
real	x[npix]			# Index of projection vector
real	y[npix]			# Receives the projection vector
real	w[npix]			# Receives the weight vector
int	weighting		# Weighting of the individual points
int	npix			# Length of projection vector
int	axis			# The axis to be projected to (x=1)

int	i, lastv
long	v[IM_MAXDIM], nsum, totpix
pointer	pix
real	asumr()
pointer	imgnlr()
errchk	imgnlr

begin
	if (im == NULL)
	    call error (1, "Image projection operator called with null im")
	if (axis < 1 || axis > IM_NDIM(im))
	    call error (2, "Attempt to take projection over nonexistent axis")


	# Set the y projection vector
	call aclrr (y, npix)
	call amovkl (long(1), v, IM_MAXDIM)

	switch (axis) {
	case 1:
	    # Since the image is read line by line, it is easy to compute the
	    # projection along the x-axis (axis 1).  We merely sum all of the
	    # image lines.

	    while (imgnlr (im, pix, v) != EOF)
		call aaddr (Memr[pix], y, y, npix)

	default:
	    # Projecting along any other axis when reading the image line
	    # by line is a bit difficult to understand.  Basically, the
	    # element 'axis' of the V vector (position of the line in the
	    # image) gives us the index into the appropriate element of
	    # y.  When computing the projection over multiple dimensions,
	    # the same output element will be referenced repeatedly.  All
	    # of the elmenents of the input line are summed and added into
	    # this output element.

	    for (lastv=v[axis];  imgnlr (im, pix, v) != EOF;  lastv=v[axis]) {
		i = lastv
		if (i <= npix)
		    y[i] = y[i] + asumr (Memr[pix], IM_LEN(im,1))
	    }
	}

	# Now compute the number of pixels contributing to each element
	# of the output vector.  This is the number of pixels in the image
	# divided by the length of the projection.

	totpix = 1
	do i = 1, IM_NDIM(im)
	    if (i == axis)
		totpix = totpix * min (npix, IM_LEN(im,i))
	    else
		totpix = totpix * IM_LEN(im,i)
	nsum = totpix / min (npix, IM_LEN(im,axis))

	# Compute the average by dividing by the number if pixels summed at
	# each point.
	call adivkr (y, real (nsum), y, npix)

	# Set the x and weight vectors
	do i = 1, npix {
	    x[i] = i
	    switch (weighting) {
	    case CF_STATISTICAL:
		if (y[i] > 0.0)
		    w[i] = 1.0 / y[i]
		else if (y[i] < 0.0)
		    w[i] = abs (1.0 / y[i])
		else
		    w[i] = 1.0
	    case CF_UNIFORM:
	        w[i] = 1.
	    default:
		w[i] = 1.
	    }
	}
end

procedure cf_fitd (ic, gt, x, y, wts, nvalues, nmax, device, interactive, ofmt,
    power)

pointer	ic			# ICFIT pointer
pointer	gt			# Graphics tools pointer
double	x[nmax]			# X data values
double	y[nmax]			# Y data values
double	wts[nmax]		# Weights
int	nvalues			# Number of data points
int	nmax			# Maximum number of data points
char	device[SZ_FNAME]	# Output graphics device
int	interactive		# Fit curve interactively?
int	ofmt			# Type of output listing
bool	power			# Convert coeff to power series?

int	ncoeff, i
double	xmin, xmax
pointer	sp, gp, cv, coeff, tty
pointer	gopen(), ttyodes()
int	fstati(), dcvstati()

begin
	# Determine data range and set up curve fitting limits.
	call alimd (x, nvalues, xmin, xmax)
	call ic_putr (ic, "xmin", real (xmin))
	call ic_putr (ic, "xmax", real (xmax))

	if (interactive == YES) {
	    gp = gopen (device, NEW_FILE, STDGRAPH)
	    call icg_fitd (ic, gp, "cursor", gt, cv, x, y, wts, nvalues)
	    call gclose (gp)
	} else 
	    # Do fit non-interactively
	    call ic_fitd (ic, cv, x, y, wts, nvalues, YES, YES, YES, YES)

	# Output answers to STDOUT
	if (ofmt != LIST_OUTPUT) {
 	    if (fstati (STDOUT, F_REDIR) == NO) {
		tty = ttyodes ("terminal")
		call ttyclear (STDOUT, tty)
		call ttycdes (tty)
	    }

	    #call ic_show (ic, "STDOUT", gt)
	    call ic_vshowd (ic, "STDOUT", cv, x, y, wts, nvalues, gt)

	    if (ofmt == VERBOSE_OUTPUT) {
		call printf (
		    "\n# \t    X     \t     Yc   \t     Y    \t     W\n")
	        call cf_listxyd (cv, x, y, wts, nvalues)
	    }
	} else
	    call cf_listxyd (cv, x, y, wts, nvalues)

	# Convert coefficients if requested for legendre or chebyshev
	if (power && ofmt != LIST_OUTPUT) {
 	    # Calculate and print coefficients
	    ncoeff = dcvstati (cv, CVNCOEFF)
	    call smark (sp)
	    call salloc (coeff, ncoeff, TY_DOUBLE)
	    call dcvpower (cv, Memd[coeff], ncoeff)
	    call printf ("# Power series coefficients would be:\n")
	    call printf ("# \t\tcoefficient\n")
	    do i = 1, ncoeff {
		call printf ("# \t%d \t%14.7e\n")
		    call pargi (i)
		    call pargd (Memd[coeff+i-1])
	    }
	    call sfree (sp)
	}

	call dcvfree (cv)
	#call ic_close$t (ic)
end


# CF_LISTXY -- Print answers to STDOUT as x,y pairs.

procedure cf_listxyd (cv, xvals, yvals, wts, nvalues)

pointer	cv			# Pointer to curfit structure
int	nvalues			# Number of data values
double	xvals[nvalues]		# Array of x data values
double	yvals[nvalues]		# Array of y data values
double	wts[nvalues]		# Array of weights

int	i
double	dcveval()

begin
	do i = 1, nvalues {
	    call printf ("\t%14.7e \t%14.7e \t%14.7e \t%14.7e\n")
		call pargd (xvals[i])
		call pargd (dcveval (cv, xvals[i]))
		call pargd (yvals[i])
		call pargd (wts[i])
	}
end

# IM_PROJECTION -- Given an image section of arbitrary dimension, compute
# the projection along a single axis by taking the average over the other
# axes.  We do not know about bad pixels.

procedure im_projectiond (im, x, y, w, npix, weighting, axis)

pointer	im			# Pointer to image header structure
double	x[npix]			# Index of projection vector
double	y[npix]			# Receives the projection vector
double	w[npix]			# Receives the weight vector
int	weighting		# Weighting of the individual points
int	npix			# Length of projection vector
int	axis			# The axis to be projected to (x=1)

int	i, lastv
long	v[IM_MAXDIM], nsum, totpix
pointer	pix
double	asumd()
pointer	imgnld()
errchk	imgnld

begin
	if (im == NULL)
	    call error (1, "Image projection operator called with null im")
	if (axis < 1 || axis > IM_NDIM(im))
	    call error (2, "Attempt to take projection over nonexistent axis")


	# Set the y projection vector
	call aclrd (y, npix)
	call amovkl (long(1), v, IM_MAXDIM)

	switch (axis) {
	case 1:
	    # Since the image is read line by line, it is easy to compute the
	    # projection along the x-axis (axis 1).  We merely sum all of the
	    # image lines.

	    while (imgnld (im, pix, v) != EOF)
		call aaddd (Memd[pix], y, y, npix)

	default:
	    # Projecting along any other axis when reading the image line
	    # by line is a bit difficult to understand.  Basically, the
	    # element 'axis' of the V vector (position of the line in the
	    # image) gives us the index into the appropriate element of
	    # y.  When computing the projection over multiple dimensions,
	    # the same output element will be referenced repeatedly.  All
	    # of the elmenents of the input line are summed and added into
	    # this output element.

	    for (lastv=v[axis];  imgnld (im, pix, v) != EOF;  lastv=v[axis]) {
		i = lastv
		if (i <= npix)
		    y[i] = y[i] + asumd (Memd[pix], IM_LEN(im,1))
	    }
	}

	# Now compute the number of pixels contributing to each element
	# of the output vector.  This is the number of pixels in the image
	# divided by the length of the projection.

	totpix = 1
	do i = 1, IM_NDIM(im)
	    if (i == axis)
		totpix = totpix * min (npix, IM_LEN(im,i))
	    else
		totpix = totpix * IM_LEN(im,i)
	nsum = totpix / min (npix, IM_LEN(im,axis))

	# Compute the average by dividing by the number if pixels summed at
	# each point.
	call adivkd (y, double (nsum), y, npix)

	# Set the x and weight vectors
	do i = 1, npix {
	    x[i] = i
	    switch (weighting) {
	    case CF_STATISTICAL:
		if (y[i] > 0.0)
		    w[i] = 1.0 / y[i]
		else if (y[i] < 0.0)
		    w[i] = abs (1.0 / y[i])
		else
		    w[i] = 1.0
	    case CF_UNIFORM:
	        w[i] = 1.
	    default:
		w[i] = 1.
	    }
	}
end

