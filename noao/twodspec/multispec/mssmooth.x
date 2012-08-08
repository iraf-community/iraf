include	<math/curfit.h>

# MS_SMOOTH -- Smooth MULTISPEC parameters with the CURFIT package.
# MS_SET_SMOOTH -- Initialize and define function for smoothing.
# MS_FREE_SMOOTH -- Free allocated memory from smoothing.

# This procedure is numerical and does not depend on the MULTISPEC
# package.

procedure ms_smooth (x, y)

real	x[ARB]				# Array of x values
real	y[ARB]				# Array of y values
int	curve_type			# Curfit function
int	order				# Order of function
real	xmin				# Minimum x value
real	xmax				# Maximum x value
int	npoints				# Number of points in fits

int	i, npts, ier
real	xmn, xmx
pointer	cv, w

real	cveval()

data	cv/NULL/, w/NULL/

begin
	# Check for a valid curfit pointer.
	if (cv == NULL)
	    call error (0, "param_smooth: Undefined smoothing function")
	
	# Zero and fit the data with uniform weights.
	call cvzero (cv)
	# call cvfit (cv, x, y, Memr[w], npts, WTS_UNIFORM, ier)

	# Accumulate points and check for out of bounds points.
	do i = 1, npts
	    if ((x[i] >= xmn) && (x[i] <= xmx))
		call cvaccum (cv, x[i], y[i], Memr[w+i-1], WTS_UNIFORM)
	call cvsolve (cv, ier)

	if (ier != OK)
	    call error (0, "param_smooth: Error in function fit")

	# Evaluate fit placing fit values back in y array.
	# call cvvector (cv, x, y, npts)
	do i = 1, npts
	    if ((x[i] >= xmn) && (x[i] <= xmx))
		y[i] = cveval (cv, x[i])

	return

entry ms_set_smooth (xmin, xmax, npoints)

	# Set or reset curfit data structure and allocate memory for weights.
	if (cv != NULL)
	    call cvfree (cv)
	if (w == NULL)
	    call malloc (w, npoints, TY_REAL)

	# Determine curve_type and order.
	call clgcurfit ("function", "order", curve_type, order)

	# Initialize curfit data structure and record number of points.
	xmn = xmin
	xmx = xmax
	call cvinit (cv, curve_type, order, xmn, xmx)
	npts = npoints

	return

entry ms_free_smooth ()

	# Free allocated memory.
	if (cv != NULL)
	    call cvfree (cv)
	if (w != NULL)
	    call mfree (w, TY_REAL)

end
