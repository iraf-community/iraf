# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# PEAKS -- find the peaks in an array of x and y points.
# The extrema in the input data points are found using extrema(xtools).
# The extrema are located to a precision of dx.
# The extrema with negative curvature (peaks) are selected and returned
# in the x array.  The spline value is returned in the y array.  The
# background is estimated by linear interpolation of the neighboring
# minima (extrema of positive curvature) to the position of the peak.
# The background is returned in the background array.  The number of
# peaks found is returned as the function value.

int procedure peaks (x, y, background, npts, dx)

real	x[npts], y[npts]	# Input data points and output peaks
real	background[npts]	# Background estimate
int	npts			# Number of input data points
real	dx			# Precision of peak positions

int	i, j, k, npeaks, nextrema
pointer	sp, a, b, c

int	extrema()
errchk	salloc

begin
	nextrema = extrema (x, y, background, npts, dx)

	if (nextrema == 0)
	    return (0)

	# Allocate working storage
	call smark (sp)
	call salloc (a, nextrema, TY_REAL)
	call salloc (b, nextrema, TY_REAL)
	call salloc (c, nextrema, TY_REAL)

	npeaks = 0
	do i = 1, nextrema {
	    if (background[i] < 0.) {
		Memr[a + npeaks] = x[i]
		Memr[b + npeaks] = y[i]
		for (j = i - 1; j > 0; j = j - 1)
		    if (background[j] > 0.)
			break;
		for (k = i + 1; k <= npts; k = k + 1)
		    if (background[k] > 0.)
			break;
		if ((j >= 1) && (k <= npts))
		    Memr[c + npeaks] =
			(y[k] - y[j]) / (x[k] - x[j]) * (x[i] - x[j]) + y[j]
		else if (j >= 1)
		    Memr[c + npeaks] = y[j]
		else if (k <= npts)
		    Memr[c + npeaks] = y[k]
		else {
		    call sfree (sp)
		    call error (1, "No background points")
		}
		npeaks = npeaks + 1
	    }
	}

	call amovr (Memr[a], x, npeaks)
	call amovr (Memr[b], y, npeaks)
	call amovr (Memr[c], background, npeaks)

	call sfree (sp)
	return (npeaks)
end
