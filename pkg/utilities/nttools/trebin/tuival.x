include "trebin.h"

# tuival -- interpolate
# Interpolate to get one value.
# A zero value of iv_step means that the output independent-variable values
# may not be uniformly spaced, so the interval x1,x2 must be gotten from
# the xout array.  (This is relevant for linear interpolation only.)
#
# Phil Hodge, 18-Apr-1988  Subroutine created
# Phil Hodge, 20-May-1996  Include extrapolate and ext_value for extrapolation.
# Phil Hodge, 29-Jul-1998  For 1-D, fit a line instead of interpolating,
#			add iv_step to calling sequence, add subroutines
#			tu_range and tu_fit1.
# Phil Hodge, 22-Apr-1999  The test on whether x is outside the range of
#		the data did not consider that xa could be decreasing.
# Phil Hodge, 25-Apr-2000  Change calling sequence:  x --> xout, j, outrows;
#		this is to support the option of a nonuniformly spaced output
#		independent variable array.

procedure tuival (i_func, xa, ya, y2, n,
	extrapolate, ext_value, xout, j, outrows, iv_step, y)

int	i_func			# i: interpolation function code
double	xa[n]			# i: input independent-variable values
double	ya[n]			# i: input dependent-variable values
double	y2[n]			# i: used only by spline interpolation
int	n			# i: size of xa, ya, y2 arrays
bool	extrapolate		# i: extrapolate rather than use ext_value?
double	ext_value		# i: value to assign if x is out of bounds
double	xout[ARB]		# i: output independent variable array
int	j			# i: current element of xout
int	outrows			# i: size of xout array
double	iv_step			# i: spacing of x values (can be negative)
double	y			# o: the interpolated value
#--
double	x			# independent variable
double	x1, x2			# beginning and end of output pixel
int	klo, khi		# indexes within xa that bracket x
int	npts			# number of points in interval

begin
	x = xout[j]

	# Initial guess for klo; we want xa[klo] <= x <= xa[klo+1].
	klo = (n + 1) / 2
	khi = klo

	# If x falls outside the range of the data, either extrapolate
	# (below) or assign a fixed value ext_value.
	if (!extrapolate && n > 1) {
	    if (xa[1] < xa[2] && (x < xa[1] || x > xa[n])) {	# increasing
		y = ext_value
		return
	    }
	    if (xa[1] > xa[2] && (x > xa[1] || x < xa[n])) {	# decreasing
		y = ext_value
		return
	    }
	}

	switch (i_func) {
	case I_NEAREST:

	    # Find klo such that xa[klo] <= x <= xa[klo+1], starting from
	    # current klo.
	    call tuhunt (xa, n, x, klo)
	    klo = max (klo, 1)
	    klo = min (klo, n)
	    khi = min (klo+1, n)
	    if (abs (x - xa[klo]) < abs (x - xa[khi]))
		y = ya[klo]
	    else
		y = ya[khi]

	case I_LINEAR:

	    # Get x1 and x2.
	    call tu_x1x2 (xout, j, outrows, iv_step, x1, x2)

	    # Get klo, khi, and npts.
	    call tu_range (xa, ya, n, x1, x2, klo, khi, npts)
	    if (npts > 1) {
		# Fit a line to the data from klo to khi inclusive.
		call tu_fit1 (xa, ya, x, klo, khi, y)
	    } else {
		# Interpolate.
		klo = klo - 1
		call tuhunt (xa, n, x, klo)
		klo = max (klo, 1)
		klo = min (klo, n-1)
		call tuiep1 (xa[klo], ya[klo], x, y)
	    }

	case I_POLY3:

	    call tuhunt (xa, n, x, klo)
	    klo = max (klo, 2)
	    klo = min (klo, n-2)
	    # Pass tuiepn the four points at klo-1, klo, klo+1, klo+2.
	    call tuiepn (4, xa[klo-1], ya[klo-1], x, y)

	case I_SPLINE:

	    call tuhunt (xa, n, x, klo)
	    klo = max (klo, 1)
	    klo = min (klo, n-1)
	    call tuispl (xa[klo], ya[klo], y2[klo], x, y)
	}
end

# This routine gets the endpoints x1,x2 of the current output pixel.
# If the output independent variable array is uniformly spaced, x1 and x2
# will just be the current pixel xout[j] - or + iv_step/2.  If the output
# independent variable array is (or may be) nonuniformly spaced, indicated
# by iv_step being zero, then x1 and x2 will be the midpoints of intervals
# adjacent to xout[j] on the left and right.
#
# x1 may be less than, equal to, or greater than x2.

procedure tu_x1x2 (xout, j, outrows, iv_step, x1, x2)

double	xout[ARB]	# i: output independent variable array
int	j		# i: current element of xout
int	outrows		# i: size of xout array
double	iv_step		# i: spacing of x values (can be negative)
double	x1, x2		# o: endpoints of output pixel

begin
	if (iv_step != 0) {
	    # output is uniformly spaced
	    x1 = xout[j] - iv_step / 2.d0
	    x2 = xout[j] + iv_step / 2.d0
	} else {
	    if (outrows == 1) {
		x1 = xout[1]
		x2 = xout[1]
	    } else if (j == 1) {
		x2 = (xout[1] + xout[2]) / 2.d0
		x1 = xout[1] - (x2 - xout[1])
	    } else if (j == outrows) {
		x1 = (xout[outrows-1] + xout[outrows]) / 2.d0
		x2 = xout[outrows] + (xout[outrows] - x1)
	    } else {
		x1 = (xout[j-1] + xout[j]) / 2.d0
		x2 = (xout[j] + xout[j+1]) / 2.d0
	    }
	}
end

# tu_range -- find range of indexes
# This routine finds the range of indexes in xa and ya corresponding to
# the interval x1 to x2.  The range is klo to khi; klo will be less than
# or equal to khi, even though xa can be either increasing or decreasing.
# klo and khi will also be within the range 1 to n inclusive, even if
# x1 to x2 is outside the range xa[1] to xa[n].  npts will be set to
# the actual number of elements of xa within the interval x1 to x2;
# thus, npts can be zero.
#
# x1 and x2 will be interchanged, if necessary, so that the array index
# in xa corresponding to x1 will be smaller than the array index corresponding
# to x2, i.e. so klo will be smaller than khi.
#
# Note that klo is used as an initial value when hunting for a value in xa,
# so klo must have been initialized to a reasonable value before calling
# this routine.

procedure tu_range (xa, ya, n, x1, x2, klo, khi, npts)

double	xa[n]		# i: array of independent-variable values
double	ya[n]		# i: array of dependent-variable values
int	n		# i: size of xa, ya, y2 arrays
double	x1, x2		# io: endpoints of output pixel
int	klo, khi	# io: range of indices in xa within x1,x2
int	npts		# o: number of elements in xa within x1,x2
#--
double	temp		# for swapping x1 and x2, if appropriate
int	k1, k2

begin
	# Swap x1 and x2 if necessary, so that klo will be less than or
	# equal to khi.
	if (xa[1] <= xa[2]) {	# input X is increasing
	    if (x1 > x2) {
		temp = x1; x1 = x2; x2 = temp
	    }
	} else {		# input X is decreasing
	    if (x1 < x2) {
		temp = x1; x1 = x2; x2 = temp
	    }
	}

	k1 = klo
	call tuhunt (xa, n, x1, k1)
	k1 = k1 + 1			# next point
	klo = k1
	klo = min (klo, n)

	k2 = k1
	call tuhunt (xa, n, x2, k2)
	khi = max (k2, 1)
	khi = min (khi, n)

	# npts can be different from khi - klo + 1, and it can be zero.
	npts = k2 - k1 + 1
end

# tu_fit1 -- fit a line (1-D) to data
# This routine fits a straight line to (xa[k],ya[k]) for k = klo to khi
# inclusive.  The fit is then evaluated at x, and the result is returned
# as y.  There must be at least two points, i.e. khi > klo.

procedure tu_fit1 (xa, ya, x, klo, khi, y)

double	xa[ARB]		# i: array of independent-variable values
double	ya[ARB]		# i: array of dependent-variable values
double	x		# i: independent variable
int	klo, khi	# i: range of indices in xa covered by iv_step
double	y		# o: value of fit at x
#--
double	sumx, sumy, sumxy, sumx2
double	xmean, ymean
double	slope, intercept
int	k

begin
	sumx = 0.d0
	sumy = 0.d0
	do k = klo, khi {
	    sumx = sumx + xa[k]
	    sumy = sumy + ya[k]
	}
	xmean = sumx / (khi - klo + 1)
	ymean = sumy / (khi - klo + 1)

	sumxy = 0.d0
	sumx2 = 0.d0
	do k = klo, khi {
	    sumxy = sumxy + (xa[k] - xmean) * (ya[k] - ymean)
	    sumx2 = sumx2 + (xa[k] - xmean)**2
	}
	slope = sumxy / sumx2
	intercept = (ymean - slope * xmean)

	y = intercept + slope * x
end


# tuiep1 -- linear interpolation
# Interpolate to get one value.  X is supposed to be between xa[1] and
# xa[2], but it is not an error if x is outside that interval.  Note
# that xa, ya are subarrays of the arrays with the same names elsewhere.

procedure tuiep1 (xa, ya, x, y)

double	xa[2]			# i: pair of independent-variable values
double	ya[2]			# i: pair of dependent-variable values
double	x			# i: independent variable
double	y			# o: the interpolated value
#--
double	p			# fraction of distance between indep var val

begin
	if (x == xa[1]) {
	    y = ya[1]
	} else if (x == xa[2]) {
	    y = ya[2]
	} else {
	    p = (x - xa[1]) / (xa[2] - xa[1])
	    y = p * ya[2] + (1.-p) * ya[1]
	}
end
