# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# IC_REJECT -- Reject points with large residuals from the fit.
#
# The sigma of the fit residuals is calculated.  The rejection thresholds
# are set at low_reject*sigma and high_reject*sigma.  Points outside the
# rejection threshold are rejected from the fit and flagged in the rejpts
# array.  Finally, the remaining points are refit.

procedure ic_rejectr (cv, x, y, w, rejpts, npts, low_reject, high_reject,
    niterate, grow, nreject)

pointer	cv				# Curve descriptor
real	x[npts]				# Input ordinates
real	y[npts]				# Input data values
real	w[npts]				# Weights
int	rejpts[npts]			# Points rejected
int	npts				# Number of input points
real	low_reject, high_reject		# Rejection threshold
int	niterate			# Number of rejection iterations
real	grow				# Rejection radius
int	nreject				# Number of points rejected

int	i, newreject

begin
	# Initialize rejection.

	nreject = 0
	call amovki (NO, rejpts, npts)

	if (niterate <= 0)
	    return

	# Find deviant points.

	do i = 1, niterate {
	    call ic_deviantr (cv, x, y, w, rejpts, npts, low_reject,
		high_reject, grow, YES, nreject, newreject)
	    if (newreject == 0)
		break
	}
end
