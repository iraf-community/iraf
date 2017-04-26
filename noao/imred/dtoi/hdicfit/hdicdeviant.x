include	<mach.h>
include	<math/curfit.h>

# IC_DEVIANT -- Find deviant points with large residuals from the fit
# and reject from the fit.
#
# The sigma of the fit residuals is calculated.  The rejection thresholds
# are set at +-reject*sigma.  Points outside the rejection threshold are
# recorded in the reject array.

procedure ic_deviantd (cv, x, y, w, rejpts, npts, low_reject, high_reject,
	grow, refit, nreject, newreject)

pointer	cv				# Curve descriptor
double	x[ARB]				# Input ordinates
double	y[ARB]				# Input data values
double	w[ARB]				# Weights
int	rejpts[ARB]			# Points rejected
int	npts				# Number of input points
real	low_reject, high_reject		# Rejection thresholds
real	grow				# Rejection radius
int	refit				# Refit the curve?
int	nreject				# Number of points rejected
int	newreject			# Number of new points rejected

int	i, j, i_min, i_max, ilast
double	sigma, low_cut, high_cut, residual
pointer	sp, residuals

begin
	# If low_reject and high_reject are zero then simply return.
	if ((low_reject == 0.) && (high_reject == 0.))
	    return

	# Allocate memory for the residuals.
	call smark (sp)
	call salloc (residuals, npts, TY_DOUBLE)

	# Compute the residuals.
	call dcvvector (cv, x, Memd[residuals], npts)
	call asubd (y, Memd[residuals], Memd[residuals], npts)

	# Compute the sigma of the residuals.  If there are less than
	# 5 points return.

	j = 0
	nreject = 0
	sigma = 0.

	do i = 1, npts {
	    if ((w[i] != 0.) && (rejpts[i] == NO)) {
		sigma = sigma + Memd[residuals+i-1] ** 2
		j = j + 1
	    } else if (rejpts[i] == YES)
		nreject = nreject + 1
	}

	if (j < 5) {
	    call sfree (sp)
	    return
	} else
	    sigma = sqrt (sigma / j)

	if (low_reject > 0.)
	    low_cut = -low_reject * sigma
	else
	    low_cut = -MAX_REAL
	if (high_reject > 0.)
	    high_cut = high_reject * sigma
	else
	    high_cut = MAX_REAL

	# Reject the residuals exceeding the rejection limits.
	# A for loop is used instead of do because with region growing we
	# want to modify the loop index.

	newreject = 0
	for (i = 1; i <= npts; i = i + 1) {
	    if ((w[i] == 0.) || (rejpts[i] == YES))
		next

	    residual = Memd[residuals + i - 1]
	    if ((residual > high_cut) || (residual < low_cut)) {
		i_min = max (1, int (i - grow))
		i_max = min (npts, int (i + grow))

		# Reject points from the fit and flag them.
		do j = i_min, i_max {
		    if ((abs (x[i] - x[j]) <= grow) && (w[j] != 0.) &&
		        (rejpts[j] == NO)) {
			if (refit == YES)
	        	    call dcvrject (cv, x[j], y[j], w[j])
			rejpts[j] = YES
		        newreject = newreject + 1
			ilast = j
		    }
		}
		i = ilast
	    }
	}

	if ((refit == YES) && (newreject > 0)) {
	    call dcvsolve (cv, i)

	    switch (i) {
	    case SINGULAR:
		call eprintf ("ic_reject: Singular solution\n")
	    case NO_DEG_FREEDOM:
		call eprintf ("ic_reject: No degrees of freedom\n")
	    }
	}

	nreject = nreject + newreject

	call sfree (sp)
end
