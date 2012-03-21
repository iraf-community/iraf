# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<math/curfit.h>
include	"names.h"

# IC_DEVIANT -- Find deviant points with large residuals from the fit
# and reject from the fit.
#
# The sigma of the fit residuals is calculated.  The rejection thresholds
# are set at +-reject*sigma.  Points outside the rejection threshold are
# recorded in the reject array.

procedure ic_deviantd (cv, x, y, w, rejpts, npts, low_reject, high_reject,
    grow, refit, nreject, newreject)

pointer	cv				# Curve descriptor
double	x[npts]				# Input ordinates
double	y[npts]				# Input data values
double	w[npts]				# Weights
int	rejpts[npts]			# Points rejected
int	npts				# Number of input points
real	low_reject, high_reject		# Rejection thresholds
real	grow				# Rejection radius
int	refit				# Refit the curve?
int	nreject				# Number of points rejected
int	newreject			# Number of new points rejected

int	i, j, i_min, i_max, pixgrow
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

	pixgrow = 0
        if (grow > 0.) {
	    do i = 1, npts-1 {
		if (abs (x[i+1] - x[i]) < 0.0001)
		    next
		if (i == 1)
		    pixgrow = grow / abs (x[i+1] - x[i])
		else
		    pixgrow = max (grow / abs (x[i+1] - x[i]), pixgrow)
	    }
	}

	newreject = 0
	for (i = 1; i <= npts; i = i + 1) {
	    if (w[i] == 0. || rejpts[i] == YES)
		next

	    residual = Memd[residuals + i - 1]
	    if (residual < high_cut && residual > low_cut)
		next

	    i_min = max (1, i - pixgrow)
	    i_max = min (npts, i + pixgrow)

	    # Reject points from the fit and flag them.
	    do j = i_min, i_max {
		if ((abs (x[i] - x[j]) <= grow) && (w[j] != 0.) &&
		    (rejpts[j] == NO)) {
		    if (refit == YES)
			call dcvrject (cv, x[j], y[j], w[j])
		    rejpts[j] = 2
		    newreject = newreject + 1
		}
	    }
	}
	do i = 1, npts
	    if (rejpts[i] != NO)
		rejpts[i] = YES

	nreject = nreject + newreject
	call sfree (sp)

	if ((refit == YES) && (newreject > 0)) {
	    call dcvsolve (cv, i)
	    switch (i) {
	    case SINGULAR:
		call error (1, "ic_reject: Singular solution")
	    case NO_DEG_FREEDOM:
		call error (2, "ic_reject: No degrees of freedom")
	    }
	}
end
