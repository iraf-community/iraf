include	<mach.h>


# IN_DEVIANT -- Find deviant points with large residuals from the fit
# and reject them from the fit.  The sigma of the fit residuals is calculated.
# The rejection thresholds are set at (+/-)reject*sigma.  Points outside the
# rejection threshold are recorded in the reject array.

procedure in_deviantr (nl, x, y, w, rejpts, npts, nvars, low_reject,
	high_reject, grow, nreject, newreject)

pointer	nl				# NLFIT descriptor
real	x[ARB]				# Input ordinates (npts * nvars)
real	y[npts]				# Input data values
real	w[npts]				# Weights
int	rejpts[npts]			# Points rejected
int	npts				# Number of input points
int	nvars				# Number of input variables
real	low_reject, high_reject		# Rejection thresholds
real	grow				# Rejection radius
int	nreject				# Number of points rejected (output)
int	newreject			# Number of new points rej. (output)

int	i, j, i_min, i_max, ilast
real	sigma, low_cut, high_cut, residual
pointer	sp, residuals

begin
#	# Debug.
#	call eprintf (
#	    "in_deviant: nl=%d, npts=%d, nvars=%d, low=%g, high=%g, grow=%g\n")
#	    call pargi (nl)
#	    call pargi (npts)
#	    call pargi (nvars)
#	    call parg$t (low_reject)
#	    call parg$t (high_reject)
#	    call parg$t (grow)

	# Initialize.
	nreject = 0
	newreject = 0

	# If low_reject and high_reject are zero then just return.
	if ((low_reject == real (0.0)) && (high_reject == real (0.0)))
	    return

	# Allocate memory for the residuals.
	call smark (sp)
	call salloc (residuals, npts, TY_REAL)

	# Compute the residuals.
	call nlvectorr (nl, x, Memr[residuals], npts, nvars)
	call asubr (y, Memr[residuals], Memr[residuals], npts)

	# Compute the sigma of the residuals.
	j = 0
	sigma = real (0.0)
	do i = 1, npts {
	    if ((w[i] != real (0.0)) && (rejpts[i] == NO)) {
		sigma = sigma + Memr[residuals+i-1] ** 2
		j = j + 1
	    } else if (rejpts[i] == YES)
		nreject = nreject + 1
	}

	# If there are less than five points for the sigma calculation,
	# just return.

	if (j < 5) {
	    call sfree (sp)
	    return
	} else
	    sigma = sqrt (sigma / j)

	# Set the lower and upper cut limits according the the sigma value.

	if (low_reject > real (0.0))
	    low_cut = -low_reject * sigma
	else
	    low_cut = -MAX_REAL
	if (high_reject > real (0.0))
	    high_cut = high_reject * sigma
	else
	    high_cut = MAX_REAL

	# Reject the residuals exceeding the rejection limits.
	# A for loop is used instead of do because with region
	# growing we want to modify the loop index.

	for (i = 1; i <= npts; i = i + 1) {

	    # Do not process points with zero weigth or already rejected.
	    if ((w[i] == real (0.0)) || (rejpts[i] == YES))
		next

	    # Reject point, and all other points closer than the growing
	    # factor.

	    residual = Memr[residuals + i - 1]
	    if ((residual > high_cut) || (residual < low_cut)) {

		# Determine region to reject.
		i_min = max (1, int (i - grow))
		i_max = min (npts, int (i + grow))

		# Reject points from the fit and flag them.
		do j = i_min, i_max {
		    if ((abs (x[i] - x[j]) <= grow) && (w[j] != real (0.0)) &&
		        (rejpts[j] == NO)) {
			rejpts[j] = YES
		        newreject = newreject + 1
			ilast = j
		    }
		}
		i = ilast
	    }
	}

	# Free memory.
	call sfree (sp)
end
