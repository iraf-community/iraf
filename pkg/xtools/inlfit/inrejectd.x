include	<pkg/inlfit.h>


# IN_REJECT -- Reject points with large residuals from the fit.
# The sigma of the fit residuals is calculated.  The rejection thresholds
# are set at low_reject*sigma and high_reject*sigma.  Points outside the
# rejection threshold are rejected from the fit and flagged in the rejpts
# array.  Finally, the remaining points are refit.

procedure in_rejectd (in, nl, x, y, w, npts, nvars, wtflag)

pointer	in				# INLFIT decriptor
pointer	nl				# NLFIT decriptor
double	x[ARB]				# Input ordinates (npts * nvars)
double	y[npts]				# Input data values
double	w[npts]				# Weights
int	npts				# Number of input points
int	nvars				# Number of variables
int	wtflag				# Type of weighting

int	i, nreject, newreject, niter
double	low, high, grow
pointer	rejpts

int	in_geti()
double	in_getd()
pointer	in_getp()

begin
#	# Debug.
#	call eprintf ("in_reject: in=%d, nl=%d, npts=%d, nvars=%d\n")
#	    call pargi (in)
#	    call pargi (nl)
#	    call pargi (npts)
#	    call pargi (nvars)

	# Get number of reject iterations, and return if they
	# are less than one.
	niter = in_geti (in, INLNREJECT)
	if (niter < 1)
	    return

	# Get rejection parameters, and rejected point list.
	low    = in_getd (in, INLLOW)
	high   = in_getd (in, INLHIGH)
	grow   = in_getd (in, INLGROW)
	rejpts = in_getp  (in, INLREJPTS)

	# Loop looking for deviant points, and refitting.
	do i = 1, niter {

	    # Look for new deviant points.
	    call in_deviantd (nl, x, y, w, Memi[rejpts], npts, nvars, low,
	        high, grow, nreject, newreject)

	    # Refit if there are new rejected points.
	    if (newreject != 0)
		call in_refitd (in, nl, x, y, w, npts, nvars, wtflag)
	    else
		break
	}

	# Update number of rejected points.
	call in_puti (in, INLNREJPTS, nreject + newreject)
end
