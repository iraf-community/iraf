include	<pkg/inlfit.h>


# IN_REJECT -- Reject points with large residuals from the fit.
# The sigma of the fit residuals is calculated.  The rejection thresholds
# are set at low_reject*sigma and high_reject*sigma.  Points outside the
# rejection threshold are rejected from the fit and flagged in the rejpts
# array.  Finally, the remaining points are refit.

procedure in_rejectr (in, nl, x, y, w, npts, nvars, wtflag)

pointer	in				# INLFIT decriptor
pointer	nl				# NLFIT decriptor
real	x[ARB]				# Input ordinates (npts * nvars)
real	y[npts]				# Input data values
real	w[npts]				# Weights
int	npts				# Number of input points
int	nvars				# Number of variables
int	wtflag				# Type of weighting

int	i, nreject, newreject, niter
real	low, high, grow
pointer	sp, wts1, rejpts

int	in_geti()
real	in_getr()
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

	call smark (sp)
	call salloc (wts1, npts, TY_REAL)
	call amovr (w, Memr[wts1], npts)

	# Get rejection parameters, and rejected point list.
	low    = in_getr (in, INLLOW)
	high   = in_getr (in, INLHIGH)
	grow   = in_getr (in, INLGROW)
	rejpts = in_getp  (in, INLREJPTS)

	# Loop looking for deviant points, and refitting.
	do i = 1, niter {

	    # Look for new deviant points.
	    call in_deviantr (nl, x, y, w, Memi[rejpts], npts, nvars, low,
	        high, grow, nreject, newreject)

	    # Refit if there are new rejected points.
	    if (newreject != 0) {
	        call amovr (Memr[wts1], w, npts)
		call in_refitr (in, nl, x, y, w, npts, nvars, wtflag)
	    } else
		break
	}

	# Update number of rejected points.
	call in_puti (in, INLNREJPTS, nreject + newreject)

	call sfree (sp)
end
