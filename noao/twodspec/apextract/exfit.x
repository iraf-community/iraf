include	"extract.h"

# EX_FIT -- Least-squares fit of smoothed profiles to data profiles with
# cleaning of deviant pixels.
#
# The profile fitting and cleaning are combined in order to minimize
# the calculations in re-evaluating the least-squares fit after rejecting
# deviant pixels.
#
# The sigma used for rejection is calculated from the sigma of the fit
# before rejecting any pixels.  Pixels whose residuals exceed
# (EX_LSIGMA(ex), EX_USIGMA(ex)) * sigma are rejected.  The maximum number
# of pixels to be replaced in each spectrum is EX_NCLEAN(ex).
#
# The output of this routine are the cleaned data profiles and the least-square
# fitted model profiles.  The number of pixels replaced is returned.


procedure ex_fit (ex, line, pstart, pend, pnrep, naps, data, bckgrnd, model)

pointer	ex				# Pointer to extraction parameters
int	line				# Image line of data
int	pstart[naps]			# Profile starting index
int	pend[naps]			# Profile ending index
int	pnrep[naps]			# Number of pixels rejected
int	naps				# Number of apertures
real	data[ARB]			# Data profiles
real	bckgrnd[ARB]			# Background profiles
real	model[ARB]			# Model profiles

int	i, j, ps, pe
int	nclean, ntotal, nreplace, nreject, nindef, nsigma
real	sum1, sum2, scale, sigma
real	v0, v1, var, wt, residual, resid_min, resid_max
bool	wtflag, cleanflag
pointer	sp, a, b, c, d

begin
	call smark (sp)
	call salloc (a, naps, TY_REAL)
	call salloc (b, naps, TY_REAL)
	call salloc (c, naps, TY_INT)
	call salloc (d, naps, TY_REAL)

	if ((EX_WTTYPE(ex) == VARIANCE) && (EX_V1(ex) != 0.)) {
	    wtflag = true
	    v0 = EX_V0(ex)
	    v1 = EX_V1(ex)
	} else
	    wtflag = false

	if (EX_CLEAN(ex) == YES) {
	    cleanflag = true
	    nclean = EX_NCLEAN(ex)
	} else
	    cleanflag = false

	# Fit each aperture profile and compute sigma of fit.
	sigma = 0.
	nsigma = 0

	do j = 1, naps {
	    if (pstart[j] == 0)
		next
	    ps = pstart[j] + 1
	    pe = pend[j] - 1

	    # Accumulate least squares sums.

	    sum1 = 0.
	    sum2 = 0.
	    nindef = 0
	    do i = ps, pe {
		if (IS_INDEF (data[i]))
		    nindef = nindef + 1
		else {
		    sum1 = sum1 + data[i] * model[i]
		    sum2 = sum2 + model[i] * model[i]
	        }
	    }

	    # Go on to next spectrum if the model is identically zero.
	    Memr[a+j-1] = sum1
	    Memr[b+j-1] = sum2
	    Memi[c+j-1] = nindef
	    if (sum2 == 0.)
		next
	    scale = sum1 / sum2
	    Memr[d+j-1] = scale

	    # Apply variance weights.
	    if (wtflag) {
		sum1 = 0.
		sum2 = 0.
		do i = ps, pe {
		    if (model[i] > 0. && !IS_INDEF (data[i])) {
			if (v0 == 0.)
			    var = v1 * max (1., scale * model[i] + bckgrnd[i])
			else
			    var = v0 + v1 *
			        max (0., scale * model[i] + bckgrnd[i])
			wt = 1. / var
			sum1 = sum1 + wt * data[i] * model[i]
			sum2 = sum2 + wt * model[i] * model[i]
		    }
		}
	        Memr[a+j-1] = sum1
	        Memr[b+j-1] = sum2
		if (sum2 == 0.)
		    next
		scale = sum1 / sum2
		Memr[d+j-1] = scale
	    }

	    # If cleaning compute sigma.
	    if (cleanflag) {
	        if (wtflag) {
	            do i = ps, pe {
		        if (!IS_INDEF(data[i])) {
			    if (v0 == 0.)
			        var = v1 *
				    max (1., scale * model[i] + bckgrnd[i])
			    else
			        var = v0 + v1 *
			            max (0., scale * model[i] + bckgrnd[i])
			    wt = 1. / var
		            sigma = sigma +
				wt * (data[i] - scale * model[i]) ** 2
			    nsigma = nsigma + 1
			}
		    }
		} else {
	            do i = ps, pe {
		        if (!IS_INDEF(data[i])) {
		            sigma = sigma + (data[i] - scale * model[i]) ** 2
			    nsigma = nsigma + 1
			}
		    }
		}
	    }
	}

	if (nsigma > 0)
	    sigma = sqrt (sigma / nsigma)

	# Reject deviant pixels from the fit, scale the model to data,
	# and replace rejected and INDEF pixels with model values.

	ntotal = 0
	do j = 1, naps {
	    if (pstart[j] == 0)
		next
	    ps = pstart[j] + 1
	    pe = pend[j] - 1

	    sum1 = Memr[a+j-1]
	    sum2 = Memr[b+j-1]
	    scale = Memr[d+j-1]
	    nindef = Memi[c+j-1]

	    # If there are no model data points go to the next spectrum.
	    if (sum2 == 0.)
		next

	    # Reject pixels if desired.
	    nreplace = 0
	    if (cleanflag) {
	        # Compare each pixel in the profile against the model and set
	        # deviant pixels to INDEF.  If the number of pixels to be
		# replaced is equal to the maximum allowed or the number of
		# pixels rejected equals the entire profile or the number of
		# deviant pixels is zero in an iteration stop cleaning and
		# exit the loop.  Ignore INDEF pixels.

	        repeat {
		    nreject = 0
		    resid_min = -EX_LSIGMA(ex) * sigma
		    resid_max = EX_USIGMA(ex) * sigma
	            do i = ps, pe {
		        if (IS_INDEF (data[i]))
			    next

		        # Compute the residual and remove point if it exceeds
			# the residual limits.
			if (wtflag) {
			    if (v0 == 0.)
				var = v1 *
				    max (1., scale * model[i] + bckgrnd[i])
			    else
				var = v0 + v1 *
				    max (0., scale * model[i] + bckgrnd[i])
			    wt = 1. / var
		            residual = sqrt (wt) * (data[i] - scale * model[i])
			} else
		            residual = data[i] - scale * model[i]

		        if ((residual < resid_min) || (residual > resid_max)) {
			    # Remove point from the least squares fit
			    # and flag the deviant pixel with INDEF.
			    if (wtflag && model[i] > 0.) {
			        sum1 = sum1 - wt * data[i] * model[i]
			        sum2 = sum2 - wt * model[i] * model[i]
			    } else {
			        sum1 = sum1 - data[i] * model[i]
			        sum2 = sum2 - model[i] * model[i]
			    }
			    if (sum2 != 0.)
				scale = sum1 / sum2
		            data[i] = INDEF
			    if (nsigma > 1) {
			        sigma = sigma ** 2 * nsigma
		                sigma = sigma - residual ** 2
			        nsigma = nsigma - 1
				if (sigma < 0.0)
				    sigma = 0.0
				else
			            sigma = sqrt (sigma / nsigma)
			    }

			    nreplace = nreplace + 1
			    nreject = nreject + 1
		        }
		        if (nreplace == nclean)
			    break
		    }
	        } until ((nreplace == nclean) || (nreject == 0) ||
		    (sum2 == 0.))
	    }

	    # If there are good pixels remaining scale the model to the
	    # data profile.
	    if ((sum1 != 0.) && (sum2 != 0.))
	        call amulkr (model[pstart[j]], sum1 / sum2, model[pstart[j]],
		    pend[j] - pstart[j] + 1)

	    # Replace bad pixels by the model values.
	    pnrep[j] = nreplace
	    if ((nindef > 0) || (nreplace > 0)) {
		do i = ps, pe {
		    if (IS_INDEF (data[i]))
			data[i] = model[i]
		}
		ntotal = ntotal + nreplace
	    }
	}

	call sfree (sp)
end
