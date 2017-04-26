
# FIT_SMOOTH -- Least-squares fit of smoothed profiles to data profiles with
# cleaning of deviant pixels.
#
# The profile fitting and cleaning are combined in order to minimize
# the calculations in re-evaluating the least-squares fit after rejecting
# deviant pixels.
#
# The sigma used for rejection is calculated from the sigma of the fit
# before rejecting any pixels.  Pixels whose residuals exceed
# +/- sigma_cut * sigma are rejected.  The maximum number of pixels to be
# replaced in each spectrum is max_replace.  If max_replace is zero then
# only the model fitting is performed.
#
# The output of this routine are the cleaned data profiles and the least-square
# fitted model profiles.  The number of pixels replaced is returned.


procedure fit_smooth (line, data, model, profiles, len_prof, nspectra, nlines)

int	line					# Image line of data
real	data[len_prof, nspectra]		# Data profiles
real	model[len_prof, nspectra]		# Model profiles
real	profiles[len_prof, nspectra, ARB]	# Work array for SMOOTH profiles
int	len_prof				# Length of profile
int	nspectra				# Number of spectra
int	nlines					# Number of lines profiles

int	max_replace				# Maximum number of bad pixels
real	sigma_cut				# Sigma cutoff on the residuals

int	i, spectrum
int	nmax, ntotal, nreplace, nreject, nindef, nsigma
real	sum1, sum2, scale, sigma
real	lower, upper, residual, resid_min, resid_max
pointer	sp, a, b, c

begin
	# Allocate working memory.
	call smark (sp)
	call salloc (a, nspectra, TY_REAL)
	call salloc (b, nspectra, TY_REAL)
	call salloc (c, nspectra, TY_INT)

	# Fit each spectrum and compute sigma of fit.
	sigma = 0.
	nsigma = 0
	do spectrum = 1, nspectra {
	    # Accumulate least squares sums.
	    sum1 = 0.
	    sum2 = 0.
	    nindef = 0
	    do i = 1, len_prof {
		if (IS_INDEFR (data[i, spectrum]))
		    nindef = nindef + 1
	        else if (model[i, spectrum] > 0.) {
		    sum1 = sum1 + data[i, spectrum] * model[i, spectrum]
		    sum2 = sum2 + model[i, spectrum] * model[i, spectrum]
	        }
	    }

	    # Compute sigma if cleanning is desired.
	    if (nmax != 0) {
	        scale = sum1 / sum2
	        do i = 1, len_prof {
		    if (!IS_INDEFR (data[i, spectrum]) &&
			(model[i, spectrum] > 0.)) {
		        sigma = sigma +
		            (data[i,spectrum] - scale * model[i,spectrum]) ** 2
			nsigma = nsigma + 1
		    }
		}
	    }

	    Memr[a + spectrum - 1] = sum1
	    Memr[b + spectrum - 1] = sum2
	    Memi[c + spectrum - 1] = nindef
	}
	sigma = sqrt (sigma / nsigma)

	# Reject deviant pixels from the fit, scale the model to data,
	# and replace rejected and INDEFR pixels with model values.
	ntotal = 0
	do spectrum = 1, nspectra {
	    sum1 = Memr[a + spectrum - 1]
	    sum2 = Memr[b + spectrum - 1]
	    nindef = Memi[c + spectrum - 1]

	    # If there are no model data points go to the next spectrum.
	    if (sum2 == 0.)
		next

	    # Reject pixels if desired.
	    nreplace = 0
	    if (nmax != 0) {
	        # Compare each pixel in the profile against the model and set
	        # deviant pixels to INDEFR.  If the number of pixels to be
		# replaced is equal to the maximum allowed or the number of
		# pixels rejected equals the entire profile or the number of
		# deviant pixels is zero in an iteration stop cleaning and
		# exit the loop.  Ignore INDEFR pixels.

	        repeat {
		    nreject = 0
		    scale = sum1 / sum2
		    resid_min = -lower * sigma
		    resid_max = upper * sigma
	            do i = 1, len_prof {
		        if (IS_INDEFR (data[i, spectrum]))
			    next

		        # Compute the residual and remove point if it exceeds
			# the residual limits.

		        residual = data[i,spectrum] - scale * model[i,spectrum]
		        if ((residual < resid_min) || (residual > resid_max)) {
			    # Remove point from the least squares fit
			    # and flag the deviant pixel with INDEFR.
			    sum1 = sum1 - data[i,spectrum] * model[i,spectrum]
			    sum2 = sum2 - model[i,spectrum] * model[i,spectrum]
		            data[i,spectrum] = INDEFR
			    nreplace = nreplace + 1
			    nreject = nreject + 1
		        }
		        if (nreplace == nmax)
			    break
		    }
	        } until ((nreplace == nmax) || (nreject == 0) || (sum2 == 0.))
	    }

	    # If there are good pixels remaining scale the model to the
	    # data profile.
	    if (sum2 > 0.)
	        call amulkr (model[1, spectrum], sum1 / sum2,
		    model[1, spectrum], len_prof)

	    # Replace bad pixels by the model values.
	    if ((nindef > 0) || (nreplace > 0)) {
		do i = 1, len_prof {
		    if (IS_INDEFR (data[i, spectrum]))
			data[i, spectrum] = model[i, spectrum]
		}
		ntotal = ntotal + nreplace
	    }
	}

	# Print the number of pixel replaced.
	call ex_prnt3 (ntotal)

	# Replace the cleaned data profiles in future SMOOTH profiles.
	if (ntotal > 0)
	    call update_smooth (line, data, profiles, len_prof, nspectra,
		nlines)

	# Free allocated memory.
	call sfree (sp)

	return

# SET_FIT_SMOOTH -- Set the cleaning parameters.

entry set_fit_smooth (max_replace, sigma_cut)

	nmax = max_replace
	lower = sigma_cut
	upper = sigma_cut
	return
end
