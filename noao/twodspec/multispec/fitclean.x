include	"ms.h"

# FIT_AND_CLEAN -- Iteratively fit profile scales using banded matrix method
# and remove deviant pixels.
#
# The profile fitting and cleaning are combined in order to minimize
# the calculations in re-evaluating the least squares fit after rejecting
# deviant pixels.
#
# The sigma of the fit is calculated and deviant pixels are those whose
# residual is more than +-sigma_cut * sigma.
# The maximum number of pixels to be replaced is max_replace.
# If max_replace is zero then only the model fitting is performed.
#
# The output of this routine are the cleaned data profiles and the
# least-square fitted model profiles.  Return the number of pixels replaced.


procedure fit_and_clean (ms, data, model, ranges, profiles, len_line,
    len_profile, nspectra, nparams)

pointer	ms					# MULTISPEC data structure
real	data[len_line]				# Input data to be fit
real	model[len_line]				# Output model line
real	ranges[nspectra, LEN_RANGES, 3]		# Profile ranges
real	profiles[len_profile, nspectra, nparams, 3] # Model profiles
int	len_line				# Length of data/model line
int	len_profile				# Length of each profile
int	nspectra				# Number of spectra
int	nparams					# Number model parameters

int	max_iterate				# Maximum number of iterations
int	max_replace				# Maximum number of bad pixels
real	sigma_cut				# Rejection cutoff
int	fit_type				# Type of I0 fitting
bool	ex_model				# Extract model?

bool	exmod
int	i_max, nmax, option, npts
int	i, iteration, n_total, n_reject
real	sigma, lower, upper, residual, resid_min, resid_max

begin
	# Initialize the model and I0 parameters to zero.
	call aclrr (PARAMETER(ms,I0,1), nspectra)
	call aclrr (model, len_line)

	# Loop until no further deviant pixels are found.
	n_total = 0

	do iteration = 1, imax {
	    # Determine I0 for each profile.
	    switch (option) {
	    case 1:
	        call  full_solution (ms, data, model, ranges, profiles,
		    len_line, len_profile, nspectra, nparams)
	    case 2:
	        call quick_solution (ms, data, ranges, profiles, len_line,
		    len_profile, nspectra)
	    }

	    # Set the model to be used to compare against the data.
	    call set_model (ms, model, profiles, ranges, len_line, len_profile,
		nspectra)

	    # If number of pixels to reject is zero then skip below.
	    n_reject = 0
	    if (n_total == nmax)
		break

	    # Compute sigma of fit.
	    sigma = 0.
	    npts = 0
	    do i = 1, len_line {
		if ((model[i] > 0.) && (!IS_INDEFR (data[i]))) {
		    sigma = sigma + (data[i] - model[i]) ** 2
		    npts = npts + 1
		}
	    }
	    sigma = sqrt (sigma / npts)
	    resid_min = -lower * sigma
	    resid_max = upper * sigma

	    # Compare each pixel against the model and set deviant pixels
	    # to INDEFR.  If the number of pixels replaced is equal to the
	    # maximum allowed stop cleaning.  Ignore points with model <= 0.
	    # Thus, points outside the spectra will not be cleaned.
	    # Ignore INDEFR pixels.
	    do i = 1, len_line {
		if (n_total == nmax)
		    break
		if ((model[i] <= 0.) || (IS_INDEFR (data[i])))
		    next

		# Determine deviant pixels.
	        residual = data[i] - model[i]
	        if ((residual < resid_min) || (residual > resid_max)) {
		    # Flag deviant pixel.
		    data[i] = INDEFR
		    n_total = n_total + 1
		    n_reject = n_reject + 1
		}
	    }

	    if (n_reject == 0)
		break
	}
	# Refit model if a model extraction is desired and bad pixels were
	# in the last fit.
	if (exmod && n_reject != 0) {
	    switch (option) {
	    case 1:
	        call  full_solution (ms, data, model, ranges, profiles,
		    len_line, len_profile, nspectra, nparams)
	    case 2:
	        call quick_solution (ms, data, ranges, profiles, len_line,
		    len_profile, nspectra)
	    }
	}

	# Scale profiles to form model profiles.
	do i = 1, nspectra
	    call amulkr (profiles[1,i,I0_INDEX,1], PARAMETER(ms,I0,i),
		profiles[1,i,I0_INDEX,1], len_profile)

	# Replace deviant or INDEF pixels by model values.
	# Even if no cleaning was done there may have been some INDEF points
	# in the input data line.

	do i = 1, len_line {
	    if (IS_INDEFR (data[i]))
		data[i] = model[i]
	}

	# Print the number of pixels replaced and return.
	call ex_prnt3 (n_total)
	return

# SET_FIT_AND_CLEAN -- Set the fitting and cleaning parameters.

entry set_fit_and_clean (max_iterate, max_replace, sigma_cut, fit_type,
   ex_model)

	imax = max_iterate
	nmax = max_replace
	lower = sigma_cut
	upper = sigma_cut
	option = fit_type
	exmod = ex_model
	return
end


procedure full_solution (ms, data, model, ranges, profiles, len_line,
    len_profile, nspectra, nparams)

pointer	ms					# MULTISPEC data structure
real	data[len_line]				# Input data to be fit
real	model[len_line]				# Output model line
real	ranges[nspectra, LEN_RANGES, 3]		# Profile ranges
real	profiles[len_profile, nspectra, nparams, 3] # Model profiles
int	len_line				# Length of data/model line
int	len_profile				# Length of each profile
int	nspectra				# Number of spectra
int	nparams					# Number model parameters

real	rnorm
pointer	sp, fitparams, solution, offset

begin
	# Initialize fitparams and ranges arrays.
	call smark (sp)
	call salloc (fitparams, nspectra * nparams, TY_REAL)
	call salloc (solution, nspectra * nparams, TY_REAL)

	offset = (I0_INDEX - 1) * nspectra
	call amovki (NO, Memr[fitparams], nspectra * nparams)
	call amovki (YES, Memr[fitparams + offset], nspectra)

	# Do least squares banded matrix solution for I0 parameters.
	# The solution vector contains the least square fit values which
	# must be copied to the I0 parameter vector.
	call solve (ms, data, model, Memr[fitparams], profiles, ranges,
	    len_line, len_profile, nspectra, nparams, Memr[solution], rnorm)
	call aaddr (PARAMETER(ms, I0, 1), Memr[solution + offset],
	    PARAMETER(ms, I0, 1), nspectra)

	call sfree (sp)
end


# QUICK_SOLUTION -- Quick determination of profile scaling parameters.

procedure quick_solution (ms, data, ranges, profiles, len_line, len_profile,
    nspectra)

pointer	ms					# MULTISPEC data structure
real	data[len_line]				# Input data to be fit
real	ranges[nspectra, LEN_RANGES]		# Profile ranges
real	profiles[len_profile, nspectra, ARB]	# Model profiles
int	len_line				# Length of data/model line
int	len_profile				# Length of each profile
int	nspectra				# Number of spectra

int	i, ic, j, n, spectrum, xc
real	i0[3]

begin
	ic = len_profile / 2

	# Determine a value for I0 for each spectrum which is in the image.
	do spectrum = 1, nspectra {
	    n = 0

	    # Check each profile point from ic on until n = 2.
	    do i = ic, len_profile - 1 {
	        xc = ranges[spectrum, X_START] + i
	        if ((xc < 1) || (xc > len_line))
		    next
	        if (IS_INDEFR (data[xc]))
		    next
		j = i + 1
	        if (profiles[j, spectrum, I0_INDEX] <= 0)
		    next
		n = n + 1
	        i0[n] = data[xc] / profiles[j, spectrum, I0_INDEX]
		if (n >= 2)
		     break
	    }

	    # Check each profile point from ic - 1 and less until n = 3.
	    do i = ic - 1, 0, -1 {
	        xc = ranges[spectrum, X_START] + i
	        if ((xc < 1) || (xc > len_line))
		    next
	        if (IS_INDEFR (data[xc]))
		    next
		j = i + 1
	        if (profiles[j, spectrum, I0_INDEX] <= 0)
		    next
		n = n + 1
	        i0[n] = data[xc] / profiles[j, spectrum, I0_INDEX]
		break
	    }

	    # Determine I0.
	    switch (n) {
	    case 3:				# Use median I0
		call asrtr (i0, i0, n)
	        PARAMETER(ms, I0, spectrum) = i0[2]
	    case 2:				# Use mean I0
	        PARAMETER(ms, I0, spectrum) = (i0[1] + i0[2]) / 2
	    case 1:				# Use only value
	        PARAMETER(ms, I0, spectrum) = i0[1]
	    }
	}
end
