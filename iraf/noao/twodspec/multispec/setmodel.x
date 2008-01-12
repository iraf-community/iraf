include "ms.h"

# SET_MODEL -- Set a line of model data from profiles based on their
# ranges starting values.

procedure set_model (ms, model, model_profiles, ranges, len_line, len_profile,
    nspectra)

pointer	ms					# MULTISPEC data structure
real	model[len_line]				# Model line created
real	model_profiles[len_profile, nspectra]	# Model profiles
real	ranges[nspectra, LEN_RANGES]		# Ranges array for the profiles
int	len_line				# The length of the model line
int	len_profile				# The length of the profiles
int	nspectra				# The number of spectra

int	i, x, spectrum

begin
	# Set the model background to zero.
	call aclrr (model, len_line)

	# For each spectrum and each profile point add contribution to model.
	do spectrum = 1, nspectra {
	    do i = 1, len_profile {
		# Column corresponding to profile point i and spectrum.
	        x = ranges[spectrum, X_START] + i - 1

		# Scale the model profile by the model parameter I0 and
		# add to the model line.
		if ((x >= 1) && (x <= len_line))
		    model[x] = model[x] + PARAMETER(ms, I0, spectrum) *
			model_profiles[i, spectrum]
	    }
	}
end

# SET_MODEL1 -- Set a line of model data from profiles based on the spectra
# function fit position centers and the ranges dx_start value.

procedure set_model1 (ms, line, profiles, coeff, ranges, len_line, len_profile,
    nspectra, model)

pointer	ms					# MULTISPEC data structure
int	line					# Image line for model
real	profiles[len_profile, nspectra]		# Profiles
real	coeff[ARB]				# Image interpolation coeff.
real	ranges[nspectra, LEN_RANGES]		# Ranges array for profiles
int	len_line				# Length of model line
int	len_profile				# Length of profiles
int	nspectra				# Number of spectra
real	model[len_line]				# Model line to be created

int	i, x, spectrum
real	x_start, dx

real	cveval(), asival()

begin
	# Clear the model to a zero background.
	call aclrr (model, len_line)

	# Add the contribution for each spectrum.
	do spectrum = 1, nspectra {
	    # Fit image interpolator to profile.
	    call asifit (profiles[1,spectrum], len_profile, coeff)

	    # Determine starting column corresponding to spectrum at specified
	    # line whose central position is given by the fit function.
	    x_start = cveval (CV(ms, X0_FIT, spectrum), real (line)) +
		ranges[spectrum, DX_START]

	    # For each column corresponding to a point in the profile determine
	    # the interpolation point dx within the profile and evaluate the
	    # the image interpolation function.

	    x = x_start
	    do i = 1, len_profile - 1 {
		x = x + 1
		if ((x >= 1) && (x <= len_line)) {
		    dx = x - x_start + 1
		    model[x] = model[x] + asival (dx, coeff)
		}
	    }
	}
end
