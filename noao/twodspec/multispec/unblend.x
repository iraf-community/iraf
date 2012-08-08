include	"ms.h"

# UNBLEND -- Create unblended data profiles from a blended data line.
#
# For each point in each spectrum profile determine the corresponding column
# in the data line from the ranges array.  If the model is non-zero then the
# data profile value for that spectrum is a fraction of the total data value
# at that point given by the fraction of that model profile to the total
# model at that point.

procedure unblend (data, data_profiles, model, model_profiles, ranges,
    len_line, len_profile, nspectra)

real	data[len_line]				# Data line to be unblended
real	data_profiles[len_profile, nspectra]	# Output data profiles
real	model[len_line]				# Model line
real	model_profiles[len_profile, nspectra]	# Model profiles
real	ranges[nspectra, LEN_RANGES]		# Ranges for model profiles
int	len_line				# Length of data/model line
int	len_profile				# Length of each profile
int	nspectra				# Number of spectra

int	i, x, spectrum

begin
	do spectrum = 1, nspectra {
	    do i = 1, len_profile {
		x = ranges[spectrum, X_START] + i - 1
		if ((x >= 1) && (x <= len_line)) {
		    if (model[x] > 0.)
		        data_profiles[i, spectrum] =
		            data[x] * model_profiles[i, spectrum] / model[x]
		    else
		        data_profiles[i, spectrum] = data[x]
	        }
	    }
	}
end
