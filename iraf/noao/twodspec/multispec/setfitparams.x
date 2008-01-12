include "ms.h"

# SET_FITPARAMS -- Set the fitparams array from the spectra range array
# and the parameters array.

procedure set_fitparams (spectra, parameters, nspectra, nparams, fitparams)

int	spectra[ARB]
int	parameters[nparams]
int	nspectra
int	nparams
int	fitparams[nspectra, nparams]

int	i, j

bool	is_in_range()

begin
	do i = 1, nspectra {
	    do j = 1, nparams {
	        if (is_in_range (spectra, i) && (parameters[j] == YES))
		    fitparams[i, j] = YES
		else
		    fitparams[i, j] = NO
	    }
	}
end
