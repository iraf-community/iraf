include	"identify.h"

# ID_RMS -- Compute RMS of fit about the user coordinates

double procedure id_rms (id)

pointer	id			# ID pointer

int	i, nrms
double	rms

begin
	rms = 0.
	nrms = 0
	for (i=1; i<=ID_NFEATURES(id); i=i+1) {
	    if (!IS_INDEFD (USER(id,i)) && WTS(id,i) != 0.) {
		rms = rms + (FIT(id,i) - USER(id,i)) ** 2
		nrms = nrms + 1
	    }
	}

	if (nrms > 0)
	    rms = sqrt (rms / nrms)
	else
	    rms = INDEFD

	return (rms)
end
