include	"ecidentify.h"

# EC_RMS -- Compute RMS of fit about the user coordinates

double procedure ec_rms (ec)

pointer	ec			# ID pointer

int	i, nrms
double	rms

begin
	rms = 0.
	nrms = 0
	for (i=1; i<=EC_NFEATURES(ec); i=i+1) {
	    if (!IS_INDEFD (USER(ec,i)) && FTYPE(ec,i) > 0) {
		rms = rms + (FIT(ec,i) - USER(ec,i)) ** 2
		nrms = nrms + 1
	    }
	}

	if (nrms > 0)
	    rms = sqrt (rms / nrms)
	else
	    rms = INDEFD

	return (rms)
end
