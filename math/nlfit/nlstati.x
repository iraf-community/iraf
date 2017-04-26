include "nlfitdefr.h"
include <math/nlfit.h>

# NLSTATI - Fetch a NLFIT integer parameter

int procedure nlstati (nl, param)

pointer	nl		# pointer to NLFIT structure
int	param		# parameter to be fetched

begin
	switch (param) {
	case NLNPARAMS:
	    return (NL_NPARAMS(nl))
	case NLNFPARAMS:
	    return (NL_NFPARAMS(nl))
	case NLITMAX:
	    return (NL_ITMAX(nl))
	case NLITER:
	    return (NL_ITER(nl))
	case NLNPTS:
	    return (NL_NPTS(nl))
	default:
	    return (INDEFI)
	}
end
