include "nlfitdef.h"
include "../lib/nlfit.h"

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
	default:
	    call error (0, "NLSTATI: Unknown integer parameter")
	}
end


# NLSTATR - Fetch an NLFIT real parameter

real procedure nlstatr (nl, param)

pointer	nl		# pointer to NLFIT structure
int	param		# parameter to be fetched

begin
	switch (param) {
	case NLSUMSQ:
	    return (NL_SUMSQ(nl))
	case NLOLDSQ:
	    return (NL_OLDSQ(nl))
	case NLTOL:
	    return (NL_TOL(nl))
	case NLLAMBDA:
	    return (NL_LAMBDA(nl))
	default:
	    call error (0, "NLSTATR: Unknown real parameter")
	}
end
