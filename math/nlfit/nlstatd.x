include <math/nlfit.h>
include "nlfitdefd.h"

# NLSTAT[RD] - Fetch an NLFIT real/double parameter

double procedure nlstatd (nl, param)

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
	case NLSCATTER:
	    return (NL_SCATTER(nl))
	default:
	    return (INDEFD)
	}
end
