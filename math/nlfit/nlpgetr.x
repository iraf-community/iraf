include "nlfitdefr.h"

# NLPGET - Retreive parameter values

procedure nlpgetr (nl, params, nparams)

pointer	nl			# pointer to the nlfit structure
real	params[ARB]		# parameter array
int	nparams			# the number of the parameters

begin
	nparams = NL_NPARAMS(nl)
	call amovr (PARAM(NL_PARAM(nl)), params, nparams)
end
