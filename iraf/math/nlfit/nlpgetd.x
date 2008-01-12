include "nlfitdefd.h"

# NLPGET - Retreive parameter values

procedure nlpgetd (nl, params, nparams)

pointer	nl			# pointer to the nlfit structure
double	params[ARB]		# parameter array
int	nparams			# the number of the parameters

begin
	nparams = NL_NPARAMS(nl)
	call amovd (PARAM(NL_PARAM(nl)), params, nparams)
end
