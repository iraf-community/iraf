include "nlfitdefd.h"

# NLPGET - Retreive parameter values

procedure nlpgetd (nl, params, nparams)

pointer	nl			# pointer to the nlfit structure
double	params[ARB]		# parameter array
size_t	nparams			# the number of the parameters

size_t	sz_val

begin
	nparams = NL_NPARAMS(nl)
	sz_val = nparams
	call amovd (PARAM(NL_PARAM(nl)), params, sz_val)
end
