include "nlfitdefd.h"


# NLZERO --  Zero the accumulators and reset the fitting parameter values to
# their original values set by nlinit().

procedure nlzerod (nl)

pointer	nl		# pointer to nl fitting structure

int	nparams		# number of parameters
int	nfparams	# number of fitted parameters

begin
	# Get number of parameters and fitting parameters.
	nparams  = NL_NPARAMS(nl)
	nfparams = NL_NFPARAMS(nl)

	# Clear temporary array space.
	call aclrd (ALPHA(NL_ALPHA(nl)), nfparams * nfparams)
	call aclrd (COVAR(NL_COVAR(nl)), nfparams * nfparams)
	call aclrd (CHOFAC(NL_CHOFAC(nl)), nfparams * nfparams)
	call aclrd (BETA(NL_BETA(nl)), nfparams)

	# Clear space for derivatives and trial parameter vectors.
	call aclrd (DERIV(NL_DERIV(nl)), nparams)
	call aclrd (TRY(NL_TRY(nl)), nparams)

	# Reset parameters.
	call amovd (OPARAM(NL_OPARAM(nl)), PARAM(NL_PARAM(nl)), nparams)
	call aclrd (DPARAM(NL_DPARAM(nl)), nparams)

	NL_SCATTER(nl) = double(0.0)
end
