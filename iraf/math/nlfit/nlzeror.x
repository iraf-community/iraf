include "nlfitdefr.h"


# NLZERO --  Zero the accumulators and reset the fitting parameter values to
# their original values set by nlinit().

procedure nlzeror (nl)

pointer	nl		# pointer to nl fitting structure

int	nparams		# number of parameters
int	nfparams	# number of fitted parameters

begin
	# Get number of parameters and fitting parameters.
	nparams  = NL_NPARAMS(nl)
	nfparams = NL_NFPARAMS(nl)

	# Clear temporary array space.
	call aclrr (ALPHA(NL_ALPHA(nl)), nfparams * nfparams)
	call aclrr (COVAR(NL_COVAR(nl)), nfparams * nfparams)
	call aclrr (CHOFAC(NL_CHOFAC(nl)), nfparams * nfparams)
	call aclrr (BETA(NL_BETA(nl)), nfparams)

	# Clear space for derivatives and trial parameter vectors.
	call aclrr (DERIV(NL_DERIV(nl)), nparams)
	call aclrr (TRY(NL_TRY(nl)), nparams)

	# Reset parameters.
	call amovr (OPARAM(NL_OPARAM(nl)), PARAM(NL_PARAM(nl)), nparams)
	call aclrr (DPARAM(NL_DPARAM(nl)), nparams)

	NL_SCATTER(nl) = real(0.0)
end
