include "nlfitdefd.h"

# NLINIT --  Initialize for non-linear fitting

procedure nlinitd (nl, fnc, dfnc, params, dparams, nparams, plist, nfparams,
		    tol, itmax)

pointer	nl		# pointer to nl fitting structure
int	fnc		# fitting function address
int	dfnc		# derivative function address
double	params[ARB]	# initial values for the parameters
double	dparams[ARB]	# initial guess at uncertainties in parameters
int	nparams		# number of parameters
int	plist[ARB]	# list of active parameters
int	nfparams	# number of fitted parameters
double	tol		# fitting tolerance
int	itmax		# maximum number of iterations

errchk	malloc, calloc, nl_list

begin
	# Allocate space for the non-linear package structure.
	call calloc (nl, LEN_NLSTRUCT, TY_STRUCT)

	# Store the addresses of the non-linear functions.
	NL_FUNC(nl) = fnc
	NL_DFUNC(nl) = dfnc

	# Allocate temporary space for arrays.
	call calloc (NL_ALPHA(nl), nfparams * nfparams, TY_DOUBLE)
	call calloc (NL_COVAR(nl), nfparams * nfparams, TY_DOUBLE)
	call calloc (NL_CHOFAC(nl), nfparams * nfparams, TY_DOUBLE)
	call calloc (NL_BETA(nl), nfparams, TY_DOUBLE)

	# Allocate space for parameter and trial parameter vectors.
	call calloc (NL_DERIV(nl), nparams, TY_DOUBLE)
	call calloc (NL_PARAM(nl), nparams, TY_DOUBLE)
	call calloc (NL_OPARAM(nl), nparams, TY_DOUBLE)
	call calloc (NL_TRY(nl), nparams, TY_DOUBLE)
	call calloc (NL_DPARAM(nl), nparams, TY_DOUBLE)
	call calloc (NL_DELPARAM(nl), nparams, TY_DOUBLE)
	call calloc (NL_PLIST(nl), nparams, TY_INT)

	# Initialize the parameters.
	NL_NPARAMS(nl) = nparams
	NL_NFPARAMS(nl) = nfparams
	call amovd (params, PARAM(NL_PARAM(nl)), nparams)
	call amovd (params, PARAM(NL_OPARAM(nl)), nparams)
	call amovd (dparams, DPARAM(NL_DELPARAM(nl)), nparams)
	call amovi (plist, PLIST(NL_PLIST(nl)), nfparams)
	NL_TOL(nl) = tol
	NL_ITMAX(nl) = itmax
	NL_SCATTER(nl) = double(0.0)

	# Set up the parameter list.
	iferr {
	    call nl_list (PLIST(NL_PLIST(nl)), NL_NPARAMS(nl), NL_NFPARAMS(nl))
	} then {
	    call nlfreed (nl)
	    nl = NULL
	}
end
