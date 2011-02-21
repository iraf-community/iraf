include "nlfitdefr.h"

# NLINIT --  Initialize for non-linear fitting

procedure nlinitr (nl, fnc, dfnc, params, dparams, nparams, plist, nfparams,
		    tol, itmax)

pointer	nl		# pointer to nl fitting structure
int	fnc		# fitting function address
int	dfnc		# derivative function address
real	params[ARB]	# initial values for the parameters
real	dparams[ARB]	# initial guess at uncertainties in parameters
int	nparams		# number of parameters
int	plist[ARB]	# list of active parameters
int	nfparams	# number of fitted parameters
real	tol		# fitting tolerance
int	itmax		# maximum number of iterations

errchk	malloc, calloc, nl_list

begin
	# Allocate space for the non-linear package structure.
	call calloc (nl, LEN_NLSTRUCT, TY_STRUCT)

	# Store the addresses of the non-linear functions.
	NL_FUNC(nl) = fnc
	NL_DFUNC(nl) = dfnc

	# Allocate temporary space for arrays.
	call calloc (NL_ALPHA(nl), nfparams * nfparams, TY_REAL)
	call calloc (NL_COVAR(nl), nfparams * nfparams, TY_REAL)
	call calloc (NL_CHOFAC(nl), nfparams * nfparams, TY_REAL)
	call calloc (NL_BETA(nl), nfparams, TY_REAL)

	# Allocate space for parameter and trial parameter vectors.
	call calloc (NL_DERIV(nl), nparams, TY_REAL)
	call calloc (NL_PARAM(nl), nparams, TY_REAL)
	call calloc (NL_OPARAM(nl), nparams, TY_REAL)
	call calloc (NL_TRY(nl), nparams, TY_REAL)
	call calloc (NL_DPARAM(nl), nparams, TY_REAL)
	call calloc (NL_DELPARAM(nl), nparams, TY_REAL)
	call calloc (NL_PLIST(nl), nparams, TY_INT)

	# Initialize the parameters.
	NL_NPARAMS(nl) = nparams
	NL_NFPARAMS(nl) = nfparams
	call amovr (params, PARAM(NL_PARAM(nl)), nparams)
	call amovr (params, PARAM(NL_OPARAM(nl)), nparams)
	call amovr (dparams, DPARAM(NL_DELPARAM(nl)), nparams)
	call amovi (plist, PLIST(NL_PLIST(nl)), nfparams)
	NL_TOL(nl) = tol
	NL_ITMAX(nl) = itmax
	NL_SCATTER(nl) = real(0.0)

	# Set up the parameter list.
	iferr {
	    call nl_list (PLIST(NL_PLIST(nl)), NL_NPARAMS(nl), NL_NFPARAMS(nl))
	} then {
	    call nlfreer (nl)
	    nl = NULL
	}
end
