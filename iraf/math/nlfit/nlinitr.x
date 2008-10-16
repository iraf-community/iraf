include "nlfitdefr.h"

# NLINIT --  Initialize for non-linear fitting

procedure nlinitr (nl, fnc, dfnc, params, dparams, nparams, plist, nfparams,
		    tol, itmax)

pointer	nl		# pointer to nl fitting structure
pointer	fnc		# fitting function address
pointer	dfnc		# derivative function address
real	params[ARB]	# initial values for the parameters
real	dparams[ARB]	# initial guess at uncertainties in parameters
size_t	nparams		# number of parameters
long	plist[ARB]	# list of active parameters
size_t	nfparams	# number of fitted parameters
real	tol		# fitting tolerance
int	itmax		# maximum number of iterations

size_t	sz_val
errchk	malloc, calloc, nl_list

begin
	# Allocate space for the non-linear package structure.
	sz_val = LEN_NLSTRUCT
	call malloc (nl, sz_val, TY_STRUCT)

	# Store the addresses of the non-linear functions.
	NL_FUNC(nl) = fnc
	NL_DFUNC(nl) = dfnc

	# Allocate temporary space for arrays.
	sz_val = nfparams * nfparams
	call calloc (NL_ALPHA(nl), sz_val, TY_REAL)
	call calloc (NL_COVAR(nl), sz_val, TY_REAL)
	call calloc (NL_CHOFAC(nl), sz_val, TY_REAL)
	sz_val = nfparams
	call calloc (NL_BETA(nl), sz_val, TY_REAL)

	# Allocate space for parameter and trial parameter vectors.
	sz_val = nparams
	call calloc (NL_DERIV(nl), sz_val, TY_REAL)
	call calloc (NL_PARAM(nl), sz_val, TY_REAL)
	call calloc (NL_OPARAM(nl), sz_val, TY_REAL)
	call calloc (NL_TRY(nl), sz_val, TY_REAL)
	call calloc (NL_DPARAM(nl), sz_val, TY_REAL)
	call calloc (NL_DELPARAM(nl), sz_val, TY_REAL)
	call calloc (NL_PLIST(nl), sz_val, TY_LONG)

	# Initialize the parameters.
	NL_NPARAMS(nl) = nparams
	NL_NFPARAMS(nl) = nfparams
	sz_val = nparams
	call amovr (params, PARAM(NL_PARAM(nl)), sz_val)
	call amovr (params, PARAM(NL_OPARAM(nl)), sz_val)
	call amovr (dparams, DPARAM(NL_DELPARAM(nl)), sz_val)
	sz_val = nfparams
	call amovl (plist, PLIST(NL_PLIST(nl)), sz_val)
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
