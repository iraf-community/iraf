include	"nlfitdef.h"
include	"../lib/nlfit.h"

# NLINIT --  Initialize for non-linear fitting

procedure nlinit (nl, fnc, dfnc, params, dparams, nparams, plist, nfparams,
    tol, itmax)

pointer	nl		# pointer to nl fitting structure
extern	fnc()		# fitting function
extern	dfnc()		# derivative function
real	params		# initial values for the parameters
real	dparams		# initial guess at uncertainties in parameters
int	nparams		# number of parameters
int	plist[ARB]	# list of active parameters
int	nfparams	# number of fitted parameters
real	tol		# fitting tolerance
int	itmax		# maximum number of iterations

int	locpr()
errchk	calloc

begin
	# Allocate space for the non-linear package structure.
	call malloc (nl, LEN_NLSTRUCT, TY_STRUCT)

	# Store the addresses of the non-linear functions.
	NL_FUNC(nl) = locpr (fnc)
	NL_DFUNC(nl) = locpr (dfnc)

	# Allocate temporary space for arrays.
	call calloc (NL_ALPHA(nl), nfparams * nfparams, TY_REAL)
	call calloc (NL_COVAR(nl), nfparams * nfparams, TY_REAL)
	call calloc (NL_CHOFAC(nl), nfparams * nfparams, TY_REAL)
	call calloc (NL_BETA(nl), nfparams, TY_REAL)

	# Allocate space for parameter and trial parameter vectors.
	call calloc (NL_DERIV(nl), nparams, TY_REAL)
	call calloc (NL_PARAM(nl), nparams, TY_REAL)
	call calloc (NL_TRY(nl), nparams, TY_REAL)
	call calloc (NL_DPARAM(nl), nparams, TY_REAL)
	call calloc (NL_PLIST(nl), nparams, TY_INT)

	# Initialize the parameters.
	NL_NPARAMS(nl) = nparams
	NL_NFPARAMS(nl) = nfparams
	call amovr (params, PARAM(NL_PARAM(nl)), nparams)
	call amovr (dparams, DPARAM(NL_DPARAM(nl)), nparams)
	call amovi (plist, PLIST(NL_PLIST(nl)), nfparams)
	NL_TOL(nl) = tol
	NL_ITMAX(nl) = itmax

	# Set up the parameter list.
	call nl_list (PLIST(NL_PLIST(nl)), NL_NPARAMS(nl), NL_NFPARAMS(nl))
end


# NL_LIST -- Procedure to order the list

procedure nl_list (list, nlist, nfit)

int	list[ARB]		# list
int	nlist			# number of elements in the list
int	nfit			# number of active list elments

int	i, j, nfitp1, iokay

begin
	nfitp1 = nfit + 1

	do i = 1, nlist {
	    iokay = 0
	    do j = 1, nfit {
		if (list[j] == i)
		    iokay = iokay + 1
	    }
	    if (iokay == 0) {
		list[nfitp1] = i
		nfitp1 = nfitp1 + 1
	    } else if (iokay > 1)
	        call error (0, "Bad ordering of parameter list")
	}

	if (nfitp1 != (nlist + 1))
	    call error (0, "Bad ordering of parameter list")
end
