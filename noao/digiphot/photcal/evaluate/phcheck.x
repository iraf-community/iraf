include "../lib/parser.h"
include "../lib/preval.h"

# PH_SETEQN -- Determine whether a PHOTCAL expression includes any set
# equations which contain references to catalog variables. Return a YES
# or NO status.

int procedure ph_seteqn (code)

pointer	code			# the photcal equation code

int	ip, ins
pointer	setcode
int	pr_geti(), pr_gsym(), ph_cateqn()
pointer	pr_gsymp()

begin
	# Return NO if there are no defined set equations.
	if (pr_geti (NSETEQS) <= 0)
	    return (NO)

	# Initialize.
	ip = 0  
	ins = Memi[code+ip]

	# Loop over the equation parsing instructions.
	while (ins != PEV_EOC) {

	    switch (ins) {
	    case PEV_SETEQ:
		ip = ip + 1
		setcode = pr_gsymp (pr_gsym (Memi[code+ip], PTY_SETEQ),
		    PSEQRPNEQ)
		if (ph_cateqn (setcode) == YES)
		    return (YES)
	    case PEV_NUMBER, PEV_CATVAR, PEV_OBSVAR, PEV_PARAM:
		ip = ip + 1
	    case PEV_EXTEQ, PEV_TRNEQ:
		ip = ip + 1
	    default:
		# do nothing
	    }

	    ip = ip + 1
	    ins = Memi[code+ip]
	}

	return (NO)
end


# PH_CATEQN -- Given a PHOTCAL set equation determine whether there are any
# references in it to catalog variables. Return a YES or NO status.

int procedure ph_cateqn (code)

pointer	code		# pointer to the equation code

int	ip, ins
int	pr_geti()

begin
	# Return NO if there are no catalog variables.
	if (pr_geti (NCATVARS) <= 0)
	    return (NO)

	# Initialize.
	ip = 0  
	ins = Memi[code+ip]

	# Loop over the equation parsing instructions.
	while (ins != PEV_EOC) {

	    switch (ins) {
	    case PEV_CATVAR:
		return (YES)
	    case  PEV_NUMBER, PEV_OBSVAR, PEV_PARAM:
		ip = ip + 1
	    case PEV_SETEQ, PEV_EXTEQ, PEV_TRNEQ:
		ip = ip + 1
	    default:
		# do nothing
	    }

	    ip = ip + 1
	    ins = Memi[code+ip]
	}

	return (NO)
end


# PH_SETVAR -- Check to see whether the fit expression portion of a PHOTCAL
# transformation equation contains references to set equations which include
# previously unreferenced catalog variables. Return the number of previously
# unreferenced set equations.

int procedure ph_setvar (eqn, sym, cmap, omap, tuservars, uservars, usererrs,
	nstdvars, nobsvars, eqset, maxnset, userset, nset)

int	eqn			# the equation number
pointer	sym			# pointer to the equation symbol
pointer	cmap			# pointer to catalog variable column map
pointer	omap			# pointer to observations variable column map
int	tuservars[nstdvars,ARB]	# list of active catalog variables per equation
int	uservars[ARB]		# list of active catalog variables
int	usererrs[ARB]		# list of active observational error columns
int	nstdvars		# total number of catalog variables
int	nobsvars		# total number of observations variables
int	eqset[maxnset,ARB]	# set equation table
int	maxnset			# maximum number of set equations
int	userset[ARB]		# the list of active set equations
int	nset			# number of set equations

bool	new
int	i, ip, ins, nvar
pointer	fitcode, setsym
int	pr_gsym(), ph_catvar()
pointer	pr_gsymp()

begin
	# Get the fit expression symbols.
	fitcode = pr_gsymp (sym, PTEQRPNFIT)

	# Initialize.
	nvar = 0
	ip = 0  
	ins = Memi[fitcode+ip]

	# Loop over the fit expression.
	while (ins != PEV_EOC) {

	    switch (ins) {
	    case PEV_SETEQ:
		ip = ip + 1
		setsym = pr_gsym (Memi[fitcode+ip], PTY_SETEQ)
		if (ph_catvar (eqn, setsym, cmap, omap, tuservars, uservars,
		    usererrs, nstdvars, nobsvars) == YES) {
		    new = true
		    do i = 1, nset + nvar {
			if (setsym != userset[i])
			    next
			eqset[i,eqn] = setsym
			new = false
			break
		    }
		    if (new) {
			nvar = nvar + 1
			eqset[nset+nvar,eqn] = setsym
			userset[nset+nvar] = setsym 
		    }
		}
	    case PEV_NUMBER, PEV_CATVAR, PEV_OBSVAR, PEV_PARAM:
		ip = ip + 1
	    case PEV_EXTEQ, PEV_TRNEQ:
		ip = ip + 1
	    default:
		# do nothing
	    }

	    ip = ip + 1
	    ins = Memi[fitcode+ip]
	}

	return (nvar)
end


# PH_CATVAR -- Determine whether any catalog variables in the set
# equation have not been previously referenced in the transformation
# equations.

int procedure ph_catvar (eqn, setsym, cmap, omap, tuservars, uservars,
	usererrs, nstdvars, nobsvars)

int	eqn			# the transformation equation number
int	setsym			# the set equation symbol
pointer	cmap			# pointer to catalog variable column map
pointer	omap			# pointer to observations variable column map
int	tuservars[nstdvars,ARB]	# active variables as a function of eqn
int	uservars[ARB]		# currently active catalog variables
int	usererrs[ARB]		# currently active observations varaiables errs
int	nstdvars		# the total number of catalog variables
int	nobsvars		# the total number of observations variables

int	setcode, ip, ins, stat, vcol, ecol
pointer	catsym, obsym
int	pr_gsym(), pr_gsymi(), pr_findmap1()
pointer	pr_gsymp()

begin
	# Get the set equation code.
	setcode = pr_gsymp (setsym, PSEQRPNEQ)

	# Initialize.
	ip = 0  
	ins = Memi[setcode+ip]
	stat = NO

	while (ins != PEV_EOC) {

	    switch (ins) {
	    case PEV_CATVAR:
		ip = ip + 1
		catsym = pr_gsym (Memi[setcode+ip] - nobsvars, PTY_CATVAR)
		vcol = pr_gsymi (catsym, PINPCOL)
		vcol = pr_findmap1 (cmap, vcol)
		if (uservars[vcol] <= 0) {
		    tuservars[vcol,eqn] = -(vcol + nobsvars)
		    uservars[vcol] = -(vcol + nobsvars)
		    stat = YES
		}

	    case PEV_OBSVAR:
		ip = ip + 1
		obsym = pr_gsym (Memi[setcode+ip], PTY_OBSVAR)
		vcol = pr_gsymi (obsym, PINPCOL)
		ecol = pr_gsymi (obsym, PINPERRCOL)
		vcol = pr_findmap1 (omap, vcol)
		if (! IS_INDEFI(ecol))
		    ecol = pr_findmap1 (omap, ecol)
		if (! IS_INDEFI(ecol))
		    usererrs[vcol] = ecol

	    case PEV_NUMBER, PEV_PARAM:
		ip = ip + 1

	    case PEV_SETEQ, PEV_EXTEQ, PEV_TRNEQ:
		ip = ip + 1

	    default:
		# do nothing
	    }

	    ip = ip + 1
	    ins = Memi[setcode+ip]
	}

	return (stat)
end
