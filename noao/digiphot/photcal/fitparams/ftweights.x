include	"../lib/parser.h"
include "../lib/preval.h"

# FT_WTUNIFORM -- Set all the weights to 1.0.

procedure ft_wtuniform (otable, wtable)

pointer	otable				# observation table
pointer	wtable				# weight table (output)

int	nobs
int	mct_nrows()

begin
	# Get number of observations.
	nobs = mct_nrows (otable)

	# Clear weight table with ones (uniform weighting), and enter
	# data at the last observation set weight table counters.

	call mct_clearr (wtable, 1.0)
	call mct_putr (wtable, nobs, 1, 1.0)
end


# FT_WTEQNS -- Compute the value of the weights equation for each point. 

procedure ft_wteqns (sym, otable, wtable)

int	sym				# equation symbol
pointer	otable				# observation table
pointer	wtable				# weight table (output)

int	n, nobs
real	wtval, minval, maxval
pointer	varptr, parptr
pointer	wtcode, mincode, maxcode

#bool	clgetb()
int	mct_nrows()
pointer	pr_gsymp(), mct_getrow()
real	pr_eval()

begin
	# Debug ?
	#if (clgetb ("debug.fitcode"))
	    #call eprintf ("ft_wteval (sym=%d) (ot=%d) (wt=%d)\n") {
		#call pargi (sym)
		#call pargi (otable)
		#call pargi (wtable)
	#}

	# Get number of observations.
	nobs = mct_nrows (otable)

	# Clear weight table with ones (uniform weighting), and enter
	# data at the last observation set weight table counters.

	call mct_clearr (wtable, 1.0)
	call mct_putr (wtable, nobs, 1, 1.0)

	# Compute weigths using the weight equation, if there is one defined.
	wtcode = pr_gsymp (sym, PTEQRPNWEIGHT)
	if (wtcode != NULL) {

	    # Get pointer to parameter values for the current equation.
	    parptr = pr_gsymp (sym, PTEQSPARVAL)

	    # Get minimum and maximum equation codes.
	    mincode = pr_gsymp (sym, PTEQRPNWTSMIN)
	    maxcode = pr_gsymp (sym, PTEQRPNWTSMAX)

	    # Iterate over all observations.
	    do n = 1, nobs {

		# Get variable values for the current observation.
		varptr = mct_getrow (otable, n)

		# Compute weight value.
		wtval = pr_eval (wtcode, Memr[varptr], Memr[parptr])
		if (IS_INDEFR(wtval) || wtval < 0.0) {

		    call mct_putr (wtable, n, 1, 0.0)

		} else {

		    # Check value against minimum.
		    if (mincode != NULL) {
		        minval = pr_eval (mincode, Memr[varptr], Memr[parptr])
		        if (! IS_INDEFR(minval) && minval >= 0.0)
		            wtval = min (wtval, minval)
		    }

		    # Check value against maximum.
		    if (maxcode != NULL) {
		        maxval = pr_eval (maxcode, Memr[varptr], Memr[parptr])
		        if (! IS_INDEFR(maxval) && maxval >= 0.0)
		            wtval = max (wtval, maxval)
		    }

		    # Enter value into weight table.
		    call mct_putr (wtable, n, 1, wtval)
		}
	    }
	}

	# Debug ?
	#call dg_dweigths ("from ft_wteval", wtable)
end


# FT_WTPHOTERRS -- Set all the weights to the correct statistical value assuming
# that all the errors are in the photometric indices, that the errors are
# independent, that the equations are linear in the photometric indices,
# and that at least one of the observed or catalog variables in the equation
# has a measured error.

procedure ft_wtphoterrs (sym, otable, wtable)

int	sym				# equation symbol
pointer	otable				# observation table
pointer	wtable				# weight table (output)

double	errval
int	n, i, nobs, nobsvars, ncatvars, nrvars, nfvars, nerrors, symvar, icol
int	itemp
pointer	omap, cmap, sp, rcount, rerrcol, fcount, ferrcol, refcode
real	val
int	mct_nrows(), pr_geti(), pr_gsymi(), pr_gvari(), pr_findmap1()
int	ft_seval()
pointer	pr_gsymp()
#int	mct_ncols()
real	mct_getr()
errchk	pr_gsymi()

begin
	# Get number of observations.
	nobs = mct_nrows (otable)

	# Clear weight table with ones (uniform weighting), and enter
	# data at the last observation set weight table counters.

	call mct_clearr (wtable, 1.0)
	call mct_putr (wtable, nobs, 1, 1.0)

	# Map the column numbers for the observed and catalog variables.
	call pr_obsmap (omap, nobsvars)
	call pr_catmap (cmap, ncatvars)

	# Get the number of reference and fit equation variables.
	nrvars = pr_gsymi (sym, PTEQNRCAT) + pr_gsymi (sym, PTEQNROBS) 
	nfvars = pr_gsymi (sym, PTEQNFCAT) + pr_gsymi (sym, PTEQNFOBS) 

	# Allocate working space.
	call smark (sp)
	call salloc (rerrcol, nobsvars + ncatvars, TY_INT)
	call amovi (NULL, Memi[rerrcol], nobsvars + ncatvars)
	call salloc (rcount, nobsvars + ncatvars, TY_INT)
	call aclri (Memi[rcount], nobsvars + ncatvars)
	call salloc (ferrcol, nobsvars + ncatvars, TY_INT)
	call amovi (NULL, Memi[ferrcol], nobsvars + ncatvars)
	call salloc (fcount, nobsvars + ncatvars, TY_INT)
	call aclri (Memi[fcount], nobsvars + ncatvars)

	# Initialize.
	nerrors = 0

	# Compute the positions of the reference equation variable errors in
	# the observations table.

	do i = 1, nrvars {
	    Memi[rcount+i-1] = pr_gvari (sym, i, PTEQREFCNT)
	    symvar = pr_gvari (sym, i, PTEQREFVAR)
	    icol = pr_gsymi (symvar, PINPERRCOL)
	    if (IS_INDEFI(icol))
		Memi[rerrcol+i-1] = NULL
	    else {
		if (pr_gsymi (symvar, PSYMTYPE) == PTY_CATVAR)
		    Memi[rerrcol+i-1] = pr_findmap1 (cmap, icol) + nobsvars
		else
		    Memi[rerrcol+i-1] = pr_findmap1 (omap, icol)
		nerrors = nerrors + 1
	    }
	}

	# Evaluate the contribution of the set equations to the errors
	# in the reference equation.

	if (pr_geti (NSETEQS) > 0) {
	    refcode = pr_gsymp (sym, PTEQRPNREF)
	    itemp = ft_seval (refcode, cmap, omap, nobsvars,
	        Memi[rerrcol], Memi[rcount], nrvars)
	    nrvars = nrvars + itemp 
	    nerrors = nerrors + itemp
	}

	# Compute the positions of the fit equation variable errors in the
	# observations table.

	do i = 1, nfvars {
	    Memi[fcount+i-1] = pr_gvari (sym, i, PTEQFITCNT)
	    symvar = pr_gvari (sym, i, PTEQFITVAR)
	    icol = pr_gsymi (symvar, PINPERRCOL)
	    if (IS_INDEFI(icol))
		Memi[ferrcol+i-1] = NULL
	    else {
		if (pr_gsymi (symvar, PSYMTYPE) == PTY_CATVAR)
		    Memi[ferrcol+i-1] = pr_findmap1 (cmap, icol) + nobsvars
		else
		    Memi[ferrcol+i-1] = pr_findmap1 (omap, icol)
		nerrors = nerrors + 1
	    }
	}

	# Evaluate the contribution of the set equations to the errors
	# in the fit equation.

	if (pr_geti (NSETEQS) > 0) {
	    refcode = pr_gsymp (sym, PTEQRPNFIT)
	    itemp = ft_seval (refcode, cmap, omap, nobsvars,
	        Memi[ferrcol], Memi[fcount], nfvars)
	    nfvars = nfvars + itemp
	    nerrors = nerrors + itemp
	}

	# Loop over the table rows.
	if (nerrors > 0) {
	    do n = 1, nobs {

	        errval = 0.0d0

	        # Add contributions from the reference equation variables.
	        do i = 1, nrvars {
		    if (Memi[rerrcol+i-1] == NULL)
		        next
		    val = mct_getr (otable, n, Memi[rerrcol+i-1])
		    if (IS_INDEFR(val) || val < 0.0)
			next
		    errval = errval + real (Memi[rcount+i-1]) * (val * val)
	        }

	        # Add contributions from the fitting equation variables.
	        do i = 1, nfvars {
		    if (Memi[ferrcol+i-1] == NULL)
		        next
		    val = mct_getr (otable, n, Memi[ferrcol+i-1])
		    if (IS_INDEFR(val) || val < 0.0)
			next
		    errval = errval + real (Memi[fcount+i-1]) * (val * val)
	        }

	        # Check for negative and zero error values and enter value
		# into weight table.
	        if (errval <= 0.0d0)
	            call mct_putr (wtable, n, 1, 0.0)
		else
	            call mct_putr (wtable, n, 1, real (1.0d0 / errval))
	    }
	}

	call pr_unmap (cmap)
	call pr_unmap (omap)

	call sfree (sp)
end


# FT_SEVAL -- Locate any catalog and observations variables that are
# defined in up to two levels of set equations.

int procedure ft_seval (code, cmap, omap, nobsvars, errcol, ecount, nerrors)

pointer	code		# the equation code
pointer	cmap		# pointer to the catalog variable column map
pointer	omap		# pointer to the observations variable column map
int	nobsvars	# the number of observations variables
int	errcol[ARB]	# the input/output error columns
int	ecount[ARB]	# the input/output error column counts
int	nerrors		# the previous number of errors

int	ip, ins, newerr
pointer	setcode
int	pr_gsym(), ft_scoeval()
pointer	pr_gsymp()

begin
	ip = 0
	ins = Memi[code+ip]

	newerr = 0
	while (ins != PEV_EOC) {
	    switch (ins) {
	    case PEV_SETEQ:
		ip = ip + 1
		setcode = pr_gsymp (pr_gsym (Memi[code+ip], PTY_SETEQ),
		    PSEQRPNEQ)
		newerr = newerr + ft_scoeval (setcode, cmap, omap, nobsvars,
		    errcol, ecount, nerrors + newerr)
	    case PEV_NUMBER, PEV_CATVAR, PEV_OBSVAR, PEV_PARAM:
		ip = ip + 1
	    case PEV_EXTEQ, PEV_TRNEQ:
		ip = ip + 1
	    default:
		;
	    }

	    ip = ip + 1
	    ins = Memi[code+ip]
	}

	return (newerr)
end


# FT_SCOEVAL -- Locate any catalog and observations variables as well as any
# set equations that are defined in a previous set equation.

int procedure ft_scoeval (code, cmap, omap, nobsvars, errcol, ecount, nerrors)

pointer	code		# the equation code
pointer	cmap		# pointer to the catalog variable column map
pointer	omap		# pointer to the observations variable column map
int	nobsvars	# the number of observations variables
int	errcol[ARB]	# the input/output error columns
int	ecount[ARB]	# the input/output error column counts
int	nerrors		# the previous number of errors

bool	new
int	i, ip, ins, newerr, ecol
pointer	sym, setcode
int	pr_gsym(), pr_gsymi(), pr_findmap1(), ft_coeval()
pointer	pr_gsymp()

begin
	ip = 0
	ins = Memi[code+ip]

	newerr = 0
	while (ins != PEV_EOC) {
	    switch (ins) {
	    case PEV_SETEQ:
		ip = ip + 1
		setcode = pr_gsymp (pr_gsym (Memi[code+ip], PTY_SETEQ),
		    PSEQRPNEQ)
		newerr = newerr + ft_coeval (setcode, cmap, omap, nobsvars,
		    errcol, ecount, nerrors + newerr)
	    case PEV_CATVAR:
		ip = ip + 1
		sym = pr_gsym (Memi[code+ip] - nobsvars, PTY_CATVAR)
		ecol = pr_gsymi (sym, PINPERRCOL)
		if (! IS_INDEFI(ecol))
		    ecol = pr_findmap1 (cmap, ecol) + nobsvars
		else
		    ecol = NULL
		if (ecol != NULL) {
		    new = true
		    do i = 1, nerrors + newerr {
			if (ecol != errcol[i])
			    next
			new = false
			ecount[i] = ecount[i] + 1 
		    }
		    if (new) {
			newerr = newerr + 1
			errcol[nerrors+newerr] = ecol
			ecount[nerrors+newerr] = 1 
		    }
		}
	    case PEV_OBSVAR:
		ip = ip + 1
		sym = pr_gsym (Memi[code+ip], PTY_OBSVAR)
		ecol = pr_gsymi (sym, PINPERRCOL)
		if (! IS_INDEFI(ecol))
		    ecol = pr_findmap1 (omap, ecol)
		else
		    ecol = NULL
		if (ecol != NULL) {
		    new = true
		    do i = 1, nerrors + newerr {
			if (ecol != errcol[i])
			    next
			new = false
			ecount[i] = ecount[i] + 1 
		    }
		    if (new) {
			newerr = newerr + 1
			errcol[nerrors+newerr] = ecol
			ecount[nerrors+newerr] = 1 
		    }
		}
	    case PEV_NUMBER, PEV_PARAM:
		ip = ip + 1
	    case PEV_EXTEQ, PEV_TRNEQ:
		ip = ip + 1
	    default:
		;
	    }

	    ip = ip + 1
	    ins = Memi[code+ip]
	}

	return (newerr)
end


# FT_COEVAL -- Locate any catalog and observations variables that are defined i
# a previous set equation.

int procedure ft_coeval (code, cmap, omap, nobsvars, errcol, ecount, nerrors)

pointer	code		# the equation code
pointer	cmap		# pointer to the catalog variable column map
pointer	omap		# pointer to the observations variable column map
int	nobsvars	# the number of observations variables
int	errcol[ARB]	# the input/output error columns
int	ecount[ARB]	# the input/output error column counts
int	nerrors		# the previous number of errors

bool	new
int	i, ip, ins, newerr, ecol
pointer	sym
int	pr_gsym(), pr_gsymi(), pr_findmap1()

begin
	ip = 0
	ins = Memi[code+ip]

	newerr = 0
	while (ins != PEV_EOC) {
	    switch (ins) {
	    case PEV_SETEQ:
		ip = ip + 1
	    case PEV_CATVAR:
		ip = ip + 1
		sym = pr_gsym (Memi[code+ip] - nobsvars, PTY_CATVAR)
		ecol = pr_gsymi (sym, PINPERRCOL)
		if (! IS_INDEFI(ecol))
		    ecol = pr_findmap1 (cmap, ecol) + nobsvars
		else
		    ecol = NULL
		if (ecol != NULL) {
		    new = true
		    do i = 1, nerrors + newerr {
			if (ecol != errcol[i])
			    next
			new = false
			ecount[i] = ecount[i] + 1 
		    }
		    if (new) {
			newerr = newerr + 1
			errcol[nerrors+newerr] = ecol
			ecount[nerrors+newerr] = 1 
		    }
		}
	    case PEV_OBSVAR:
		ip = ip + 1
		sym = pr_gsym (Memi[code+ip], PTY_OBSVAR)
		ecol = pr_gsymi (sym, PINPERRCOL)
		if (! IS_INDEFI(ecol))
		    ecol = pr_findmap1 (omap, ecol)
		else
		    ecol = NULL
		if (ecol != NULL) {
		    new = true
		    do i = 1, nerrors + newerr {
			if (ecol != errcol[i])
			    next
			new = false
			ecount[i] = ecount[i] + 1 
		    }
		    if (new) {
			newerr = newerr + 1
			errcol[nerrors+newerr] = ecol
			ecount[nerrors+newerr] = 1 
		    }
		}
	    case PEV_NUMBER, PEV_PARAM:
		ip = ip + 1
	    case PEV_EXTEQ, PEV_TRNEQ:
		ip = ip + 1
	    default:
		;
	    }

	    ip = ip + 1
	    ins = Memi[code+ip]
	}

	return (newerr)
end
