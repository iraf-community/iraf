include "../lib/parser.h"

define	DET_TOL		1.0e-20

# PH_IEREQN -- Compute error estimates for the photometric indices based
# on the value of any user-supplied errors equations. Use the chi-fitting
# method developed by Bevington.

int procedure ph_iereqn (params, a, nobs, deltaa, aindex, errors, nstd,
	seteqn, setvals, serrors, nset, nustd, eqindex, neq)

pointer	params[ARB]	# input array of pointers to the fitted parameters
real	a[ARB]		# array of observed and catalog variables
int	nobs		# the number of observed variables
real	deltaa[ARB]	# array of increments for the catalog variables
int	aindex[ARB]	# list of active catalog variables
real	errors[ARB]	# output array of error estimates
int	nstd		# number of catalog variables to be fit
int	seteqn[ARB]	# array of set equation codes
real	setvals[ARB]	# the set equation values
real	serrors[ARB]	# output array of set equation errors
int	nset		# the number of set equations
int	nustd		# the number of active catalog variables
int	eqindex[ARB]	# array of equations indices
int	neq		# number of equations

int	i, j, sym, nerrors
pointer	errcode
real	rms, det
int	pr_gsym()
pointer	pr_gsymp()
real	pr_eval(), ph_accum(), ph_incrms()

include "invert.com"

begin
	# Evaluate the reference and error equations.
	nerrors = 0
	do i = 1, neq {

	    sym = pr_gsym (eqindex[i], PTY_TRNEQ)
	    Memr[py+i-1] = pr_eval (pr_gsymp (sym, PTEQRPNREF), a,
	        Memr[params[eqindex[i]]])
	    if (IS_INDEFR(Memr[py+i-1]))
		return (0)

	    errcode = pr_gsymp (sym, PTEQRPNERROR)
	    if (errcode == NULL)
		Memr[pyerr+i-1] = 0.0 
	    else {
	        Memr[pyerr+i-1] = pr_eval (errcode, a,
		    Memr[params[eqindex[i]]])
	        if (IS_INDEFR(Memr[pyerr+i-1]))
		    Memr[pyerr+i-1] = 0.0 
	        else
		    nerrors = nerrors + 1
	    }
	}

	# Return if there are no error equations.
	if (nerrors <= 0)
	    return (0)

	# Compute each equations contribution to the to the total error.
	call aclrr (errors, nstd)
	call aclrr (serrors, nset)
	do i = 1, neq {

	    # Add in the appropriate error term.
	    if (Memr[pyerr+i-1] <= 0.0)
		next
	    call amovr (a[nobs+1], Memr[pafit], nstd)
	    Memr[py+i-1] = Memr[py+i-1] + Memr[pyerr+i-1]

	    # Accumulate the matrices and vectors, do the inversion, and
	    # compute the new parameter increments.
	    rms = ph_accum (params, Memr[py], Memr[pyfit], eqindex, neq, a,
	        nobs, deltaa, aindex, Memr[pda], nstd, Memd[palpha],
		Memr[pbeta], Memi[pik], Memi[pjk], nustd, det)

	    # Compute the contribution to the sum of the squares of the errors.
	    #if ((! IS_INDEFR(rms)) && (abs (det) >= DET_TOL)) {
	    if (! IS_INDEFR(rms)) {
	        rms = ph_incrms (params, Memr[py], Memr[pyfit], eqindex, neq,
		    a, nobs, Memr[pda], aindex, nstd, rms)
		do j = 1, nstd {
		    if (aindex[j] == 0)
			next
		    errors[j] = errors[j] + (a[nobs+j] - Memr[pafit+j-1]) ** 2
		}
		do j = 1, nset {
		    serrors[j] = serrors[j] + (pr_eval (seteqn[j], a,
		        Memr[params[1]]) - setvals[j]) ** 2
		}
	    }

	    # Reset the error term.
	    Memr[py+i-1] = Memr[py+i-1] - Memr[pyerr+i-1]
	    call amovr (Memr[pafit], a[nobs+1], nstd)
	}

	# Compute the errors themselves.
	do i = 1, nstd {
	    if (errors[i] <= 0.0)
		errors[i] = 0.0
	    else
	        errors[i] = sqrt (errors[i])
	}
	do i = 1, nset {
	    if (serrors[i] <= 0.0)
		serrors[i] = 0.0
	    else
	        serrors[i] = sqrt (serrors[i])
	}


	return (nerrors)
end


# PH_IERVAL -- Compute error estimates for the photometric indices based
# on the value of any user-supplied errors equations.

int procedure ph_ierval (params, a, eindex, nobs, deltaa, aindex, errors,
	nstd, seteqn, setvals, serrors, nset, nustd, eqindex, neq)

pointer	params[ARB]	# input array of pointers to the fitted parameters
real	a[ARB]		# array of observed and catalog variables
int	eindex[ARB]	# indices of observed and catalog variables with errors
int	nobs		# the number of observed variables
real	deltaa[ARB]	# array of increments for the catalog variables
int	aindex[ARB]	# list of active catalog variables
real	errors[ARB]	# output array of error estimates
int	nstd		# number of catalog variables to be fit
int	seteqn[ARB]	# array of set equation codes
real	setvals[ARB]	# the set equation values
real	serrors[ARB]	# output array of set equation errors
int	nset		# the number of set equations
int	nustd		# the number of active catalog variables
int	eqindex[ARB]	# array of equation indices
int	neq		# number of equations

int	i, j, k, sym, nerrors
real	atemp, rms, det
int	pr_gsym()
pointer	pr_gsymp()
real	pr_eval(), ph_accum(), ph_incrms()

include "invert.com"

begin
	# Initialize.
	nerrors = 0
	call aclrr (errors, nstd)
	call aclrr (serrors, nset)

	# Loop over the observational variables.
	do j = 1, nobs {

	    # Use only variables with errors.
	    if (eindex[j] <= 0)
		next
	    nerrors = nerrors + 1
	    if (IS_INDEFR(a[eindex[j]]) || (a[eindex[j]] <= 0.0))
		next

	    atemp = a[j]
	    a[j] = a[j] + a[eindex[j]]
	    call amovr (a[nobs+1], Memr[pafit], nstd)

	    # Evaluate the reference equations.
	    do i = 1, neq {
	        sym = pr_gsym (eqindex[i], PTY_TRNEQ)
	        Memr[py+i-1] = pr_eval (pr_gsymp (sym, PTEQRPNREF), a,
	            Memr[params[eqindex[i]]])
	        if (IS_INDEFR(Memr[py+i-1]))
		    return (0)
	    }

	    # Accumulate the matrices and vectors, do the inversion, and
	    # compute the new parameter increments.
	    #call eprintf ("errors before accum\n")
	    rms = ph_accum (params, Memr[py], Memr[pyfit], eqindex, neq, a,
	        nobs, deltaa, aindex, Memr[pda], nstd, Memd[palpha],
		Memr[pbeta], Memi[pik], Memi[pjk], nustd, det)
	    #call eprintf ("errors after accum\n")

	    # Compute the contribution to the sum of the squares of the errors.
	    #if ((! IS_INDEFR(rms)) && (abs (det) >= DET_TOL)) {
	    if (! IS_INDEFR(rms)) {
	        #call eprintf ("errors before accum\n")
	        rms = ph_incrms (params, Memr[py], Memr[pyfit], eqindex, neq,
		    a, nobs, Memr[pda], aindex, nstd, rms)
	        #call eprintf ("errors before accum\n")

		do k = 1, nstd {
		    if (aindex[k] <= 0)
			next
		    errors[k] = errors[k] + (a[nobs+k] - Memr[pafit+k-1]) ** 2
		}

	        a[j] = atemp
		do k = 1, nset {
		    serrors[k] = serrors[k] + (pr_eval (seteqn[k], a,
		        Memr[params[1]]) - setvals[k]) ** 2
		}
	        call amovr (Memr[pafit], a[nobs+1], nstd)

	    } else {

	        a[j] = atemp
	        call amovr (Memr[pafit], a[nobs+1], nstd)
	    }
	}

	# Compute the errors themselves.
	do i = 1, nstd {
	    if (errors[i] <= 0.0)
		errors[i] = 0.0
	    else
	        errors[i] = sqrt (errors[i])
	}
	do i = 1, nset {
	    if (serrors[i] <= 0.0)
		serrors[i] = 0.0
	    else
	        serrors[i] = sqrt (serrors[i])
	}

	return (nerrors)
end


# PH_ERVAL -- Compute the error in an equation by summing the effects of errors
# in the observational variables.

real procedure ph_erval (symfit, fitval, params, a, aindex, eindex, nobsvars)

pointer	symfit			# pointer to the fit equation
real	fitval			# the best fit value
real	params[ARB]		# the fitted equation parameters
real	a[ARB]			# the observed and catalog variable values
int	aindex[ARB]		# the list of active variables
int	eindex[ARB]		# the list 
int	nobsvars		# the number of observed variables

int	i, nerrors
real	errval, afit
real	pr_eval()

begin
	# Initialize.
	nerrors = 0
	errval = 0.0

	# Loop over the observed variables.
	do i = 1, nobsvars {

	    # Skip observed variables with no errors.
	    if (eindex[i] <= 0)
		next
	    nerrors = nerrors + 1
	    if ((IS_INDEFR(a[eindex[i]])) || (a[eindex[i]] <= 0.0))
		next

	    # Evaluate the contribution of each observed variable to the
	    # total error.
	    afit = a[i]
	    a[i] = a[i] + a[eindex[i]]
	    errval = errval + (pr_eval (symfit, a, params) - fitval) ** 2
	    a[i] = afit
	}

	if (nerrors <= 0)
	    return (INDEFR)
	else
	    return (sqrt (errval))
end
