include <mach.h>
include "../lib/parser.h"

# PH_IVINIT -- Preallocate the space required by the inversion code.

procedure ph_ivinit (nstd, nustd, neq)

int	nstd		# number of catalog variables
int	nustd		# number of catalog variables to be fit
int	neq		# number of equations

include "invert.com"

begin
	call malloc (py, neq, TY_REAL)
	call malloc (pyfit, neq, TY_REAL)
	call malloc (pa, nstd, TY_REAL)
	call malloc (pdelta, nstd, TY_REAL)
	call malloc (pda, nstd, TY_REAL)

	call malloc (palpha, nustd * nustd, TY_DOUBLE)
	call malloc (pbeta, nustd, TY_REAL)
	call malloc (pik, nustd, TY_INT)
	call malloc (pjk, nustd, TY_INT)

	call malloc (pyerr, neq, TY_REAL)
	call malloc (pafit, nstd, TY_REAL)
end


# PH_IVFREE -- Free the space required by the inversion code.

procedure ph_ivfree ()

include "invert.com"

begin
        call mfree (py, TY_REAL)
        call mfree (pyfit, TY_REAL)
        call mfree (pa, TY_REAL)
        call mfree (pdelta, TY_REAL)
        call mfree (pda, TY_REAL)

        call mfree (palpha, TY_DOUBLE)
        call mfree (pbeta, TY_REAL)
        call mfree (pik, TY_INT)
        call mfree (pjk, TY_INT)

        call mfree (pyerr, TY_REAL)
        call mfree (pafit, TY_REAL)
end


# PH_OBJCHECK -- Check that the equations for this particular star are
# invertable.

int procedure ph_objcheck (params, a, vartable, nstdvars, nreq, eqset,
	maxnset, vindex, nvar, eqindex, neq)

pointer	params[ARB]		# array of pointers to the fitted parameters
real	a[ARB]			# array of observed and catalog variables
int	vartable[nstdvars,ARB]	# table of variables as a function of equation
int	nstdvars		# the total number of catalog variables
int	nreq			# the total number of reference equations
int	eqset[maxnset,ARB]	# set equation table
int	maxnset			# maximum number of set equations
int	vindex[ARB]		# output index of variables
int	nvar			# number of variables used in the equations
int	eqindex[ARB]		# output index of equations
int	neq			# number of equations to be reduced

int	i, j, sym, ncat, nset
real	rval
int	pr_gsym()
pointer	pr_gsymp()
real	pr_eval()

begin
	# Initialize
	call aclri (vindex, nstdvars)
	call aclri (eqindex, nreq)

	# Evalute the reference equations.
	neq = 0
	do i = 1, nreq {
	    sym = pr_gsym (i, PTY_TRNEQ)
	    rval = pr_eval (pr_gsymp (sym, PTEQRPNREF), a, Memr[params[i]])
	    if (IS_INDEFR(rval))
		next
	    neq = neq + 1
	    eqindex[neq] = i
	}

	# If there is no data return.
	if (neq <= 0)
	    return (ERR)

	# Determine which variables are used by the reduced set of equations.
	do i = 1, neq {
	    do j = 1, nstdvars {
		if (vartable[j,eqindex[i]] == 0)
		    next
		vindex[j] = vartable[j,eqindex[i]]
	    }
	}

	# Deterine which set equations are used by the reduced set of equations
	nset = 0
	do j = 1, maxnset {
	    do i = 1, neq {
		if (eqset[j,eqindex[i]] == 0)
		    next
		nset = nset + 1
		break
	    }
	}

	# Count the number of variables.
	nvar = 0
	ncat = 0
	do j = 1, nstdvars {
	    if (vindex[j] == 0)
		next
	    nvar = nvar + 1
	    if (vindex[j] > 0)
		ncat = ncat + 1
	}

	if ((ncat + nset) > neq)
	    return (ERR)

	return (OK)
end


define	MAX_NITER1	10		# maximum number of iterations
define	MAX_NITER2	50		# maximum number of trials
define	DET_TOL		1.0E-20		# minimum value of the determinant
#define	DET_TOL		0.0		# minimum value of the determinant
define	MIN_DELTA	0.01		# minimum absolute value of
					# parameter increments

# PH_INVERT -- Invert the transformation to compute the standard indices.

int procedure ph_invert (params, a, nobs, deltaa, aindex, nstd,
	nustd, eqindex, nueq)

pointer	params[ARB]	# input array of pointers to the fitted parameters
real	a[ARB]		# array of observed and catalog variables
int	nobs		# the number of observed variables
real	deltaa[ARB]	# array of increments for the catalog variables
int	aindex[ARB]	# index of active catalog variables
int	nstd		# total number of catalog variables
int	nustd		# number of catalog variables to be fit
int	eqindex[ARB]	# the equation index
int	nueq		# total number of equations used

int	i, sym, niter
real	stdev1, stdev2, det, rms
int	pr_gsym()
pointer	pr_gsymp()
real	pr_eval(), ph_accum(), ph_incrms()

include "invert.com"

begin
	# Evalute the reference equations.
	do i = 1, nueq {
	    sym = pr_gsym (eqindex[i], PTY_TRNEQ)
	    Memr[py+i-1] = pr_eval (pr_gsymp (sym, PTEQRPNREF), a,
	        Memr[params[eqindex[i]]])
	    if (IS_INDEFR(Memr[py+i-1]))
		return (ERR)
	}

	# Initialize the parameter increments. This will be incremented
	# each time through the fitting loop.
	call amovr (deltaa, Memr[pdelta], nstd)

	# Accumulate the matrices and vectors, do the inversion and compute
	# the new parameter increments. The fit will terminate when the
	# determinant of the inversion matrix becomes < 1.0e-20, if the
	# standard deviation of the fit begins to increase, if the rms of
	# the fit < EPSILONR, or the maximum number of iterations is
	# exceeded, whichever comes first. 

	niter = 0

	repeat {

	    # Compute the curvature currection. Return INDEFR if there are
	    # bad data in the fit. Terminate the fit if the determinant of
	    # the curvature matrix is too close to zero.

	    stdev1 = ph_accum (params, Memr[py], Memr[pyfit], eqindex, nueq,
	        a, nobs, Memr[pdelta], aindex, Memr[pda], nstd,
		Memd[palpha], Memr[pbeta], Memi[pik], Memi[pjk], nustd, det)

	    #call eprintf ("acc: niter=%d det=%g stdev1=%g\n")
		#call pargi (niter+1)
		#call pargr (det)
		#call pargr (stdev1)

	    # Return if there is INDEF data in the fit.
	    if (IS_INDEFR(stdev1))
	        return (ERR)

	    # Check the size of the determinant but force at least one fit.
	    if ((abs (det) < DET_TOL) && (niter > 0))
		break

	    # Find the new parameter values.
	    call amovr (a, Memr[pa], nstd)
	    stdev2 = ph_incrms (params, Memr[py], Memr[pyfit], eqindex, nueq,
	        a, nobs, Memr[pda], aindex, nstd, stdev1)

	    #call eprintf ("inc: niter=%d det=%g stdev2=%g\n")
		#call pargi (niter+1)
		#call pargr (det)
		#call pargr (stdev2)

	    # Check the new values.
	    if (IS_INDEFR(stdev2) || (stdev2 >= stdev1)) {
		if (niter == 0)
	            return (ERR)
		else {
		    call amovr (Memr[pa], a, nstd)
		    return (OK)
		}
	    }

	    # User the new deltas and increment the fit counters.
	    call anegr (Memr[pda], Memr[pdelta], nstd)
	    call ph_deltamin (Memr[pdelta], MIN_DELTA, Memr[pdelta], nstd)
	    niter = niter + 1
	    rms = sqrt (stdev2)

	} until ((niter == MAX_NITER1) || (rms <= EPSILONR))

	return (OK)
end


# PH_ACCUM -- Accumulate the matrix of second derivatives and vector
# of first derivatives  required for parabolic expansion of the rms
# non-linear least squares fitting technique. This code is a modification
# of the Bevington CHIFIT subroutine where the reduced chi-squared has
# been replaced by the rms.  The original CHIFIT cannot be used to fit the
# case when there are n data points and n unknowns since the number of
# degrees of freedom is  zero and hence so is the reduced chi-squared.

real procedure ph_accum (params, y, yfit, eqindex, neq, a, nobs, deltaa,
	aindex, da, nstd, alpha, beta, ik, jk, nustd, det)

pointer	params[ARB]	    # input array of ptrs to the fitted parameters
real	y[ARB]		    # array of reference equation values
real	yfit[ARB]	    # array of fitted reference equation values
int	eqindex[ARB]	    # the equation indices
int	neq		    # number of equations to be inverted
real	a[ARB]		    # array of observed and catalog variables
int	nobs		    # the number of observed variables
real	deltaa[ARB]	    # array of increments for the catalog variables
int	aindex[ARB]	    # index of active catalog variables
real	da[ARB]		    # new array of parameter increments
int	nstd		    # number of catalog variables
double	alpha[nustd,ARB]    # the matrix of second derivatives
real	beta[ARB]	    # the vector of first derivatives
int	ik[ARB]		    # working array for the matrix inversion
int	jk[ARB]		    # working array for the matrix inversion
int	nustd		    # The number of catalog variables to be fit
real	det		    # determinant of inverted matrix

int	i, j, k, nj, nk, sym
real	aj, ak, rms1, rms2, rms3
int	pr_gsym()
pointer	pr_gsymp()
real	pr_eval()

begin
	# Compute the initial rms, checking for INDEF valued variables.
	rms1 = 0.0
	do i = 1, neq {
	    sym = pr_gsym (eqindex[i], PTY_TRNEQ)
	    yfit[i] = pr_eval (pr_gsymp (sym, PTEQRPNFIT), a,
	        Memr[params[eqindex[i]]])
	    if (IS_INDEFR(yfit[i]))
		return (INDEFR)
	    rms1 = rms1 + (y[i] - yfit[i]) ** 2
	}
	rms1 = rms1 / neq

	nj = 0
	do j = 1, nstd {

	    # Check the status of the parameter.
	    if (aindex[j] == 0)
		next
	    nj = nj + 1

	    # Increment each parameter.
	    aj = a[j+nobs]
	    a[j+nobs] = aj + deltaa[j]

	    # Compute a new rms.
	    rms2 = 0.0
	    do i = 1, neq {
	        sym = pr_gsym (eqindex[i], PTY_TRNEQ)
	        yfit[i] = pr_eval (pr_gsymp (sym, PTEQRPNFIT), a,
		    Memr[params[eqindex[i]]])
	        rms2 = rms2 + (y[i] - yfit[i]) ** 2
	    }
	    rms2 = rms2 / neq

	    # Begin accumulating the diagonal elements .
	    alpha[nj,nj] = rms2 - 2.0 * rms1
	    beta[nj] = -rms2

	    # Accumulate the non-diagonal elements.
	    nk = 0
	    do k = 1, nstd {

		if (aindex[k] == 0)
		    next
		nk = nk + 1

		if ((nk - nj) == 0)
		    next
	        else if ((nk - nj) < 0) {
		    alpha[nk,nj] = (alpha[nk,nj] - rms2) / 2.0
		    alpha[nj,nk] = alpha[nk,nj]
		    next
	        }

		alpha[nj,nk] = rms1 - rms2
		ak = a[k+nobs]
		a[k+nobs] = ak + deltaa[k]

	        rms3 = 0.0
	        do i = 1, neq {
	            sym = pr_gsym (eqindex[i], PTY_TRNEQ)
	            yfit[i] = pr_eval (pr_gsymp (sym, PTEQRPNFIT), a,
		        Memr[params[eqindex[i]]])
	            rms3 = rms3 + (y[i] - yfit[i]) ** 2
	        }
	        rms3 = rms3 / neq

		alpha[nj,nk] = alpha[nj,nk] + rms3
		a[k+nobs] = ak
	    }

	    # Continue accumulating the diagonal elements.
	    a[j+nobs] = aj - deltaa[j]
	    rms3 = 0.0
	    do i = 1, neq {
	        sym = pr_gsym (eqindex[i], PTY_TRNEQ)
	        yfit[i] = pr_eval (pr_gsymp (sym, PTEQRPNFIT), a,
		    Memr[params[eqindex[i]]])
	        rms3 = rms3 + (y[i] - yfit[i]) ** 2
	    }
	    rms3 = rms3 / neq

	    a[j+nobs] = aj
	    alpha[nj,nj] = (alpha[nj,nj] + rms3) / 2.0
	    beta[nj] = (beta[nj] + rms3) / 4.0
	}

	# Eliminate any curvature from the matrix of second derivatives.
	do j = 1, nj {
	    if (alpha[j,j] > 0.0)
		next
	    if (alpha[j,j] < 0.0)
		alpha[j,j] = -alpha[j,j]
	    else
		alpha[j,j] = 0.01
	    do k = 1, nk {
		if ((k - j) == 0)
		    next
		alpha[j,k] = 0.0
		alpha[k,j] = 0.0
	    }
	}

	# Invert the matrix.
	call phminv (alpha, ik, jk, nj, det)

	# Increment the parameters.
	nj = 0
	do j = 1, nstd {
	    da[j] = 0.0
	    if (aindex[j] == 0)
		next
	    nj = nj + 1
	    nk = 0
	    do k = 1, nstd {
	        if (aindex[k] == 0)
		    next
		nk = nk + 1
		da[j] = da[j] + beta[nk] * alpha[nj,nk]
	    }
	    da[j] = 0.2 * da[j] * deltaa[j]
	}

	# If the determinate is too small increment the parameters by
	# deltas and try again.
	#if (abs (det) < DET_TOL) {
	    #call eprintf ("using approx\n")
	    #do j = 1, nstd {
	        #if (aindex[j] == 0) {
		    #da[j] = 0.0
		    #next
		#}
		#call eprintf ("i=%d dain=%g ")
		    #call pargi (j)
		    #call pargr (da[j])
		#if (da[j] > 0.0)
		    #da[j] = abs (deltaa[j])
		#else if (da[j] < 0.0)
		    #da[j] = -abs (deltaa[j])
		#else
		    #da[j] = 0.0
		#call eprintf ("daout=%g\n")
		    #call pargr (da[j])
	    #}
	#}

	return (rms1)
end


# PH_INCRMS -- Increment the parameters until three values of the rms are found
# which bracket the best fitting data point and fit a parabola to the three
# different rms points.

real procedure ph_incrms (params, y, yfit, eqindex, neq, a, nobs, da, aindex,
	nstd, rms1)

pointer	params[ARB]	# input array of ptrs to the fitted parameters
real	y[ARB]		# the array of reference equation values
real	yfit[ARB]	# the array of fitted reference equation values
int	eqindex[ARB]	# list of active equations
int	neq		# the number of equations
real	a[ARB]		# array of observed and fitted variables
int	nobs		# number of observed variables
real	da[ARB]		# the parameter increments
int	aindex[ARB]	# the index of active catalog variables
int	nstd		# number of catalog variables
real	rms1		# the first rms point

int	j, i, sym, niter
real	orms2, rms2, orms3, rms3, delta, rms
int	pr_gsym()
pointer	pr_gsymp()
real	pr_eval()

begin
	# Adjust the parameters.
	rms = rms1
	do j = 1, nstd {
	    if (aindex[j] == 0)
		next
	    a[j+nobs] = a[j+nobs] + da[j]
	}

	# Alter the parameter increments until the rms starts to decrease.
	orms2 = MAX_REAL
	niter = 0
	repeat {

	    rms2 = 0.0
	    do i = 1, neq {
	        sym = pr_gsym (eqindex[i], PTY_TRNEQ)
	        yfit[i] = pr_eval (pr_gsymp (sym, PTEQRPNFIT), a,
		    Memr[params[eqindex[i]]])
	        rms2 = rms2 + (y[i] - yfit[i]) ** 2
	    }
	    rms2 = rms2 / neq
	    #call eprintf ("    niter=%d rms1=%g rms2=%g\n")
		#call pargi (niter)
		#call pargr (rms1)
		#call pargr (rms2)
	    if (rms2 <= 0.0)
		break
	    if ((rms1 - rms2) >= 0.0)  
		break

	    # If rms2 does not decrease and does not change from one iteration
	    # to the next we are probably near the precision limits of the
	    # computer or the computed curvature has the wrong sign. In that
	    # case quit.

	    if (orms2 < MAX_REAL) {
		if (abs ((rms2 - orms2) / orms2) < EPSILONR)
		    return (rms)
	    }

	    orms2 = rms2
	    niter = niter + 1
	    if (niter >= MAX_NITER2)
		return (rms)

	    do j = 1, nstd {
	        if (aindex[j] == 0)
		    next
		#da[j] = da[j] / 2.0
		a[j+nobs] = a[j+nobs] - da[j]
	    }


	}

	# Alter the parameter increments until the rms starts to increase.
	orms3 = MAX_REAL
	niter = 0
	repeat {

	    do j = 1, nstd {
		if (aindex[j] == 0)
		    next
	        a[j+nobs] = a[j+nobs] + da[j]
	    }

	    rms3 = 0.0
	    do i = 1, neq {
	        sym = pr_gsym (eqindex[i], PTY_TRNEQ)
	        yfit[i] = pr_eval (pr_gsymp (sym, PTEQRPNFIT), a,
		    Memr[params[eqindex[i]]])
	        rms3 = rms3 + (y[i] - yfit[i]) ** 2
	    }
	    rms3 = rms3 / neq
	    #call eprintf ("    niter=%d rms1=%g rms2=%g rms3=%g\n")
		#call pargi (niter)
		#call pargr (rms1)
		#call pargr (rms2)
		#call pargr (rms3)
	    if ((rms3 - rms2) >= 0.0)
		break

	    if (orms3 < MAX_REAL) {
		if (abs ((rms3 - orms3) / orms3) < EPSILONR)
		    return (rms)
	    }

	    niter = niter + 1
	    if (niter >= MAX_NITER2)
		return (rms)


	    orms3 = rms3
	    rms1 = rms2
	    rms2 = rms3

	}

	# Fit a parabola to the three values of the rms that bracket the fit.
	if (rms3 <= rms2)
	    delta = 1.0
	else
	    delta = 1.0 / (1.0 + (rms1 - rms2) / (rms3 - rms2)) + 0.5
	do j = 1, nstd {
	    if (aindex[j] == 0)
		next
	    a[nobs+j] = a[nobs+j] - delta * da[j]
	}

	rms = 0.0
	do i = 1, neq {
	    sym = pr_gsym (eqindex[i], PTY_TRNEQ)
	    yfit[i] = pr_eval (pr_gsymp (sym, PTEQRPNFIT), a,
	        Memr[params[eqindex[i]]])
	    rms = rms + (y[i] - yfit[i]) ** 2
	}
	rms = rms / neq

	if ((rms2 - rms) < 0.0) {
	    do j = 1, nstd {
	        if (aindex[j] == 0)
		    next
	        a[nobs+j] = a[nobs+j] + (delta - 1.) * da[j]
	    }
	    do i = 1, neq {
	        sym = pr_gsym (eqindex[i], PTY_TRNEQ)
	        yfit[i] = pr_eval (pr_gsymp (sym, PTEQRPNFIT), a,
		    Memr[params[eqindex[i]]])
	    }
	    rms = rms2
	}

	#call eprintf ("    incr: rms1=%g rms2=%g rms3=%g rms=%g\n")
	    #call pargr (rms1)
	    #call pargr (rms2)
	    #call pargr (rms3)
	    #call pargr (rms)

	return (rms)
end


# PH_DELTAMIN -- Check to make sure that the absolute value of the deltaa
# is always greater than or equal to min_delta.

procedure ph_deltamin (a, min_delta, b, npix)

real	a[ARB]			# input vector
real	min_delta		# minimum permitted absolute value 
real	b[ARB]			# output vector
int	npix			# number of points

int	i

begin
	do i = 1, npix {
	    if (abs(a[i]) >= min_delta)
		b[i] = a[i]
	    else if (a[i] < 0.0)
		b[i] = -min_delta
	    else
		b[i] = min_delta
	}
end
