include <math.h>
include <gset.h>
include <math/nlfit.h>
include "rvpackage.h"
include "rvflags.h"

# RV_FGAUSS - Fit a Gaussian to the specified function.  Compute and return
# an array of the fitted gaussian at the specified resolution in ccf[].
# 'c' contains the coefficients of the fit. 'ishift' is used as an initial
# guess at the center parameter, c[2]. 

procedure rv_fgauss (rv, xcf, ycf, ledge, redge, npts, ishift, c, sigma)

pointer	rv				#I RV struct pointer
real	xcf[ARB], ycf[ARB]		#I CCF array
int	ledge, redge			#I Index of left edge
int 	npts				#I Number of points
int	ishift				#I Initial shift guess
real	c[ARB]				#O Array of coefficients
real	sigma				#O Error of center position

pointer	sp, gp, nl, w, list, fit
real	distance, width, sigmac[NPARS], oldc2, diff
int	ft_func, ft_dfunc
int	i, j, stat, npar, nvars
long	lseed
bool	reset_c1

extern	cgauss1d(), cdgauss1d()
extern	lorentz(), dlorentz()
extern	lorentz_old(), dlorentz_old()
real	fit_weight(), rv_maxpix()
int	locpr(), check_converge(), rv_fitconv()

include "fitcom.com"
define	NPARS			4

begin
	call smark (sp)
	call salloc (w, npts, TY_REAL)
	call salloc (fit, npts, TY_REAL)
	call salloc (list, NPARS, TY_INT)
	call aclrr(Memr[w], npts)
	call aclrr(Memr[fit], npts)
	call aclri(Memi[list], NPARS)
	call aclrr(sigmac, NPARS)
	call aclrr(c, NPARS)

	# Mark the points being used in the fit.
	gp = RV_GP(rv)
	if (gp != NULL && RV_INTERACTIVE(rv) == YES) {
	    call gseti (gp, G_WCS, 2)
	    call gpmark (gp, xcf[ledge], ycf[ledge], npts, 4, 2., 2.)
	    call gflush (gp)
	}

	# Initialize the parameters.
	if (DBG_DEBUG(rv) == YES) {
	    call d_printf (DBG_FD(rv), "rv_fgauss:\tFunction = %d\n")
		call pargi(RV_FITFUNC(rv))
	}
	call init_gcoeffs (rv, xcf, ycf, ledge, redge, npts, ishift, c)

	# Set up some of the NLFIT stuff.
	width = npts
	Memi[list] = 1				# amplitude
	Memi[list+1] = 2			# center
	Memi[list+2] = 3			# sigma/fwhm
	if (IS_INDEF(RV_BACKGROUND(rv))) {
	    Memi[list+3] = 4			# background
	    nfitpars = NPARS
	} else
	    nfitpars = NPARS - 1
	
	# Get the function addresses.
	if (RV_FITFUNC(rv) == GAUSSIAN) {
	    ft_func = locpr (cgauss1d)
	    ft_dfunc = locpr (cdgauss1d)
	} else if (RV_FITFUNC(rv) == LORENTZIAN) {
	    ft_func = locpr (lorentz)
	    ft_dfunc = locpr (dlorentz)
	}

	# Now iterate the fit.
	j = 1
	oldc2 = c[2]
	nvars = 1
	lseed = 1
	ccfvar = 0.0
	chisqr = 0.0
	while (j < RV_MAXITERS(rv)) {

	    if (j > 1) {
	        # Move data window if necessary; only one pixel per iteration.
		diff = oldc2 - c[2]
		reset_c1 = false
		if (diff > 1 && ledge > 1) {
		    ledge = ledge - 1
		    reset_c1 = true
		} else if (diff < -1 && (ledge+npts) < RV_CCFNPTS(rv)) {
		    ledge = ledge + 1
		    reset_c1 = true
		}
		if (reset_c1) {
	    	    if (!IS_INDEF(RV_BACKGROUND(rv)))
	                c[1] = rv_maxpix (ycf[ledge], npts) - RV_BACKGROUND(rv)
	    	    else
	        	c[1] = rv_maxpix (ycf[ledge], npts)
	    	}

		# Now check to see if we're converging sensibly, and recover
		# by rejecting points or adjusting parameters.
		stat = check_converge (rv, xcf, ycf, ledge, redge, width, 
		    npts, ishift, oldc2, lseed, c)
	    }

	    # Compute the point weighting.
	    do i = 1, npts {
	        distance = abs (c[2] - xcf[ledge+i-1])
	        Memr[w+i-1] = fit_weight (distance, width, RV_WEIGHTS(rv))
                #if (DEBUG(rv)) {
                #   call d_printf (DBG_FD(rv),"\tx=%g y=%g dist=%g weight=%g\n")
                #       call pargr(xcf[ledge+i-1]) ; call pargr(ycf[ledge+i-1]) 
                #       call pargr (distance) ; call pargr(Memr[w+i-1])
                #}
	    }

	    # Initialize the NLFIT routines and do the fitting.
	    call nlinitr (nl, ft_func, ft_dfunc, c, sigmac, NPARS, Memi[list], 
		nfitpars, RV_TOLERANCE(rv), RV_MAXITERS(rv))
	    call nlfitr (nl, xcf[ledge], ycf[ledge], Memr[w], npts, nvars,
		WTS_USER, stat)
	    call nlvectorr (nl, xcf[ledge], Memr[fit], npts, 1)
	    call nlpgetr (nl, c, npar)
	    call nlerrorsr (nl, ycf[ledge], Memr[fit], Memr[w], npts, ccfvar,
	        chisqr, sigmac)
	    call nlfreer (nl)

	    if (DBG_DEBUG(rv) == YES && DBG_FD(rv) != NULL) {
	        call d_printf (DBG_FD(rv),
		    "\titer %d = %.6g %.6g %.6g %.6g chi2=%g o2=%g\n")
		    call pargi (j); call pargr (c[1]) ; call pargr (c[2])
		    call pargr (c[3]) ; call pargr (c[4]); call pargr (chisqr)
		    call pargr (oldc2)
	        call flush (DBG_FD(rv))
	    }

	    # Now check for convergence.
	    if (j == 1) 				# initialize
	    	oldc2 = c[2]
	    else if (abs(c[2] - oldc2) < 0.0001) 	# converged
	    	break
	    else
	    	oldc2 = c[2]

	    j = j + 1					# next iteration
	}

	# See if we couldn't converge
	if (rv_fitconv (rv, j, c) == ERR_FIT) {
             RV_ERRCODE(rv) = ERR_FIT
             call aclrr (c, NPARS)
             call aclrr (sigmac, NPARS)
	     call sfree (sp)
             return
	}
	niter = j
	nfit = width
	sigma = abs (sigmac[2])
	call amovr (sigmac, ECOEFF(rv,1), nfitpars)
	if (!IS_INDEF(RV_BACKGROUND(rv)))
	    ECOEFF(rv,4) = 0.0

	# Debug output.
	if (DBG_DEBUG(rv) == YES && DBG_LEVEL(rv) >= 2 && DBG_FD(rv) != NULL) {
	    call d_printf(DBG_FD(rv),"\tfitted c[1-4] = %.6g %.6g %.6g %.6g\n")
		call pargr (c[1]) ; call pargr (c[2])
		call pargr (c[3]) ; call pargr (c[4])
	    call flush (DBG_FD(rv))
	}

	if (nl != NULL)
	    call nlfreer (nl)
	call sfree (sp)
end


# FIT_WEIGHT - Compute the point weighting, with error checking to avoid
# problems with exponentiation of negative numbers and weights.

real procedure fit_weight (dist, width, wt_exp)

real	dist					#I Distance from center
real	width					#I Width of data window
real	wt_exp					#I Weighting exponent

real	base, weight

begin
	if (wt_exp == 0.0)
	    return (1.0)

	base = max (0.0, (1. - (dist / (width / 2.))))
	if (base > 0.0)
	    weight = base ** wt_exp
	else
	    weight = 0.0

	return (weight)
end


# INIT_GCOEFFS - Initialize the Gaussian/Lorentzian coefficients based on 
# the data.

procedure init_gcoeffs (rv, xcf, ycf, ledge, redge, npts, ishift, c)

pointer	rv				#I RV struct pointer
real	xcf[ARB], ycf[ARB]		#I CCF array
int	ledge, redge			#I Index of left edge
int 	npts				#I Number of points
int	ishift				#I Initial shift guess
real	c[4]				#O Array of initial coefficients

real	y
int	left, right
real	rv_maxpix(), rv_minpix()

begin
	# Initialize the parameters.
	if (!IS_INDEF(RV_BACKGROUND(rv))) {
	    c[1] = rv_maxpix (ycf[ledge], npts) #- RV_BACKGROUND(rv)
	    c[4] = RV_BACKGROUND(rv) 			  # background
	} else {
	    c[1] = rv_maxpix (ycf[ledge], npts)
	    c[4] = rv_minpix (ycf[ledge], npts)
	}
	y = (c[1] - c[4]) / 2. + c[4]
	c[2] = xcf[ishift] - 0.1			  # center
	left = ledge
	right = redge
	while (ycf[left+1] < y && left < ishift)
	    left = left + 1
	while (ycf[right-1] < y && right > ishift)
	    right = right - 1
	if (RV_FITFUNC(rv) == LORENTZIAN) {
	    # Lorentz FWHM
	    #c[3] = max (2.,(xcf[right] - xcf[left] + 1))	
	    c[3] = min (-2.,-(xcf[right] - xcf[left] + 1)/2.0)
	    c[3] = max (2.,(xcf[right] - xcf[left] + 1)/2.0)
	} else {
	    # Sigma ** 2
	    #c[3] = sqrt (xcf[right] - xcf[left] + 1) / 2.35482
	    c[3] = max (2.,(((xcf[right]-xcf[left]+1) / 2.35482) ** 2.))
	}

	if (DBG_DEBUG(rv) == YES) {
	    call d_printf (DBG_FD(rv), "\tinit c[1-4] = %.6g %.6g %.6g %.6g\n")
		call pargr (c[1]) ; call pargr (c[2])
		call pargr (c[3]) ; call pargr (c[4])
	    call d_printf (DBG_FD(rv), "\tr/l=%d/%d xr/l=%f/%f y=%f\n")
		call pargi(right) ; call pargi(left) ; call pargr(xcf[right])
		call pargr(xcf[left])  ;  call pargr(y)
	    call flush (DBG_FD(rv))
	}
end


# CHECK_CONVERGENCE - Check to see if we're converging correctly, otherwise
# reject points.

int procedure check_converge (rv, xcf, ycf, ledge, redge, width, npts, 
    ishift, oldc, lseed, c)

pointer	rv				#I RV struct pointer
real	xcf[ARB], ycf[ARB]		#I CCF array
int	ledge, redge			#I Index of left edge
real 	width				#I Width of fit region
int	npts				#I Number of points
int	ishift				#I Initial shift guess
real	oldc				#I Old center
long	lseed				#I Seed
real	c[ARB]				#O Array of coefficients

int	i
real	urand(), frac

begin
	# Generate a random percentage to nudge the params in case they
	# get lost in parameter space.
	frac = urand (lseed) / 10.0

	# Check for negative sigma ** 2
	if (c[3] <= 0.0 && RV_FITFUNC(rv) == GAUSSIAN) {
	    if (!IS_INDEF(RV_BACKGROUND(rv))) {
		#if (RV_INTERACTIVE(rv) == YES) {
	        #  call rv_errmsg (
	    	#    "Fit not converging: rejecting points below background\n")
		#}
	    	while (ycf[ledge+1] < RV_BACKGROUND(rv) && ledge < ishift)
	    	    ledge = ledge + 1
	    	while (ycf[redge-1] < RV_BACKGROUND(rv) && redge > ishift)
	    	    redge = redge - 1
	    	npts = redge - ledge + 1
	    	if (npts < RV_MINWIDTH(rv))
	    	    return (ERR_FIT)
	    	width = real (npts)
	    }
	    call init_gcoeffs (rv, xcf, ycf, ledge, redge, npts, ishift, c)
	    do i = 2, 3
	    	c[i] = c[i] - (c[i] * frac)	# add some scatter
 	}

	# Now check for a negative amplitude or unusual shift and reset.
	if (abs(c[2]-oldc) >= 5 || c[1] <= 0.0) {
	    call init_gcoeffs (rv, xcf, ycf, ledge, redge, npts, ishift, c)
	    do i = 2, 3
	    	c[i] = c[i] + (c[i] * frac)	# add some scatter
	}

	if (DBG_DEBUG(rv) == YES) {
	    call d_printf (DBG_FD(rv), "\tchk    = %.6g %.6g %.6g %.6g\n")
		call pargr (c[1]) ; call pargr (c[2])
		call pargr (c[3]) ; call pargr (c[4])
	    call flush (DBG_FD(rv))
	}
	return (OK)
end


# RV_FITCONV - Check to see if the fit converged.

int procedure rv_fitconv (rv, niter, coeff)

pointer	rv					#I RV struct pointer
int	niter					#I Number of iterations
real	coeff[4]				#I Coefficient array

begin
	if (niter >= RV_MAXITERS(rv))
	    return (ERR_FIT)
	if (coeff[1] < 0.0)
	    return (ERR_FIT)
	if (coeff[3] < 0.0 && RV_FITFUNC(rv) == GAUSSIAN)
	    return (ERR_FIT)

	return (OK)
end
