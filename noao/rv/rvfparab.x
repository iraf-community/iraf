include <math.h>
include <gset.h>
include <math/nlfit.h>
include "rvpackage.h"
include "rvflags.h"

# RV_FPARAB - Fit a parabola to the specified function.  Compute and return
# an array of the fitted parabola at the specified resolution in ccf[].
# 'c' contains the coefficients of the fit. 

procedure rv_fparab (rv, xcf, ycf, ledge, redge, npts, ishift, c, sigma)

pointer	rv				#I RV struct pointer
real	xcf[npts], ycf[npts]		#I CCF array
int	ledge, redge			#I Index of left edge
int 	npts				#I Number of points
int	ishift				#I initial shift index
real	c[NPARS]			#O Array of coefficients
real	sigma				#O Error of position (pixels)

pointer	sp, gp, nl, list, w, ipx, ipy, fit
int	i, j, stat, npar, il, ir, rnpts
int	ft_func, ft_dfunc
real	center, oldcenter, width, distance
real	ce[3], diff

extern 	polyfit(), dpolyfit()
real	fit_weight()
int	locpr()

include "fitcom.com"
define	NPARS			3

begin
	call smark (sp)
	call salloc (list, NPARS, TY_INT)
	call salloc (ipx, NPARS, TY_REAL)
	call salloc (ipy, NPARS, TY_REAL)
	call salloc (w, npts, TY_REAL)
	call salloc (fit, npts, TY_REAL)

	gp = RV_GP(rv)
	if (gp != NULL && RV_INTERACTIVE(rv) == YES) {
	    call gseti (gp, G_WCS, 2)
	    call gpmark (gp, xcf[ledge], ycf[ledge], npts, 4, 2., 2.)
	    call gflush (gp)
	}

	# Initialize the parameters.
	il = ishift - 1
	ir = ishift + 1
	call amovr (xcf[il], Memr[ipx], NPARS)
	call amovr (ycf[il], Memr[ipy], NPARS)
	call parab (Memr[ipx], Memr[ipy], c)
	call aclrr (ce, NPARS)

	# Initialize the list of params to fit.
	Memi[list] = 1
	Memi[list+1] = 2
	Memi[list+2] = 3

	if (DBG_DEBUG(rv) == YES && DBG_FD(rv) != NULL) {
	    call d_printf (DBG_FD(rv), "\nrv_fparab:\n\t")
	    call d_printf (DBG_FD(rv), "init c[1-3] = %.6g %.6g %.6g\n")
		call pargr (c[1]) ; call pargr (c[2]) ; call pargr (c[3])
	    call d_flush (DBG_FD(rv))
	}

	# Now iterate the fit.
	j = 1
	oldcenter = 0.0
	center = xcf[ishift]
	width = npts
	rnpts = npts * 1000
	ft_func = locpr (polyfit)
	ft_dfunc = locpr (dpolyfit)
	while (j < RV_MAXITERS(rv)) {

            # Move data window if necessary; only one pixel per iteration.
	    if (j > 1 && c[3] != 0.0) {
	        center = abs (-c[2] / (2. * c[3]))
		diff = (oldcenter - center)
                if (diff > 1 && ledge > 1)
                    ledge = ledge - 1
                else if (diff < -1 && (ledge+npts) < RV_CCFNPTS(rv))
                    ledge = ledge + 1
            }

	    # Compute the point weighting.
	    do i = 0, npts-1 {
	        distance = abs (center - xcf[ledge+i])
	        Memr[w+i] = fit_weight (distance, width, RV_WEIGHTS(rv))
                if (DEBUG(rv)) {
                    call d_printf (DBG_FD(rv),"\tx=%g y=%g dist=%g weight=%g\n")
                        call pargr(xcf[ledge+i-1]) ; call pargr(ycf[ledge+i-1])
                        call pargr (distance) ; call pargr(Memr[w+i-1])
                }
	    }

	    # Now do the NLFIT initializations and fit.
	    call nlinitr (nl, ft_func, ft_dfunc, c, ce, NPARS, Memi[list], 
		NPARS, RV_TOLERANCE(rv), RV_MAXITERS(rv))
	    call nlfitr (nl, xcf[ledge], ycf[ledge], Memr[w], npts, 1, 
		WTS_USER, stat)
	    call nlvectorr (nl, xcf[ledge], Memr[fit], npts, 1)
	    call nlpgetr (nl, c, npar)
	    call nlerrorsr (nl, ycf[ledge], Memr[fit], Memr[w], npts, ccfvar,
	        chisqr, ce)
	    call nlfreer (nl) 				# free the NLFIT struct
	    
	    # Now check for convergence.
	    if (c[3] != 0.0)
	        center = abs (-c[2] / (2. * c[3]))
	    if (j == 1) 				# initialize
		oldcenter = center
	    else if (abs(center - oldcenter) < 0.001) 	# converged
		break
	    else
	        oldcenter = center

	    j = j + 1					# next iteration
	}
	niter = j
	nfit = nint (width)
	nfitpars = NPARS
	if (ce[3] != 0.0)
	    sigma = abs (-ce[2] / (2. * ce[3]))
	call amovr (ce, ECOEFF(rv,1), NPARS)

	if (DBG_DEBUG(rv) == YES && DBG_LEVEL(rv) >= 2 && DBG_FD(rv) != NULL) {
	    call d_printf (DBG_FD(rv), "\tfitted c[1-3] = %.6g %.6g %.6g\n")
		call pargr (c[1]) ; call pargr (c[2]) ; call pargr (c[3])
	    call d_printf (DBG_FD(rv), "\tfitted ce[1-3] = %.6g %.6g %.6g\n")
		call pargr (ce[1]) ; call pargr (ce[2]) ; call pargr (ce[3])
	    call flush (DBG_FD(rv))
	}

	call sfree (sp)
end


# PARAB -- Fit a parabola to three points - used to get a first pass at the
# coefficients.

procedure parab (x, y, c)

real	x[NPARS], y[NPARS]			#I Input (x,y) data pairs
real	c[NPARS]				#O Parabola coefficients

begin
	c[3] = (y[1]-y[2]) * (x[2]-x[3]) / (x[1]-x[2]) - (y[2]-y[3])
	c[3] = c[3] / ((x[1]**2-x[2]**2) * (x[2]-x[3]) / (x[1]-x[2]) -
		(x[2]**2-x[3]**2))

	c[2] = (y[1] - y[2]) - c[3] * (x[1]**2 - x[2]**2)
	c[2] = c[2] / (x[1] - x[2])

	c[1] = y[1] - c[2] * x[1] - c[3] * x[1]**2
end
