include <gset.h>
include "rvpackage.h"
include "rvflags.h"

# RV_FIT - Fit the CCF in the specified region.  Return the exact pixel
# shift and sigma of the fit.

procedure rv_fit (rv, xcf, ycf, ledge, redge, npts, ishift, shift, sigma)

pointer	rv				#I RV struct pointer
real	xcf[ARB], ycf[ARB]		#I Array of correlation peaks
int	ledge				#I Left edge to fit
int	redge				#I Right edge to fit
int	npts				#I Npts between edges
int 	ishift				#I Initial index of shift
real 	shift				#O Computed shift
real	sigma				#O Sigma of fit

pointer	gp
real	hght, init, peak, c[4]
real	a, b, thresh, fwhm
int	i, tnum

real	rv_width(), center1d()
int	rv_getshift()

include "fitcom.com"
include "rvsinc.com"
define	NPARS		4

begin
	# Erase the old fit first.
	call rv_erase_fit (rv, false)
	RV_FITDONE(rv) = NO
	RV_ERRCODE(rv) = OK
	IS_DBLSTAR(rv) = NO

	# Do some window bounds checking.
	if (ledge < (RV_WINCENTER(rv)-RV_WINDOW(rv)) || 
	    redge > (RV_WINCENTER(rv)+RV_WINDOW(rv))) {
		call rv_err_comment (rv, 
		   "WARNING: Some points in fit are outside window bounds.", "")
		if (RV_INTERACTIVE(rv) == YES) {
		   call rv_errmsg (
		       "Warning: Some points in fit are outside window bounds.")
		   call tsleep (1)
		}
	}

	# Save some info
	RV_ISHIFT(rv) = ishift
	RV_ISTART(rv) = ledge
	RV_IEND(rv) = redge
	gp = RV_GP(rv)

	# Initialize some variables
	tnum = RV_TEMPNUM(rv)

	# Do the fitting
	switch (RV_FITFUNC(rv)) {
	case GAUSSIAN:				# call gaussian fitting
	    call rv_fgauss (rv, xcf, ycf, ledge, redge, npts, ishift, c, sigma)
	    if (RV_ERRCODE(rv) == ERR_FIT) {
		if (c[3] <= 0.0) {
		    return
		} else {
		    call rv_draw_fit (rv, gp, NO)
		    return
		}
	    }
	    shift = c[2]
	    if (IS_INDEF(RV_BACKGROUND(rv)))
	        hght = c[1] + c[4]
	    else
		hght = c[1] + RV_BACKGROUND(rv)

	case LORENTZIAN:			# call lorentzian fitting
	    call rv_fgauss (rv, xcf, ycf, ledge, redge, npts, ishift, c, sigma)
	    #c[3] = abs (c[3])
	    if (RV_ERRCODE(rv) == ERR_FIT) {
		call rv_draw_fit (rv, gp, NO)
		return
	    }
	    shift = c[2]
	    call lorentz (shift, 3, c, 4, hght)
	    hght = 2.0 * c[1] / c[3]
	    if (!IS_INDEF(RV_BACKGROUND(rv)))
		hght = hght + RV_BACKGROUND(rv)

	case PARABOLA:			 	# call parabola fitting routine
	    call rv_fparab (rv, xcf, ycf, ledge, redge, npts, ishift, c, sigma)
	    shift = -c[2] / (2. * c[3])
	    hght = c[1] + (c[2] * shift) + (c[3] * shift * shift)
            peak = c[1] - c[2]**2 / (4. * c[3])
            RV_FWHM(rv) = sqrt ( abs(-peak / (2. * c[3])))

	case CENTER1D:
	    init = real (rv_getshift (ycf[ledge], npts, MAXIMUM))
            call alimr (ycf[ledge], npts, a, b)
            #thresh = (b - a) - 0.01
            thresh = 0.0
    	    peak = center1d (init, ycf[ledge], npts, npts/5., 1, 2., thresh)
	    RV_HEIGHT(rv) = INDEF
	    RV_FWHM(rv) = INDEF
	    RV_ERROR(rv) = INDEFD
	    RV_R(rv) = INDEF
	    RV_DISP(rv) = INDEF
	    if (RV_GP(rv) != NULL && RV_INTERACTIVE(rv) == YES) {
	        call gpmark (gp, xcf[ledge], ycf[ledge], npts, 4, 2., 2.)
	        call gflush (gp)
	    }

	    if (IS_INDEF(peak)) {		# check for an error
		RV_ERRCODE(rv) = ERR_FIT
		return
	    } else {
    	        shift = peak + (xcf[ledge] - 1.0)

	        if (RV_GP(rv) != NULL && RV_INTERACTIVE(rv) == YES) {
	            call gline (gp, shift, ycf[ishift], shift, ycf[ishift]+0.1)
	            call gflush (gp)
	        }
	    }
	    RV_FITDONE(rv) = YES
	    return

	case SINC:
    	    call rv_sinc (rv, shift, fwhm, hght)
	    RV_FWHM(rv) = fwhm
	    RV_SHIFT(rv) = shift
	    RV_ERROR(rv) = INDEFD
	    RV_R(rv) = INDEF
	    RV_DISP(rv) = INDEF
	    if (RV_GP(rv) != NULL && RV_INTERACTIVE(rv) == YES)
	        call gpmark (gp, xcf[ledge], ycf[ledge], npts, 4, 2., 2.)
	    RV_FITDONE(rv) = YES
	    if (IS_INDEF(fwhm)) {	# no fwhm computed, so leave here
		call rv_draw_fit (rv, gp, NO)
		return
            }
	}
	call amovr (c, COEFF(rv,1), NPARS)
	RV_FWHM(rv) = rv_width (rv)
	RV_HEIGHT(rv) = hght

	# Redraw the new points fit if they were changed.  Also save new fit
	# window parameters
	if (ledge != RV_ISTART(rv) && RV_FITFUNC(rv) != SINC) {
	    if (gp != NULL && RV_INTERACTIVE(rv) == YES) {
		call gseti (gp, G_PMLTYPE, GL_CLEAR)
	        call gpmark (gp, xcf[RV_ISTART(rv)], ycf[RV_ISTART(rv)], 
		    (RV_IEND(rv)-RV_ISTART(rv)+1), 4, 2., 2.)
		call gseti (gp, G_PMLTYPE, GL_SOLID)
	        call gpmark (gp, xcf[ledge], ycf[ledge], npts, 4, 2., 2.)
	        call gpline (gp, xcf[RV_ISTART(rv)], ycf[RV_ISTART(rv)], 
		    (RV_IEND(rv)-RV_ISTART(rv)+1))
	        call gflush (gp)
	    }
	    RV_ISTART(rv) = ledge
	    RV_IEND(rv) = ledge + npts
	}
	nfit = npts

	# Compute the antisymmetric part of correlation and velocity error
	call realloc (RV_ANTISYM(rv), RV_CCFNPTS(rv), TY_REAL)
	if (!IS_INDEF(RV_FWHM(rv))) {
	    call rv_antisym (rv, shift, hght, RV_FWHM(rv), ycf, RV_CCFNPTS(rv), 
	        ANTISYM(rv,1), ccfvar, RV_ERROR(rv), RV_R(rv))
	} else {
	    RV_R(rv) = INDEF
	    if (RV_DCFLAG(rv) != -1)
	        RV_ERROR(rv) = sigma * RV_DELTAV(rv)
	    else
	        RV_ERROR(rv) = sigma
	}

	# Now get the dispersion of the peak
	if (RV_DCFLAG(rv) != -1 && !IS_INDEF(RV_FWHM(rv)))
	    RV_DISP(rv) = RV_FWHM(rv) * RV_DELTAV(rv)
	else
	    RV_DISP(rv) = INDEF

	# Debugging info
	if (DBG_DEBUG(rv) == YES && DBG_LEVEL(rv)>=2) {
	    call d_printf(DBG_FD(rv), "rvfitfunc:\n")
	    call d_printf(DBG_FD(rv), "\tledge=%d redge=%d npts=%d ishift=%d\n")
  	        call pargi(ledge);	call pargi(redge)
		call pargi(npts);	call pargi(ishift)
	    call d_printf(DBG_FD(rv), 
	    "\tshift=%.4g sigma=%.4g fwhm=%.4g disp=%.4g hght=%.4g peak=%.4g\n")
  		call pargr(shift);  call pargr(sigma);  call pargr(RV_FWHM(rv))
		call pargr(RV_DISP(rv));call pargr(hght); call pargr(peak)
	    do i = 1, NPARS {
    		call d_printf (DBG_FD(rv), "\t    c[%d]=%g +/- %g\n")
		    call pargi(i); call pargr(c[i]); call pargr (ECOEFF(rv,i))
	    }
	    call flush (DBG_FD(rv))
	}

	# Put stuff in the common for the log
	binshift = xcf[ishift]
	
	if (RV_ERRCODE(rv) == OK) {
	    RV_FITDONE(rv) = YES
	    RV_UPDATE(rv) = YES
	}

	# Plot the computed fit.
	call rv_draw_fit (rv, gp, NO)

	# Mark the background level.
	if (RV_FITFUNC(rv) == GAUSSIAN || RV_FITFUNC(rv) == LORENTZIAN) {
            if (RV_INTERACTIVE(rv) == YES)  	
                call rv_draw_background (rv, gp)
	}
end


# RV_WIDTH - Procedure to compute the width of the CCF.

real procedure rv_width (rv)

pointer	rv				#I RV struct pointer

real	fwhm, h, l, r, peak, shift
int	gstati()

include "fitcom.com"

begin
	# Now correct it for a fixed baseline
	switch (RV_FITFUNC(rv)) {
	case GAUSSIAN:
	    fwhm = sqrt (COEFF(rv,3)) * 2.35482
	    l = COEFF(rv,2) - fwhm / 2.
	    r = COEFF(rv,2) + fwhm / 2.
	    call cgauss1d (l, 1, COEFF(rv,1), nfitpars, h)
	case LORENTZIAN:
	    fwhm = 2. * abs (COEFF(rv,3))
	    fwhm = abs (COEFF(rv,3))		# for new lorentzian
	    l = COEFF(rv,2) - fwhm / 2.
	    r = COEFF(rv,2) + fwhm / 2.
	    call lorentz (l, 1, COEFF(rv,1), nfitpars, h)
	case PARABOLA:
            peak = COEFF(rv,1) - COEFF(rv,2)**2 / (4. * COEFF(rv,3))
            fwhm = sqrt ( abs(- peak / (2. * COEFF(rv,3))))
	    #return (fwhm) 
	    shift = -COEFF(rv,2) / (2.*COEFF(rv,3))
	    l = shift - fwhm / 2.
	    r = shift + fwhm / 2.
	    h = COEFF(rv,1) + COEFF(rv,2) * l + COEFF(rv,3) * l**2
	case CENTER1D:
	    return (INDEF)
	case SINC:
	    # The structure parameters were computed before, we just need
	    # this to draw the marker.
	    l = RV_SHIFT(rv) - RV_FWHM(rv) / 2.
	    r = RV_SHIFT(rv) + RV_FWHM(rv) / 2.
	    h = RV_FWHM_Y(rv)
	    fwhm = RV_FWHM(rv)
	}
	RV_FWHM_Y(rv) = h
	
	# Now draw the line showing the width
	if (RV_GP(rv) != NULL && RV_INTERACTIVE(rv) == YES) {
	    if (gstati(RV_GP(rv),G_PLCOLOR) != C_BACKGROUND) {
	        call gseti (RV_GP(rv), G_PLTYPE, GL_DASHED)
	        call gseti (RV_GP(rv), G_PLCOLOR, RV_LINECOLOR(rv))
	    }
	    call gline (RV_GP(rv), l, h, r, h)
	    if (gstati(RV_GP(rv),G_PLCOLOR) != C_BACKGROUND) {
	        call gseti (RV_GP(rv), G_PLTYPE, GL_SOLID)
	        call gseti (RV_GP(rv), G_PLCOLOR, C_FOREGROUND)
	    }
	    call gflush (RV_GP(rv))
	}	

	return (fwhm)
end


# RV_XFIT - Set the fitting endpoints as described for the 'g' keystroke 
# command.

procedure rv_xfit (rv, x, do_correction)

pointer	rv					#I RV struct pointer
real	x					#I Current cursor x position
int	do_correction				#I Do heliocentric correction?

real	sregion, eregion, y
real	shift, sigma
int	istart, iend, ishift, npts, stat

int	rv_getshift(), rv_rvcorrect()

include "fitcom.com"

begin
    	sregion = x				# get endpoints
    	call rv_getpts (rv, eregion, y, 2)

	npts = RV_CCFNPTS(rv) 		# Fit the region
	call rv_fixx (sregion, eregion, WRKPIXX(rv,1), WRKPIXX(rv,npts))
	istart = int (npts/2 + 1 + sregion)
	iend = int (npts/2 + 1 + eregion)
	npts = int (iend - istart + 1)
	nfit = npts
    	ishift = rv_getshift (WRKPIXY(rv,istart), npts, MAXIMUM)

	# now jump into the fitting routines
    	call rv_fit (rv, WRKPIXX(rv,1), WRKPIXY(rv,1), istart, iend, npts, 
	    ishift+istart-1, shift, sigma)
	if (RV_ERRCODE(rv) == ERR_FIT) {
	    if (RV_INTERACTIVE(rv) == YES)
	        call rv_errmsg ("Fit did not converge")
	    else
	        call rv_err_comment (rv, "Fit did not converge", "")
	    return
	}
	RV_SHIFT(rv) = shift
	RV_SIGMA(rv) = sigma

	if (do_correction == YES) {
    	    stat = rv_rvcorrect (rv, shift, sigma, RV_VOBS(rv), RV_VCOR(rv),
		RV_ERROR(rv))
	    if (stat != OK) {
		call rv_err_comment (rv, 
		    "WARNING: Heliocentric correction not done properly.", "") 
	    }
	    if (RV_INTERACTIVE(rv) == YES)
	        call rv_writeln (rv, STDOUT)
	}
end


# RV_YFIT - Fit the CCF based on the Y value of the cursor, as described 
# for the 'y' keystroke command or the HEIGHT parameter.  

procedure rv_yfit (rv, y, do_correction)

pointer	rv					#I RV struct pointer
real	y					#I Current Y cursor value
int	do_correction				#I Do heliocentric correction?

real	sregion, eregion
real	shift, sigma, center
int	istart, iend, ishift, npts, stat, i

int	rv_getshift(), rv_rvcorrect()

include "fitcom.com"

begin
	# Search the array for the closest points in y
	npts = RV_WINR(rv) - RV_WINL(rv) + 1
	center = RV_CCFNPTS(rv)/2 + 1 + WRKPIXX(rv,RV_WINCENTER(rv))
	i = int (center - RV_WINDOW(rv))
    	ishift = rv_getshift (WRKPIXY(rv,i), npts, MAXIMUM) + i - 1
	i = 0
	while (WRKPIXY(rv,ishift-i) > y && i <= npts) {
	    sregion = WRKPIXX(rv, ishift-i)
	    i = i + 1
	}
	i = 0
	while (WRKPIXY(rv, ishift+i) > y && i <= npts) {
	    eregion = WRKPIXX(rv, ishift+i)
	    i = i + 1
	}

	# Pick up at fitting routines
	npts = RV_CCFNPTS(rv)
	istart = int (npts/2 + 1 + sregion)
	iend = int (npts/2 + 1 + eregion)
	npts = (iend - istart + 1)

	# Do the minwidth/maxwidth/window constraints
	call rv_fix_window (rv, 1., real(RV_CCFNPTS(rv)), y, WRKPIXX(rv,1), 
	    WRKPIXY(rv,1), istart, iend, ishift, npts)

	# Go ahead and fit this puppy
    	call rv_fit (rv, WRKPIXX(rv,1), WRKPIXY(rv,1), istart, iend,
	    npts, ishift, shift, sigma)
	if (RV_ERRCODE(rv) == ERR_FIT) {
	    if (RV_INTERACTIVE(rv) == YES)
	        call rv_errmsg ("Fit did not converge")
	    else
	        call rv_err_comment (rv, "Fit did not converge", "")
	    return
	}
	RV_SHIFT(rv) = shift
	RV_SIGMA(rv) = sigma

	if (do_correction == YES) {
    	    stat = rv_rvcorrect (rv, shift, sigma, RV_VOBS(rv), RV_VCOR(rv), 
	        RV_ERROR(rv))
	    if (RV_INTERACTIVE(rv) == YES)
	        call rv_writeln (rv, STDOUT)
	}
end


# RV_FIX_WINDOW - Resize the fit window according to the minwidth/maxwidth
# constraint parameters.  This routine also recenters the window on the
# initial guess at the shift so points are evenly spaced about the peak.
# Does a bounds check to avoid segmentation violations.

procedure rv_fix_window (rv, x1, x2, y, xcf, ycf, istart, iend, ishift, npts)

pointer	rv						#I RV struct pointer
real	x1, x2						#I Bounds check
real	y						#I Threshold level
real	xcf[ARB], ycf[ARB]				#I CCF array
int	istart						#U Start pixel of fit
int	iend						#U End pixel of fit
int	ishift						#U Peak pixel of fit
int	npts						#U Npts in between

int	i, np1, np2

begin
	if (npts < RV_MINWIDTH(rv)) {
	    np1 = RV_MINWIDTH(rv) - npts	# Pad some points
	    istart = istart - (np1 / 2)
	    iend = iend + (np1 / 2)
	    if (mod(np1,2) == 1)
		iend = iend + 1
	} else if (npts > RV_MAXWIDTH(rv)) {
	    np1 = npts - RV_MAXWIDTH(rv)	# Delete some points
	    istart = istart + (np1 / 2)
	    iend = iend - (np1 / 2)
	    if (mod(np1,2) == 1)
	        iend = iend - 1
	}
	npts = int (iend - istart + 1)

	# Next, we have to make sure we honor the original constraint that
	# all points are above a certain level
	if (npts > RV_MINWIDTH(rv)) {
	    i = istart
	    while (ycf[i] < y && i <= ishift)		# Fix left side
	        i = i + 1
	    if (i != istart)
	        istart = i #+ 1
	    i = iend
	    while (ycf[i] < y && i >= ishift)		# Fix right side
	        i = i - 1
	    if (i != iend)
	        iend = i #- 1
	}
	npts = int (iend - istart + 1)

	# Now recenter the window on the peak
	np1 = ishift - istart
	np2 = iend - ishift
	if ((np1 - np2) < -1) {			# Peak is left of center
	     istart = istart - abs(np1 - np2) / 2
	     iend = iend - abs(np1 - np2) / 2
	} else if ((np1 - np2) > 1) {		# Peak is right of center
	     istart = istart + abs(np1 - np2) / 2
	     iend = iend + abs(np1 - np2) / 2
	}
	npts = int (iend - istart + 1)

	# Lastly, make sure we aren't out of bounds after all this work
	if (istart < x1) {
	    np1 = (x1 - istart)
	    istart = int (x1)
	    iend = iend + np1
	}
	if (iend > x2) {
	    np1 = (iend - x2)
	    iend = int (x2)
	    istart = istart - np1
	}
	npts = int (iend - istart + 1) 		# Update npts and return
end
