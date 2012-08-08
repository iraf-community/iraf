include <gset.h>
include "rvpackage.h"
include "rvflags.h"

# RV_DRAW_FIT - Draw the fitted function to the screen.  Called in 
# rv_erase_fit() to erase old fit, and from the fitting rouines and
# plot rouines so they all draw the same function.

procedure rv_draw_fit (rv, gp, is_velocity)

pointer	rv				#I RV struct pointer
pointer	gp 				#I Graphics descriptor
int	is_velocity			#I Plot function on velocity scale?

extern	cgauss1d, lorentz
pointer	sp, pltx, plty
real	step, xl
int	i, pltnpts, nfitpts, nvars, gstati()
double	rv_shift2vel()

include	"rvsinc.com"

begin
	# Check for exit conditions
	#if (RV_INTERACTIVE(rv) == NO || gp == NULL)
	if (gp == NULL || RV_FITDONE(rv) == NO || RV_ERRCODE(rv) == ERR_FIT) 
	    return

	nfitpts = RV_IEND(rv) - RV_ISTART(rv) + 1
	if (RV_FITFUNC(rv) == SINC)
	    pltnpts = (snfit-1) * 10
	else 
	    pltnpts = 10 * nfitpts
	nvars = 1

	# Plot the deblended fit if that is what was done
	if (IS_DBLSTAR(rv) == YES) {
	    call rv_plt_deblend (rv, gp, is_velocity)
	    return
	}

	call smark (sp)				# Allocate space for the plt
	call salloc (pltx, pltnpts, TY_REAL)
	call salloc (plty, pltnpts, TY_REAL)

	# Draw the computed CCF
	call gseti (gp, G_WCS, 2)
	if (RV_FITFUNC(rv) == CENTER1D) {
	    if  (is_velocity == YES && RV_DCFLAG(rv) != -1) {
                call gline (gp, RV_VREL(rv), WRKPIXY(rv,RV_ISHIFT(rv)),
		    RV_VREL(rv), WRKPIXY(rv,RV_ISHIFT(rv))+0.1)
	    } else {
                call gline (gp, RV_SHIFT(rv), WRKPIXY(rv,RV_ISHIFT(rv)),
		    RV_SHIFT(rv), WRKPIXY(rv,RV_ISHIFT(rv))+0.1)
	    }
	    call gflush (gp)
	    call sfree (sp)
	    return

	} else if (RV_FITFUNC(rv) == SINC) {
	    if (is_velocity == YES && RV_DCFLAG(rv) != -1) {
	        do i = 1, pltnpts
		    Memr[pltx+i-1] = real(rv_shift2vel(rv, Memr[splx+i-1]))
	    } else
	        call amovr (Memr[splx], Memr[pltx], pltnpts)
	    call amovr (Memr[sply], Memr[plty], pltnpts)
	} else {
	    call rv_gpltsteps (rv, pltnpts, xl, step)

	    do i = 1, pltnpts {
	        Memr[pltx+i-1] = xl + (i-1) * step
	        switch (RV_FITFUNC(rv)) {
	        case GAUSSIAN:
	           call cgauss1d (Memr[pltx+i-1], nvars, COEFF(rv,1), 4, 
			Memr[plty+i-1])
	        case LORENTZIAN:
	            call lorentz (Memr[pltx+i-1], nvars, COEFF(rv,1), 4, 
		        Memr[plty+i-1])
	        case PARABOLA:
	           call polyfit (Memr[pltx+i-1], nvars, COEFF(rv,1), 3, 
			Memr[plty+i-1])
	        }
	    }
	    if (is_velocity == YES && RV_DCFLAG(rv) != -1) {
	        do i = 1, pltnpts
		    Memr[pltx+i-1] = real(rv_shift2vel(rv, Memr[pltx+i-1]))
	    }
	}

	if (gstati(gp, G_PLTYPE) != GL_CLEAR) {
    	    call gseti (gp, G_PLTYPE, GL_DASHED)
    	    call gseti (gp, G_PLCOLOR, RV_LINECOLOR(rv))
	}
	call gpline (gp, Memr[pltx], Memr[plty], pltnpts)
	if (gstati(gp, G_PLTYPE) != GL_CLEAR) {
    	    call gseti (gp, G_PLTYPE, GL_SOLID)
    	    call gseti (gp, G_PLCOLOR, C_FOREGROUND)
	}

	call gflush (gp)
	call sfree (sp)
end


# RV_DRAW_BACKGROUND - Draw the background marker with the correct line type
# and at the same level.

procedure rv_draw_background (rv, gp)

pointer	rv					#I RV struct pointer
pointer	gp					#I Graphics pointer

real	left, right
int	gstati()

begin
	# Check error conditions.
	if (gp == NULL || RV_FITFUNC(rv) == PARABOLA)
	    return
	if (RV_FITFUNC(rv) == CENTER1D || IS_DBLSTAR(rv) == YES)
	    return
	if (IS_INDEF(RV_BACKGROUND(rv)) && RV_FITFUNC(rv) == SINC)
	    return

	# Get the background window sizes.
	if (RV_DTYPE(rv) == SUMMARY_PLOT) {
	    if (RV_DCFLAG(rv) != -1) {
                left = (RV_WINL(rv) - RV_WINDOW(rv)) * RV_DELTAV(rv)
                right = (RV_WINR(rv) + RV_WINDOW(rv)) * RV_DELTAV(rv)
	    } else {
                left = RV_WINL(rv) - RV_WINDOW(rv)
                right = RV_WINR(rv) + RV_WINDOW(rv)
	    }
	} else {
	    left = real (RV_WINL(rv))
	    right = real (RV_WINR(rv))
	}

	# Draw the background.
	if (RV_FITDONE(rv) == YES) {
	    # Mark the background level
	    if (IS_INDEF(RV_BACKGROUND(rv))) {
	  	if (gstati(gp, G_PLTYPE) != GL_CLEAR) {
    	    	    call gseti (gp, G_PLTYPE, GL_DASHED)
    	    	    call gseti (gp, G_PLCOLOR, C_GREEN)
		}
		call gline (gp, left, COEFF(rv,4), right, COEFF(rv,4))
	  	if (gstati(gp, G_PLTYPE) != GL_CLEAR) {
    	            call gseti (gp, G_PLTYPE, GL_SOLID)
    	    	    call gseti (gp, G_PLCOLOR, C_FOREGROUND)
		}
	    } else {
	  	if (gstati(gp, G_PLTYPE) != GL_CLEAR) {
    	    	    call gseti (gp, G_PLTYPE, GL_DASHED)
    	    	    call gseti (gp, G_PLCOLOR, C_GREEN)
		}
		call gline (gp, left, RV_BACKGROUND(rv), right, 
		    RV_BACKGROUND(rv))
	  	if (gstati(gp, G_PLTYPE) != GL_CLEAR) {
    	            call gseti (gp, G_PLTYPE, GL_SOLID)
    	    	    call gseti (gp, G_PLCOLOR, C_FOREGROUND)
		}
	    }
	
    	} else if (!IS_INDEF(RV_BACKGROUND(rv))) {
    	    #call gseti (gp, G_PLCOLOR, C_GREEN)
	    call gline (gp, left, RV_BACKGROUND(rv), left, RV_BACKGROUND(rv))
    	    #call gseti (gp, G_PLCOLOR, C_FOREGROUND)

	} else {
    	    #call gseti (gp, G_PLCOLOR, C_GREEN)
	    call gline (gp, left, 0.0, right, 0.0)
    	    #call gseti (gp, G_PLCOLOR, C_FOREGROUND)
	}
end


# RV_ERASE_FIT - Erase the previous fit prior to computing new one.  Points,
# function, FWHM line and background level are erased, and the underlying
# ccf in the region redrawn,

procedure rv_erase_fit (rv, redraw)

pointer	rv				#I RV struct pointer
bool	redraw				#I Redraw background?

pointer	gp
real	statr, rv_width()
int	ledge, redge, npts

begin
	# Check for exit conditions
	if (RV_INTERACTIVE(rv) == NO || RV_GP(rv) == NULL)
	    return
	if (RV_FITDONE(rv) == NO || RV_AUTODRAW(rv) == NO)
	    return

	gp = RV_GP(rv)
	redge = RV_IEND(rv)			# initializations
	ledge = RV_ISTART(rv)
	npts = redge - ledge + 1

	# First set the line and polymarker types to be black.
	call gseti (gp, G_WCS, 2)
	call gseti (gp, G_PLCOLOR, C_BACKGROUND)
	call gseti (gp, G_PLTYPE, GL_CLEAR)
	call gseti (gp, G_PMLTYPE, GL_CLEAR)

	# Erase the computed CCF.
	call rv_draw_fit (rv, gp, NO)

	# Erase the points being used in the fit.
	call gpmark (gp, WRKPIXX(rv,ledge), WRKPIXY(rv,ledge), npts, 4, 2., 2.)
	call gflush (gp)

	# Erase the background level.
	if ((RV_FITFUNC(rv) == GAUSSIAN || RV_FITFUNC(rv) == LORENTZIAN) && 
	    IS_DBLSTAR(rv) == NO) {
	        call rv_draw_background (rv, gp)
	        call gflush (gp)
	}

	# Erase the FWHM level.
	if (!IS_INDEF(RV_FWHM_Y(rv)) && IS_DBLSTAR(rv) == NO) {
	    statr = rv_width (rv)
	    call gflush (gp)
	}

	# Erase the computed CCF.
	#call rv_draw_fit (rv, gp, NO)

	# Just in case, let's also erase the residuals.
	if (RV_RESDONE(rv) == YES) {
	    call rv_resid_plot (rv)
	    RV_RESDONE(rv) = NO
	}

	# Now redraw the ccf, with a little on each end to cover up the slop.
	call gseti (gp, G_PLTYPE, GL_SOLID)
	call gseti (gp, G_PLCOLOR, C_FOREGROUND)
	call gseti (gp, G_PMLTYPE, GL_SOLID)
	call gpline (gp, WRKPIXX(rv,max(1,ledge-4)), WRKPIXY(rv,max(1,ledge-4)),
	    min(npts+8,RV_CCFNPTS(rv)))
	call gflush (gp)

        # Redraw the background level.
	if (redraw && IS_DBLSTAR(rv) == NO)
	    call rv_draw_background (rv, gp)
	call gflush (gp)
end


# RV_GPLTSTEPS - Get the function starting and increment parameters

procedure rv_gpltsteps (rv, npts, xl, step)

pointer	rv					#I RV struct pointer
int	npts					#I Npts being plotted
real	xl					#O Start position
real	step					#O Plot increment

real	dv, c2, c3, istart, iend

begin
	dv = RV_DELTAV(rv)			# Initialize
	c2 = COEFF(rv,2)
	c3 = COEFF(rv,3)
	istart = WRKPIXX(rv,RV_ISTART(rv))
	iend = WRKPIXX(rv,RV_IEND(rv))
	
	switch (RV_FITFUNC(rv)) {
	case PARABOLA:
	    xl = istart
	    step = abs (iend - istart) / real (npts-1)
	case GAUSSIAN:
	    xl = c2 - (3. * sqrt(c3))
	    step = (c2 + (3. * sqrt(c3)) - xl) / real (npts-1)
	case LORENTZIAN:
	    xl = c2 - (2. * c3)
	    step = ((c2 + (2. * c3)) - xl) / real(npts-1)
	}
end


# RV_PLT_DEBLEND -- Plot the fitted model function.

procedure rv_plt_deblend (rv, gp, is_velocity)

pointer	rv					#I RV struct pointer
pointer	gp					#I Graphics descriptor
int	is_velocity				#I Plot on velocity axis?

real	w, xval, yval
real	x1, x2, y1, y2
int	i, j, npts, pnpts
int	i1, nsub, offset

double	rv_shift2vel()
real	model()
int	gstati()

begin
	if (gp == NULL)
	    return

	nsub = 10
	pnpts = nsub * (npts-1)

	# Compute model spectrum with continuum and plot.
	i1 = DBL_I1(rv)
	x1 = WRKPIXX(rv,1)
	npts = DBL_NFITP(rv)
	if (gstati(gp, G_PLTYPE) != GL_CLEAR) {
    	    call gseti (gp, G_PLTYPE, GL_DASHED)
    	    call gseti (gp, G_PLCOLOR, RV_LINECOLOR(rv))
	}
	do i = 1, npts-1 {
	    do j = 1, nsub {
	        offset = ((i-1)*nsub+j)-1
	        w = x1 + (i1+i-2) + (j-1) * 0.1
	        if (is_velocity == YES && RV_DCFLAG(rv) != -1)
	            #xval = w * RV_DELTAV(rv)
	            xval = real (rv_shift2vel(rv,w))
		else
	            xval = w
	        yval = model (w, DBL_COEFFS(rv,1), 3*DBL_NSHIFTS(rv)+2)
	        yval = DBL_SCALE(rv) * yval +
	   	    (DBL_Y1(rv)+DBL_SLOPE(rv)*(w-DBL_X1(rv)))

		if (i == 1 && j == 1)
		    call gamove (gp, xval, yval)
		else
		    call gadraw (gp, xval, yval)
	    }
	    call gflush (gp)
	}
	if (gstati(gp, G_PLTYPE) != GL_CLEAR)
    	    call gseti (gp, G_PLTYPE, GL_SOLID)

	# Draw the background to the screen.
	y1 = DBL_Y1(rv)
	y2 = DBL_Y2(rv)
	if (is_velocity == YES && RV_DCFLAG(rv) != -1) {
	    #x1 = DBL_X1(rv) * RV_DELTAV(rv)
	    #x2 = DBL_X2(rv) * RV_DELTAV(rv)
	    x1 = real (rv_shift2vel(rv,DBL_X1(rv)))
	    x2 = real (rv_shift2vel(rv,DBL_X2(rv)))
	} else {
	    x1 = DBL_X1(rv)
	    x2 = DBL_X2(rv)
	}
	call gline (gp, x1, y1, x2, y2)
	if (gstati(gp, G_PLTYPE) != GL_CLEAR) {
    	    call gseti (gp, G_PLTYPE, GL_SOLID)
    	    call gseti (gp, G_PLCOLOR, C_FOREGROUND)
	}
	call gflush (gp)
end
