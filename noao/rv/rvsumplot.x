include	<gset.h>
include "rvpackage.h"
include "rvflags.h"

# RV_BPLOT - Write the split-plot of the spectrum and correlation function
# to the metacode file, or screen. This routine is called when multiple
# Gaussians have been fit.

procedure rv_bplot (rv, gp)

pointer	rv				#I RV struct pointer
pointer	gp				#I Graphics pointer

pointer	sp, fmt, vel, shift
int	i
real	x, y, mx, my, gap, tick
real	x1, y1, y2, xp, yp

real	model()
double	rv_shift2vel()

define  GAP     .015     	# Gap size in NDC
define  TICK    .025     	# Gap size in NDC

begin
	if (gp == NULL)
	    return				# No-op

	call smark (sp)				# Allocate some space
	call salloc (fmt, SZ_LINE, TY_CHAR)
	call salloc (vel, SZ_LINE, TY_CHAR)
	call salloc (shift, SZ_LINE, TY_CHAR)

	# Clear the screen
	call gclear (gp)

	# Draw the three plots to the screen
	if (OBJCONT(rv) == YES) {
	    call split_plot (rv, gp, TOP, OCONT_DATA(rv,1), RV_NPTS(rv),
	        SUMMARY_PLOT, SPECTRUM_PLOT)
	} else {
	    call split_plot (rv, gp, TOP, OBJPIXY(rv,1), RV_NPTS(rv),
	        SUMMARY_PLOT, SPECTRUM_PLOT)
	}
	call split_plot (rv, gp, BOTTOM, WRKPIXY(rv,1), RV_CCFNPTS(rv), 
	    BINARY_PLOT, VCORRELATION_PLOT)

	# Label the velocities
	call strcpy ("u=180;h=c;v=b;s=0.5;q=h", Memc[fmt], SZ_LINE)
	call gseti (gp, G_WCS, 1)
	gap = GAP
	tick = TICK
	do i = 1, DBL_NSHIFTS(rv) {
	    call sprintf (Memc[shift], SZ_FNAME, "%d\0")
	        call pargi (i)
	    if (RV_DCFLAG(rv) != -1)
	        x = real (rv_shift2vel(rv,DBL_SHIFT(rv,i)))
	    else
	        x = DBL_SHIFT(rv,i)
            y = model (DBL_SHIFT(rv,i), DBL_COEFFS(rv,1), 3*DBL_NSHIFTS(rv)+2)
            y = DBL_SCALE(rv) * y + (DBL_Y1(rv) + DBL_SLOPE(rv) *
	    	(DBL_SHIFT(rv,i) - DBL_X1(rv)))
 
	    # Draw the tick line
            call gctran (gp, x, y, mx, my, 1, 0)
            call gctran (gp, mx, my + gap, x1, y1, 0, 1)
            call gctran (gp, mx, my + gap + tick, x1, y2, 0, 1)
            call gline (gp, x1, y1, x1, y2)

	    # Mark the shift number
	    call gctran (gp, mx, my + gap + tick + gap, x1, y2, 0, 1)
	    call gseti (gp, G_TXCOLOR, RV_TXTCOLOR(rv))
	    call gtext (gp, x1, y2, Memc[shift], Memc[fmt])
	    call gseti (gp, G_TXCOLOR, C_FOREGROUND)

	    # Now print the velocity
	    call gctran (gp, 0.14, (0.58-(i-1)*0.04), xp, yp, 0, 1)
	    if (RV_DCFLAG(rv) != -1) {
	        call sprintf (Memc[vel], SZ_LINE, "Vh[%d] = %.3f +- %.3f\0")
		    call pargi (i)
		    call pargr (DBL_VHELIO(rv,i))
		    call pargr (DBL_VERR(rv,i))
	    } else {
	        call sprintf (Memc[vel], SZ_LINE, "Shift[%d] = %.3f\0")
		    call pargi (i)
		    call pargr (DBL_SHIFT(rv,i))
	    }
	    call gtext (gp, xp, yp, Memc[vel], "s=0.75")
	    call gflush (gp)
	}

	call gflush (gp)
	call sfree (sp)
end


# RV_EPLOT - Write the split-plot of the spectrum and correlation function
# to the metacode file, or screen.  The procedure name is derived from the
# keystroke to call the plot from cursor mode.

procedure rv_eplot (rv, gp)

pointer	rv				#I RV struct pointer
pointer	gp				#I Graphics pointer

int	i
real	h, xp, yp, step, shift
real	x1, x2, y1, y2

begin
	if (gp == NULL)
	    return			# No-op

	# Do a double star?
	if (IS_DBLSTAR(rv) == YES) {
	    call rv_bplot (rv, gp)
	    return
	}

	# Clear the screen
	call gclear (gp)

	# Draw the three plots to the screen
	if (OBJCONT(rv) == YES) {
	    call split_plot (rv, gp, TOP, OCONT_DATA(rv,1), RV_NPTS(rv),
	        SUMMARY_PLOT, SPECTRUM_PLOT)
	} else {
	    call split_plot (rv, gp, TOP, OBJPIXY(rv,1), RV_NPTS(rv),
	        SUMMARY_PLOT, SPECTRUM_PLOT)
	}
	call split_plot (rv, gp, MIDDLE, WRKPIXY(rv,1), 
	    RV_CCFNPTS(rv), SUMMARY_PLOT, CORRELATION_PLOT)
	call split_plot (rv, gp, BOTTOM, WRKPIXY(rv,1), 
	    RV_CCFNPTS(rv), SUMMARY_PLOT, VCORRELATION_PLOT)

	# Now get the coords to draw the text
	call gseti (gp, G_WCS, 2)
	call ggwind (gp, x1, x2, y1, y2)   
	call gseti (gp, G_TXCOLOR, RV_TXTCOLOR(rv))
	if (RV_ERRCODE(rv) == ERR_FIT) {
	    call gctran (gp, 0.14, 0.4, xp, yp, 0, 2)
	    call gtext (gp, xp, yp, "Fit did not converge.", "")
	    call gflush (gp)
	    return
	} else {
	    step = (y2 - y1) / 9.0	    # For pretty spacings (empirical)
	    yp = y2 - (step / 2.)
	    call gctran (gp, 0.14, yp, xp, y2, 0, 2)
	    do i = 1, 5 {
	        yp = yp - step
	        call wpl_text (rv, gp, xp, yp, i)
	    }
	}
	call gseti (gp, G_TXCOLOR, C_FOREGROUND)
	call gflush (gp)

	# Lastly, write out the indicator for the FWHM calculation
	call gseti (gp, G_PLCOLOR, RV_LINECOLOR(rv))
	if (RV_FITFUNC(rv) != CENTER1D) {
	    h = RV_FWHM_Y(rv)
	    if (RV_DCFLAG(rv) == -1) {
		if (RV_FITFUNC(rv) != PARABOLA) {
	            call gline (gp, (COEFF(rv,2)-(0.5*RV_FWHM(rv))), h, 
		        (COEFF(rv,2)+(0.5*RV_FWHM(rv))), h)
		} else {
		    shift = -COEFF(rv,2) / (2.*COEFF(rv,3))
	            call gline (gp, (shift-(0.5*RV_FWHM(rv))), h, 
		        (shift+(0.5*RV_FWHM(rv))), h)
		}
	    } else {
	        call gline (gp, real(RV_VREL(rv)-(0.5*RV_DISP(rv))), h,
		    real(RV_VREL(rv)+(0.5*RV_DISP(rv))), h)
	    }
	}
	call gseti (gp, G_PLCOLOR, C_FOREGROUND)
	call gflush (gp)
end


# WPL_TEXT - Write the text string to the screen at the specified point.

procedure wpl_text (rv, gp, xp, yp, lnum)

pointer	rv					#I RV struct pointer
pointer	gp					#I Graphics pointer
real	xp, yp					#I Position
int 	lnum					#I Line to write

pointer	sp, bp

begin
	# Allocate working space
	call smark (sp)			
	call salloc (bp, SZ_LINE, TY_CHAR)

	switch (lnum) {
	case 1:
	    call sprintf (Memc[bp], SZ_LINE, "Shift = %-.3f")
		call pargr (RV_SHIFT(rv))
	case 2:
	    call sprintf (Memc[bp], SZ_LINE, "Height = %-.3f")
		call pargr (RV_HEIGHT(rv))
	case 3:
	    if (RV_DCFLAG(rv) != -1) {
	        call sprintf (Memc[bp], SZ_LINE, "VHelio = %-.3f +- %-.3f")
		    call pargd (RV_VCOR(rv))
		    call pargd (RV_ERROR(rv))
	    } else {
	        call sprintf (Memc[bp], SZ_LINE, "VHelio = INDEF")
	    }
	case 4:
	    call sprintf (Memc[bp], SZ_LINE, "Width = %-.3f %s")
		if (RV_DCFLAG(rv) != -1) {
		    call pargr (RV_DISP(rv))
		    call pargstr ("")
		} else {
		    call pargr (RV_FWHM(rv))
		    call pargstr ("pix")
		}
	case 5:
	    call sprintf (Memc[bp], SZ_LINE, "R = %-.3f")
		call pargr (RV_R(rv))
	}

	# Write the text
	call gtext (gp, xp, yp, Memc[bp], "")

	call sfree (sp)
end
