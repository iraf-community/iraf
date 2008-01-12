include	<gset.h>
include "rvpackage.h"
include "rvflags.h"
include "rvplots.h"
include "rvsample.h"

# RV_PLOT - Do the plotting for the task.  Flags are passed in which tell the
# type of plot to draw

procedure rv_plot (rv, flags)

pointer	rv					#I RV struct pointer
int	flags					#I Type of plot to draw

pointer gp, sp
pointer title
real	y1, y2, statr
real	rv_width()

begin
	if (RV_INTERACTIVE(rv) == YES)
	    gp = RV_GP(rv)
	else
	    gp = RV_MGP(rv)

	# Get the graphics pointer and clear the workstation
	if (gp == NULL)
	    return

	# Take care of the simple case first
	switch (flags) {
	case AMPLITUDE_PLOT, POWER_PLOT, PHASE_PLOT:
	    call gclear (gp)				
	    call fft_plot (rv, flags)
	    RV_GTYPE(rv) = flags
	    return
	}

	RV_GTYPE(rv) = flags		# Update the current plot type

	call smark (sp)
	call salloc (title, 4*SZ_LINE, TY_CHAR)

	# Do the silly title string
	switch (flags) {
	case SPECTRUM_PLOT, FILTER_PLOT, NORM_PLOT:
	    call get_plot_title (rv, title, RV_NPTS(rv))
	default:
	    call get_plot_title (rv, title, RV_CCFNPTS(rv))
	}

	if (flags!=SPECTRUM_PLOT && flags!=FILTER_PLOT && flags!=NORM_PLOT) {
	     RV_X1(rv) = WRKPIXX(rv,1)
	     RV_X2(rv) = WRKPIXX(rv,RV_CCFNPTS(rv))
	}

	# Call the plot primitives
	switch (flags) {
	case SPECTRUM_PLOT, FILTER_PLOT, NORM_PLOT:
	    call gclear (gp)				
	    if (RV_GTYPE(rv) == NORM_PLOT)
	        call rv_nplot (rv, SPLIT_PLOT)
	    else
	        call rv_splot (rv, SPLIT_PLOT)
	    if (ORCOUNT(rv) != ALL_SPECTRUM)
	        call rv_mark_regions (RV_OSAMPLE(rv), gp)
	    if (RRCOUNT(rv) != ALL_SPECTRUM)
	        call rv_mark_regions (RV_RSAMPLE(rv), gp)

	case CORRELATION_PLOT:
	    call gclear (gp)
	    call split_plot (rv, gp, TOP, WRKPIXY(rv,1),
	        RV_CCFNPTS(rv), OBJECT_SPECTRUM, CORRELATION_PLOT)
	    call split_plot (rv, gp, BOTTOM, WRKPIXY(rv,1),
	        RV_CCFNPTS(rv), OBJECT_SPECTRUM, CORRELATION_PLOT)
	    if (RV_FITDONE(rv) == YES && IS_DBLSTAR(rv) == NO)
	        statr = rv_width (rv)		# Redraw FWHM indicator

	case RESIDUAL_PLOT:
	    call rv_resid_plot (rv)

	default:
	    call error (0, "Invalid plot request in rv_plot().")
	}
	call gflush (gp)
	call ggwind (gp, RV_X1(rv), RV_X2(rv), y1, y2)

	call sfree(sp)
end


# RV_NPLOT - Plot the (two) normalized spectra to the screen

procedure rv_nplot (rv, flag)

pointer	rv				#I RV struct pointer
int	flag				#I Type of flag to print (SINGLE/SPLIT)

pointer	gp				# Graphics pointer
pointer sp, title, bp, xlbl, ylbl, sid
int     npts
real    x1, x2, y1, y2

begin
	gp = RV_GP(rv)
	if (gp == NULL)
		return

	# Allocate working space
	call smark (sp)			
	call salloc (bp,    SZ_LINE,   TY_CHAR)
	call salloc (title, 4*SZ_LINE, TY_CHAR)

	# Clear the screen
	call gclear (gp)

	if (flag == SINGLE_PLOT || RV_CONTINUUM(rv) == OBJ_ONLY) {

            call salloc (xlbl, SZ_FNAME, TY_CHAR)
            call salloc (ylbl, SZ_FNAME, TY_CHAR)
            call salloc (sid, SZ_LINE, TY_CHAR)
            call aclrc (Memc[title], 4*SZ_LINE)
            call aclrc (Memc[xlbl], SZ_FNAME)
            call aclrc (Memc[ylbl], SZ_FNAME)
            call aclrc (Memc[sid], SZ_LINE)

            # Draw the plot to the screen
            npts = RV_NPTS(rv)
            call sysid (Memc[sid], SZ_LINE)
            call sprintf (Memc[title], 4*SZ_LINE,
    "%s\nObject='%s' Reference='%s'\nStar='%s' Temp='%s' npts=%d aperture=%d")
                 call pargstr (Memc[sid])
                 call pargstr (IMAGE(rv))
                 call pargstr (RIMAGE(rv))
                 call pargstr (OBJNAME(rv))
                 call pargstr (TEMPNAME(rv))
                 call pargi (npts)
                 call pargi (RV_APNUM(rv))
            call strcpy ("Intensity", Memc[ylbl], SZ_FNAME)
            call gascale (gp, OCONT_DATA(rv,1), npts, 2)
            call ggwind (gp, x1, x2, y1, y2)
            y1 = y1 - ((y2-y1)/10.0)
            y2 = y2 + ((y2-y1)/10.0)
            if (RV_DCFLAG(rv) == -1) {
                call strcpy ("Pixel", Memc[xlbl], SZ_FNAME)
                x1 = 1.0
                x2 = real (npts)
            } else {
                call strcpy ("Wavelength", Memc[xlbl], SZ_FNAME)
                x1 = 10.0 ** (RV_OW0(rv))
                x2 = 10.0 ** (RV_OW2(rv))
            }

            # Draw the axis labels.
	    call gsview (gp, 0.115, 0.95, 0.125, 0.845)
            call gswind (gp, x1, x2, y1, y2)
            call glabax (gp, Memc[title], Memc[xlbl], Memc[ylbl])

            # Draw the vector.
            call gvline (gp, OCONT_DATA(rv,1), npts, x1, x2)

            # Lastly, annotate ther plot so we know what we're looking at.
            call gctran (gp, 0.6, 0.23, x1, y1, 0, 1)
            call gseti (gp, G_TXCOLOR, RV_TXTCOLOR(rv))
            call gtext (gp, x1, y1, "Normalized Spectrum", "")
            call gseti (gp, G_TXCOLOR, C_FOREGROUND)
            call gseti (gp, G_XDRAWAXES, 3)             # reset gio flags

	    # Draw sample regions.
 	    call rv_mark_regions (RV_OSAMPLE(rv), gp)

            call gflush (gp)

	} else if (flag == SPLIT_PLOT) {
	    if (RV_CONTINUUM(rv) == BOTH || RV_CONTINUUM(rv) == OBJ_ONLY) {
		call split_plot (rv, gp, TOP, OCONT_DATA(rv,1), RV_NPTS(rv),
		    OBJECT_SPECTRUM, NORM_PLOT)
	    } else {
	        call split_plot (rv, gp, TOP, OBJPIXY(rv,1), RV_NPTS(rv),
	            OBJECT_SPECTRUM, SPECTRUM_PLOT)
	    }
	    call rv_mark_regions (RV_OSAMPLE(rv), gp)
	    if (RV_CONTINUUM(rv) == BOTH || RV_CONTINUUM(rv) == TEMP_ONLY) {
	        call split_plot (rv, gp, BOTTOM, RCONT_DATA(rv,1), 
		    RV_RNPTS(rv), REFER_SPECTRUM, NORM_PLOT)
	    } else {
	        call split_plot (rv, gp, BOTTOM, REFPIXY(rv,1), 
		    RV_RNPTS(rv), REFER_SPECTRUM, SPECTRUM_PLOT)
	    }
	    call rv_mark_regions (RV_RSAMPLE(rv), gp)
	}

	call sfree (sp)
end


# RV_RESID_PLOT - Plot the residuals of the fit to the screen.

procedure rv_resid_plot (rv)

pointer	rv					#I RV struct pointer

pointer	sp, gp, resx, resy, buf
real	x, y, sigma, mean, xp, yp
int	npts, i

int	gstati()
real	model()

begin
	gp = RV_GP(rv)
	if (gp == NULL)
	    return

	if (RV_FITDONE(rv) == NO) {
	    call rv_errmsg ("Error: No fit yet done to the data.")
	    return
	} else if (RV_FITFUNC(rv) == CENTER1D || RV_FITFUNC(rv) == SINC) {
	    call rv_errmsg ("Residual plot unavailable for `%s' fit.")
	    if (RV_FITFUNC(rv) == CENTER1D)
		call pargstr ("center1d")
	    else
		call pargstr ("sinc")
	    return
	} 

	if (IS_DBLSTAR(rv) == YES)
	    npts = DBL_NFITP(rv)
	else
	    npts = RV_IEND(rv) - RV_ISTART(rv) + 1

	call smark (sp)
	call salloc (resx, npts, TY_REAL)
	call salloc (resy, npts, TY_REAL)
	call salloc (buf, SZ_LINE, TY_CHAR)

	# Compute the residuals or ratio of the fit
	if (IS_DBLSTAR(rv) == YES)
	    x =  WRKPIXX(rv,DBL_I1(rv))
	else
	    x =  WRKPIXX(rv,RV_ISTART(rv))
	do i = 1, npts {
	    Memr[resx+i-1] =  x
	    if (IS_DBLSTAR(rv) == NO) {
	        switch (RV_FITFUNC(rv)) {
	        case GAUSSIAN:
	            call cgauss1d (x, 1, COEFF(rv,1), 4, y)
	        case LORENTZIAN:
	            call lorentz (x, 1, COEFF(rv,1), 4, y)
	        case PARABOLA:
	            call polyfit (x, 1, COEFF(rv,1), 3, y)
	        }
	    } else {
                y = model (x, DBL_COEFFS(rv,1), 3*DBL_NSHIFTS(rv)+2)
                y = DBL_SCALE(rv) * y +
                    (DBL_Y1(rv) + DBL_SLOPE(rv) * (x-DBL_X1(rv)))
	    }

	    if (IS_DBLSTAR(rv) == YES)
	        Memr[resy+i-1] = WRKPIXY(rv,i+DBL_I1(rv)-1) - y
	    else
	        Memr[resy+i-1] = WRKPIXY(rv,i+RV_ISTART(rv)-1) - y
	    x = x + 1.
	}

	# Add back in the background.
	call aavgr (Memr[resy], npts, mean, sigma)	# save residuals
	if (IS_DBLSTAR(rv) == YES) {
	    do i = 0, npts-1 {
                Memr[resy+i] = Memr[resy+i] + (DBL_Y1(rv) + DBL_SLOPE(rv) *
		    (Memr[resx+i]-DBL_X1(rv)))
	    }
	} else if (!IS_INDEF(RV_BACKGROUND(rv))) {
	    call aaddkr (Memr[resy], RV_BACKGROUND(rv), Memr[resy], npts)
	} else {
	    if (RV_FITFUNC(rv) != PARABOLA) {
	        call aaddkr (Memr[resy], COEFF(rv,4), Memr[resy], npts)
	    } else {
	        call aaddkr (Memr[resy], WRKPIXY(rv,RV_ISTART(rv)), 
		    Memr[resy], npts)
	    }
	}

	# Draw the label and vectors
	if (gstati(gp,G_PLTYPE) != GL_CLEAR)
	    call gseti (gp, G_PLTYPE, GL_DOTTED)
	call gline (gp, WRKPIXX(rv,RV_ISTART(rv)), WRKPIXY(rv,RV_ISTART(rv)), 
	    Memr[resx], Memr[resy])
	call gpline (gp, Memr[resx], Memr[resy], npts)
	call gline (gp, Memr[resx+npts-1], Memr[resy+npts-1],
	    WRKPIXX(rv,RV_IEND(rv)), WRKPIXY(rv,RV_IEND(rv))) 
	if (gstati(gp,G_PLTYPE) != GL_CLEAR)
	    call gseti (gp, G_PLTYPE, GL_SOLID)

	# Mark plot with sigma and mean of the residuals.
	if (gstati(gp,G_PLTYPE) != GL_CLEAR) {
	    call aavgr (Memr[resy], npts, mean, sigma)
	    call gseti (gp, G_TXCOLOR, RV_TXTCOLOR(rv))
	    call sprintf (Memc[buf], SZ_LINE, "Mean residual = %f")
		call pargr (mean)
	    call gctran (gp, 0.15, 0.63, xp, yp, 0, 2)
	    call gtext (gp, xp, yp, Memc[buf], "")
	    call sprintf (Memc[buf], SZ_LINE, "     sigma = %f")
		call pargr (sigma)
	    call gctran (gp, 0.15, 0.58, xp, yp, 0, 2)
	    call gtext (gp, xp, yp, Memc[buf], "")
	    call gseti (gp, G_TXCOLOR, C_FOREGROUND)
	}
	RV_RESDONE(rv) = YES

	call gflush (gp)
	call sfree (sp)
end


# RV_SPLOT - Plot the two spectra to the screen

procedure rv_splot (rv, flag)

pointer	rv				#I RV struct pointer
int	flag				#I Type of flag to print (SINGLE/SPLIT)

pointer	gp				# Graphics pointer
pointer	sp, title, xlbl, ylbl, sid
int	npts
real	x1, x2, y1, y2

begin
	gp = RV_GP(rv)
	if (gp == NULL)
	    return

	# Clear the screen.
	call gclear (gp)

        # Draw the plot to the screen.
	if (flag == SINGLE_PLOT) {

	    call smark (sp)
	    call salloc (title, 4*SZ_LINE, TY_CHAR)
	    call salloc (xlbl, SZ_FNAME, TY_CHAR)
	    call salloc (ylbl, SZ_FNAME, TY_CHAR)
            call salloc (sid, SZ_LINE, TY_CHAR)
	    call aclrc (Memc[title], 4*SZ_LINE)
	    call aclrc (Memc[xlbl], SZ_FNAME)
	    call aclrc (Memc[ylbl], SZ_FNAME)
	    call aclrc (Memc[sid], SZ_LINE)

            # Draw the plot to the screen
            npts = RV_NPTS(rv)
            call sysid (Memc[sid], SZ_LINE)
            call sprintf (Memc[title], 4*SZ_LINE,
    "%s\nObject='%s' Reference='%s'\nStar='%s' Temp='%s' npts=%d aperture=%d")
                 call pargstr (Memc[sid])
                 call pargstr (IMAGE(rv))
                 call pargstr (RIMAGE(rv))
                 call pargstr (OBJNAME(rv))
                 call pargstr (TEMPNAME(rv))
                 call pargi (npts)
                 call pargi (RV_APNUM(rv))
            call strcpy ("Intensity", Memc[ylbl], SZ_FNAME)
            call gascale (gp, OBJPIXY(rv,1), npts, 2)
            call ggwind (gp, x1, x2, y1, y2)
	    y1 = y1 - ((y2-y1)/10.0)
	    y2 = y2 + ((y2-y1)/10.0)
	    if (RV_DCFLAG(rv) == -1) {
		call strcpy ("Pixel", Memc[xlbl], SZ_FNAME)
		x1 = 1.0
		x2 = real (npts)
	    } else {
		call strcpy ("Wavelength", Memc[xlbl], SZ_FNAME)
		x1 = 10.0 ** (RV_OW0(rv))
		x2 = 10.0 ** (RV_OW2(rv))
	    }

            # Draw the axis labels.
	    call gsview (gp, 0.115, 0.95, 0.125, 0.845)
            call gswind (gp, x1, x2, y1, y2)
            call glabax (gp, Memc[title], Memc[xlbl], Memc[ylbl])

	    # Draw the vector.
            call gvline (gp, OBJPIXY(rv,1), npts, x1, x2)

            # Lastly, annotate ther plot so we know what we're looking at.
            call gctran (gp, 0.63, 0.23, x1, y1, 0, 1)
            call gseti (gp, G_TXCOLOR, RV_TXTCOLOR(rv))
            call gtext (gp, x1, y1, "Object Spectrum", "")
            call gseti (gp, G_TXCOLOR, C_FOREGROUND)
            call gseti (gp, G_XDRAWAXES, 3)             # reset gio flags

	    call gflush (gp)
	    call sfree (sp)

	} else if (flag == SPLIT_PLOT) {
	    call split_plot (rv, gp, TOP, OBJPIXY(rv,1), RV_NPTS(rv), 
	        OBJECT_SPECTRUM, SPECTRUM_PLOT)
	    call rv_mark_regions (RV_OSAMPLE(rv), gp)
	    call split_plot (rv, gp, BOTTOM, REFPIXY(rv,1), RV_RNPTS(rv),
	        REFER_SPECTRUM, SPECTRUM_PLOT)
	    call rv_mark_regions (RV_RSAMPLE(rv), gp)
	}

end


# RV_ZPLOT - Zoom in on the current current cursor position

procedure rv_zplot (rv, gp, x, y, wcs)

pointer	rv				#I RV struct pointer
pointer	gp				#I Graphics pointer
real	x				#I X cursor position
real	y				#I Y cursor position
int	wcs				#I WCS of cursor position

real	x1, y1
double	rv_shift2vel()

begin
	# Check for boundary coordinates
	call gctran (gp, x, y, x1, y1, wcs, 0)
	if (y1 > 0.775 && y1 < 0.9 && x1 > 0.115 && x1 < 0.95) {
	    call gctran (gp, x, y, x, y, wcs, 3)
	    if (RV_DCFLAG(rv) == -1)
		RV_WINCENPAR(rv) = x
	    else
		RV_WINCENPAR(rv) = real (rv_shift2vel(rv,x))
	    RV_WINCENTER(rv) = max (real(1), real(x+RV_CCFNPTS(rv)/2+1))
	    RV_WINCENTER(rv) = min (real(RV_WINCENTER(rv)), 
		real(RV_CCFNPTS(rv)-1))
	    IS_DBLSTAR(rv) = NO
	    RV_FITDONE(rv) = NO
	    RV_Y1(rv) = INDEF
	    RV_Y2(rv) = INDEF
            call rv_batch_xcor (rv, RV_TEMPNUM(rv), RV_APNUM(rv), NO, YES, NO)
	} else
	    call rv_errmsg ("You must point at the top plot to zoom.\n")
end
