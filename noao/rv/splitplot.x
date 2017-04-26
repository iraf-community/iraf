include	<gset.h>
include "rvpackage.h"
include "rvflags.h"
include "rvplots.h"
include "rvsample.h"

# SPLIT_PLOT - Plot the Fourier transform, power spectrum or spectrum
# normalization to the top or botom half of the screen.

procedure split_plot (rv, gp, where, rinpt, npts, dtype, pltype)

pointer	rv				#I RV struct pointer
pointer	gp				#I Graphics pointer
int	where				#I Where to make plot
real	rinpt[npts]			#I Input array
int	npts				#I No. points in input
int	dtype				#I Type of data being plotted (obj|ref)
int	pltype				#I Type of data plot to draw

pointer	sp, xdata, pldata, title
pointer	xlbl, ylbl
real	x1, x2, y1, y2
int	i, fnpts, pnpts
double	rv_shift2vel()

begin
	if (gp == NULL)
	    return

	call smark (sp)
	call salloc (title, 3*SZ_LINE, TY_CHAR)
	call salloc (xlbl, SZ_FNAME, TY_CHAR)
	call salloc (ylbl, SZ_FNAME, TY_CHAR)
	call salloc (pldata, 2*npts, TY_REAL)
	call salloc (xdata, 2*npts, TY_REAL)

	# Do some misc. initial stuff
	RV_WHERE(rv) = where
	RV_DTYPE(rv) = dtype
	call aclrr (Memr[pldata], 2*npts)
	call amovr (rinpt, Memr[pldata], npts)
	pnpts = RV_IEND(rv) - RV_ISTART(rv) + 1

	switch (pltype) {
	case SPECTRUM_PLOT, PREPARED_PLOT, NORM_PLOT:
	    fnpts = npts
	    call sp_spectrum (rv, title, xlbl, ylbl, x1, x2, fnpts, 
	        Memr[xdata], pltype)

	case FILTER_PLOT:
	    fnpts = npts
	    call sp_spectrum (rv, title, xlbl, ylbl, x1, x2, fnpts, 
		Memr[xdata], pltype)

	case CORRELATION_PLOT:
	    call sp_correlation (rv, where, title, xlbl, ylbl, x1, x2)
	    if (where == BOTTOM)
	        fnpts = npts
	    else {
		x1 = WRKPIXX(rv,1)
		x2 = WRKPIXX(rv,RV_CCFNPTS(rv))
		fnpts = RV_CCFNPTS(rv)
		call strcpy ("", Memc[ylbl], SZ_LINE)
	    }
	    call amovr (WRKPIXX(rv,1), Memr[xdata], fnpts)

	case VCORRELATION_PLOT:
	    call sp_vcorrelation (rv, title, xlbl, ylbl, x1, x2)
	    if (RV_DCFLAG(rv) == -1 && (dtype == SUMMARY_PLOT ||
		dtype == BINARY_PLOT)) {
		    x1 = RV_WINL(rv) - RV_WINDOW(rv)
		    x2 = RV_WINR(rv) + RV_WINDOW(rv)
		    fnpts = RV_CCFNPTS(rv)
		    call amovr (WRKPIXX(rv,1), Memr[xdata], fnpts)
	    } else if (where == BOTTOM) {
		do i = 1, npts 
		    Memr[xdata+i-1] = real (rv_shift2vel(rv,WRKPIXX(rv,i)))
	        fnpts = npts
	    } 

	case FOURIER_PLOT:
	    call sp_fourier (rv, dtype, where, rinpt, pldata, title, 
		xlbl, ylbl, x1, x2, npts, fnpts)
	    fnpts = int (fnpts / RVP_FFT_ZOOM(rv))

	case PS_PLOT:
	    call sp_psplot (rv, dtype, where, rinpt, pldata, title, 
		xlbl, ylbl, x1, x2, npts, fnpts)
	    fnpts = int (fnpts / RVP_FFT_ZOOM(rv))

	case ANTISYM_PLOT:
	    call sp_anplot (rv, title, xlbl, ylbl, x1, x2)
	    fnpts = RV_CCFNPTS(rv)
	    call amovr (WRKPIXX(rv,1), Memr[xdata], fnpts)
	    call amovr (ANTISYM(rv,1), Memr[pldata], fnpts)

	default:
	    call error (0, "split_plot: Illegal plot flag passed.")
	}

	# Set viewports for the plot to the screen
	call sp_set_viewports (rv, gp, where, dtype, pltype, Memc[xlbl])

	# Now label the axes for the various plots
	call sp_label_axes (rv, gp, dtype, pltype, where, x1, x2, y1, y2,
	    Memr[pldata], fnpts, pnpts, Memc[title], Memc[xlbl], Memc[ylbl])

	# lastly, draw the actual vector in the window
	call sp_draw_vector (rv, gp, pltype, where, x1, x2, y1, y2, 
	    Memr[pldata], Memr[xdata], fnpts, pnpts)

	# Now make it pretty
	call sp_annotate (rv, gp, pltype, dtype, fnpts, x1, x2, y1, y2)

	call gflush (gp)
	call sfree (sp)
end


# SP_SET_VIEWPORTS - Set the view ports for the various plit screens

procedure sp_set_viewports (rv, gp, where, dtype, pltype, xlbl)

pointer	rv					#I RV struct pointer
pointer	gp					#I Graphics pointer
int	where					#I Where is plot being drawn
int	dtype					#I Type of data (2ndary flag)
int	pltype					#I Plot type
char	xlbl[SZ_FNAME]				#I X-axis Label

begin
	# Set those darned viewpoints
	switch (where) {
	case TOP:					# Set top viewport
	    call gseti (gp, G_WCS, 1)
	    if (pltype == CORRELATION_PLOT && dtype != ANTISYM_PLOT) {
		call gseti (gp, G_WCS, 3)
	        call gsview (gp, 0.115, 0.95, 0.775, 0.90)
	    } else if (pltype == CORRELATION_PLOT && dtype == ANTISYM_PLOT)
	        call gsview (gp, 0.115, 0.95, 0.65, 0.90)
	    else if (pltype == SPECTRUM_PLOT && dtype == SUMMARY_PLOT)
	        call gsview (gp, 0.115, 0.95, 0.70, 0.90)
	    else
	        call gsview (gp, 0.115, 0.95, 0.51, 0.865)
	    call strcpy ("", xlbl, SZ_FNAME)		# Overwrite axis label

	case MIDDLE:
	    call gseti (gp, G_WCS, 1)
	    call gsview (gp, 0.115, 0.95, 0.475, 0.63)	# In a summary plot

	case BOTTOM:					# Set bottom viewport
	    call gseti (gp, G_WCS, 1)
	    if (pltype == ANTISYM_PLOT)
	        call gsview (gp, 0.115, 0.95, 0.30, 0.55)
	    else if (pltype == CORRELATION_PLOT) {
		call gseti (gp, G_WCS, 2)		# Restore attributes
	        call gsview (gp, 0.115, 0.95, 0.15, 0.725)
	    } else if (pltype == VCORRELATION_PLOT && dtype == SUMMARY_PLOT) {
		call gseti (gp, G_WCS, 2)		# Restore attributes
	        call gsview (gp, 0.115, 0.95, 0.125, 0.465)
	    } else if (pltype == VCORRELATION_PLOT && dtype == BINARY_PLOT) {
		call gseti (gp, G_WCS, 1)
	        call gsview (gp, 0.115, 0.95, 0.125, 0.64)
	    } else {
	        call gsview (gp, 0.115, 0.95, 0.125, 0.50)
	    }

	default:
	    call gclear (gp)
	}

	call gflush (gp)
end


# SP_LABEL_AXES - Draw the axes labels for the requested plots

procedure sp_label_axes (rv, gp, dtype, pltype, where, x1, x2, y1, y2, pldata, 
    fnpts, pnpts, title, xlbl, ylbl)

pointer	rv					#I RV struct pointer
pointer	gp					#I Graphics pointer
int	dtype					#I Type of data (2ndry flag)
int	pltype					#I Type of plot to draw
int	where					#I Where to draw the plot
real	x1, x2, y1, y2				#I Axis endpoints
real	pldata[ARB]				#I Data vector being drawn
int	fnpts, pnpts				#i FFT npts and plot npts
char	title[ARB]				#I Plot title
char	xlbl[SZ_LINE], ylbl[SZ_LINE]		#I Plot labels

real	v1, v2, range
int	istart, npts
double	rv_shift2vel()
real	rv_maxpix(), rv_minpix()

begin
	# Now do the real plotting
	if (where == BOTTOM && (pltype == CORRELATION_PLOT || 
	    pltype == VCORRELATION_PLOT)) {
		istart = max (1, RV_WINCENTER(rv) + 1 - RV_WINDOW(rv))
	        npts = min (RV_WINR(rv) - RV_WINL(rv) + 1, RV_CCFNPTS(rv))
	        y2 = rv_maxpix (pldata[istart], npts)
	        y1 = rv_minpix (pldata[istart], npts)
	} else {
	    y2 = rv_maxpix (pldata, fnpts)
	    y1 = rv_minpix (pldata, fnpts)
	}
	range = abs (y2 - y1)
	if (dtype != BINARY_PLOT) {
	    y2 = y2 + (.15 * range)
	    y1 = y1 - (.12 * range)
	} else
	    y2 = y2 + (.35 * range)
	call gswind (gp, x1, x2, y1, y2)

	if ((pltype == CORRELATION_PLOT || pltype == VCORRELATION_PLOT) && 
	    where == BOTTOM) {

		if (dtype != BINARY_PLOT) {		# Force plot scaling
	    	    if (!IS_INDEF(RV_Y2(rv)))
	        	y2 = RV_Y2(rv)
		    else
			RV_Y2(rv) = y2
	    	    if (!IS_INDEF(RV_Y1(rv)))
	        	y1 = RV_Y1(rv)
		    else
			RV_Y1(rv) = y1
		    call gswind (gp, x1, x2, y1, y2)
		}

		if (dtype == SUMMARY_PLOT || dtype == BINARY_PLOT) {
		    call sp_color_viewport (gp)
	            call glabax (gp, "", xlbl, ylbl) 

		} else if (RV_DCFLAG(rv) == -1) {
		    call sp_color_viewport (gp)
	            call glabax (gp, title, xlbl, ylbl) 

		} else {
		    call gseti (gp, G_WCS, 1)		# Set attributes
		    v1 = real (rv_shift2vel(rv,real(RV_WINL(rv))))
		    v2 = real (rv_shift2vel(rv,real(RV_WINR(rv))))
	            call gsview (gp, 0.115, 0.95, 0.15, 0.725)
		    call gswind (gp, v1, v2, y1, y2)
		    call gseti (gp, G_YDRAWAXES, 0)
		    call gseti (gp, G_XDRAWAXES, 2)
		    call sp_color_viewport (gp)
		    call glabax (gp, "", "", "") 	# Draw top axis

		    call gseti (gp, G_WCS, 2)		# Draw bottom labels
		    call gseti (gp, G_XDRAWAXES, 1)
		    call gseti (gp, G_YDRAWAXES, 3)
	            call glabax (gp, "", xlbl, ylbl) 

		    call gseti (gp, G_WCS, 2)		# Restore attributes
		    call gseti (gp, G_XDRAWAXES, 3)
		    call gseti (gp, G_YDRAWAXES, 3)
		}

	} else if (pltype == CORRELATION_PLOT) {
	    call gseti (gp, G_LABELTICKS, NO)		# Set attributes
	    call gseti (gp, G_DRAWTICKS, NO)

	    if (where==TOP) { 				# Do the label
	        call glabax (gp, title, "", "")
	    } else if (where == MIDDLE) {
		call sp_color_viewport (gp)
		call glabax (gp, "", "", "") 
	    }

	    call gseti (gp, G_LABELTICKS, YES)	# Restore attributes
	    call gseti (gp, G_DRAWTICKS, YES)

	} else if (pltype == FOURIER_PLOT || pltype == PS_PLOT || 
	    pltype == FILTER_PLOT) {
	      if (where==TOP) { 			# Do the label
		call gseti (gp, G_WCS, 1)		# Do the top axis
		call gswind (gp, x1, x2, y1, y2)
	        call gsview (gp, 0.115, 0.95, 0.51, 0.865)
		call gseti (gp, G_XDRAWAXES, 2)
		call gseti (gp, G_XDRAWTICKS, 2)
		call gseti (gp, G_YDRAWAXES, 0)
	        call glabax (gp, "", "", "")

		call gseti (gp, G_WCS, 4)		# Do the plot title
		call gswind (gp, x1, x2, y1, y2)
	        #call gsview (gp, 0.115, 0.95, 0.51, 0.865)
	        call gsview (gp, 0.115, 0.95, 0.51, 0.91)
		call gseti (gp, G_XDRAWAXES, 0)
		call gseti (gp, G_YDRAWAXES, 0)
	        call glabax (gp, title, "", "")

	  	call gseti (gp, G_WCS, 2)		# Remainder of top plot
		call gseti (gp, G_YDRAWAXES, 3)
		call gseti (gp, G_XDRAWAXES, 1)
		call gseti (gp, G_XDRAWTICKS, 0)
		call gseti (gp, G_XLABELTICKS, NO)
		call gswind (gp, x1, x2, y1, y2)
	        call gsview (gp, 0.115, 0.95, 0.51, 0.865)
	        call glabax (gp, "", xlbl, ylbl)

		call gseti (gp, G_XDRAWTICKS, YES)	# Restore reality
		call gseti (gp, G_XLABELTICKS, YES)
		call gseti (gp, G_XDRAWAXES, 2)

	      } else if (where == BOTTOM) {
		call gseti (gp, G_WCS, 1)
		call gswind (gp, x1, x2, y1, y2)
	        call gsview (gp, 0.115, 0.95, 0.125, 0.5)
		call gseti (gp, G_XLABELTICKS, YES)
		call gseti (gp, G_XDRAWAXES, 1)

		call sp_color_viewport (gp)
	        call glabax (gp, "", xlbl, ylbl)

		call gseti (gp, G_XDRAWAXES, 2)		# Draw top boundary
	    	call gseti (gp, G_DRAWTICKS, NO)
		call glabax (gp, "", "", "")

	    	call gseti (gp, G_DRAWTICKS, YES)
	    	call gseti (gp, G_LABELTICKS, YES)
		call gseti (gp, G_XLABELTICKS, 2)
	      }

	} else if (pltype == SPECTRUM_PLOT || pltype == NORM_PLOT ||
	    pltype == PREPARED_PLOT) {

	    if (where == TOP) {
		if (dtype == SUMMARY_PLOT) {
		    call glabax (gp, title, "", ylbl)
		} else {
	    	    call gseti (gp, G_XLABELTICKS, NO)
		    call glabax (gp, title, "", ylbl)
	    	    call gseti (gp, G_XLABELTICKS, YES)
	  	}

	    } else if (where == BOTTOM) {
		call gseti (gp, G_XLABELTICKS, YES)
		call gseti (gp, G_XDRAWAXES, 3)
		call sp_color_viewport (gp)
	        call glabax (gp, "", xlbl, ylbl)
	    }

	} else {
	    call sp_color_viewport (gp)
	    call glabax (gp, title, xlbl, ylbl)
	}

	call gflush (gp)
end


# SP_DRAW_VECTOR - Draw the vector for the requested plot

procedure sp_draw_vector (rv, gp, pltype, where, x1, x2, y1, y2, pldata, xdata,
    fnpts, pnpts)

pointer	rv					#I RV struct pointer
pointer	gp					#I Graphics pointer
int	pltype					#I Type of plot to draw
int	where					#I Where to plot data
real	x1, x2, y1, y2				#I Boundaries of plot
real	pldata[ARB]				#I Vector to plot
real	xdata[ARB]				#I X-Vector to plot
int	fnpts, pnpts				#I FFT npts and plot npts

real	left, right
int	i, npts

begin
	switch (pltype) {
	case CORRELATION_PLOT:
	    if (DBG_QUICK(rv) == NO || (DBG_QUICK(rv) == YES && where==BOTTOM)){
                if (where == BOTTOM) {
                    i = RV_WINCENTER(rv) - RV_WINDOW(rv)
                    npts = 2 * RV_WINDOW(rv) + 1
                    call gpline (gp, xdata[i], pldata[i], npts)
                } else 
                    call gpline (gp, xdata[2], pldata[2], fnpts-2)
		call gflush (gp)
	    }

	    left = RV_WINL(rv)
	    right = RV_WINR(rv)
	    switch (where) {
	    case TOP:
		call gseti (gp, G_PLTYPE, GL_DASHED)
		call gseti (gp, G_PLCOLOR, C_RED)
		call gline (gp, left, y1, left, y2)
		call gline (gp, right, y1, right, y2)
		call gseti (gp, G_PLTYPE, GL_SOLID)
		call gseti (gp, G_PLCOLOR, C_FOREGROUND)
	    	call gline (gp, x1, 0.0, x2, 0.0) 		# Zero level
	    case MIDDLE:
		# Actual plot window
		call gseti (gp, G_PLTYPE, GL_DASHED)
		#call gseti (gp, G_PLCOLOR, C_RED)
		call gline (gp, real(left-RV_WINDOW(rv)), y1, 
		    		real(left-RV_WINDOW(rv)), y2)
		call gline (gp, real(right+RV_WINDOW(rv)), y1, 
		    		real(right+RV_WINDOW(rv)), y2)
		# Parameter plot window
		call gseti (gp, G_PLTYPE, GL_DOTTED)
		call gseti (gp, G_PLCOLOR, C_RED)
		call gline (gp, left, y1, left, y2)
		call gline (gp, right, y1, right, y2)
		call gseti (gp, G_PLTYPE, GL_SOLID)
		call gseti (gp, G_PLCOLOR, C_FOREGROUND)
	    	call gline (gp, x1, 0.0, x2, 0.0) 		# Zero level
	    case BOTTOM:
	        call gseti (gp, G_WCS, 2)
		if (RV_FITDONE(rv) == YES) {
		    if (IS_DBLSTAR(rv) == NO) {
	                call gpmark (gp, xdata[RV_ISTART(rv)], 
			    pldata[RV_ISTART(rv)], pnpts, 4, 2., 2.)
	 		call rv_draw_fit (rv, gp, NO)
			call gseti (gp, G_PLCOLOR, C_GREEN)
	 	        call rv_draw_background (rv, gp)
			call gseti (gp, G_PLCOLOR, C_FOREGROUND)
		    } else {
			i = DBL_I1(rv)
	                call gpmark (gp, xdata[i], pldata[i], DBL_NFITP(rv), 
			    4, 2., 2.)
	 		call rv_plt_deblend (rv, gp, NO)
		    }
		}
	    }
	    RV_X1(rv) = x1
	    RV_X2(rv) = x2

	case VCORRELATION_PLOT:
	    call gpline (gp, xdata, pldata, fnpts)
	    if (where == BOTTOM && RV_FITDONE(rv) == YES) {
		if (IS_DBLSTAR(rv) == NO) {
	            call gpmark (gp, xdata[RV_ISTART(rv)], 
			pldata[RV_ISTART(rv)], pnpts, 4, 2., 2.)
		    if (RV_DCFLAG(rv) != -1)
		        call rv_draw_fit (rv, gp, YES)
		    else
		        call rv_draw_fit (rv, gp, NO)
	 	    call rv_draw_background (rv, gp)
		} else {
		    i = DBL_I1(rv)
	            call gpmark (gp, xdata[i], pldata[i], DBL_NFITP(rv), 
		        4, 2., 2.)
	 	    call rv_plt_deblend (rv, gp, YES)
		}
	    }

	case CONVOLUTION_PLOT, ANTISYM_PLOT, FILTER_PLOT, PREPARED_PLOT:
	    if (DBG_QUICK(rv) == NO && RV_DTYPE(rv) != SUMMARY_PLOT)
	        call gpline (gp, xdata, pldata, fnpts)

	case SPECTRUM_PLOT, NORM_PLOT:
	    if (DBG_QUICK(rv) == NO && RV_DTYPE(rv) != SUMMARY_PLOT) {
		if (RV_DTYPE(rv) == OBJECT_SPECTRUM)
	            call gpline (gp, xdata, pldata, RV_NPTS(rv))
		else
	            call gpline (gp, xdata, pldata, RV_RNPTS(rv))
	    } else if (RV_DTYPE(rv) == SUMMARY_PLOT) {
	        call gpline (gp, xdata, pldata, RV_NPTS(rv))
	        #call gvline (gp, pldata, RV_NPTS(rv), x1, x2)
	    }
	default:
	    call gvline (gp, pldata, fnpts, x1, x2)
	}
	RV_GTYPE(rv) = pltype
	call gflush (gp)
end


# SP_ANNOTATE - Annotate the split plot to clarify what's what.

procedure sp_annotate (rv, gp, pltype, dtype, fnpts, x1, x2, y1, y2)

pointer	rv						#I RV struct pointer
pointer	gp						#I Graphics pointer
int	pltype, dtype					#I Plot and data types
int	fnpts						#I Npts in fft plot
real	x1, x2, y1, y2					#I Plot boundaries

double	dex()
real	l, r
int	i

begin
	call gseti (gp, G_TXCOLOR, RV_TXTCOLOR(rv))
	switch (pltype) {
	case FOURIER_PLOT, PS_PLOT:
	    if (dtype == OBJECT_SPECTRUM) {
	        if (RV_FILTER(rv) == OBJ_ONLY || RV_FILTER(rv) == BOTH) {
		    call gseti (gp, G_WCS, 2)
	            call gsview (gp, 0.115, 0.95, 0.51, 0.865)
		    call gswind (gp, x1, x2, y1, y2)
	            call fft_fltoverlay (rv, gp, int(fnpts*RVP_FFT_ZOOM(rv))*2, 
			y2)
		}
	    } else if (dtype == REFER_SPECTRUM) {
	        if (RV_FILTER(rv) == TEMP_ONLY || RV_FILTER(rv) == BOTH) {
		    call gseti (gp, G_WCS, 2)
	            call gsview (gp, 0.115, 0.95, 0.125, 0.5)
		    call gswind (gp, x1, x2, y1, y2)
	            call fft_fltoverlay (rv, gp, int(fnpts*RVP_FFT_ZOOM(rv))*2, 
			y2)
		}
	    }
	    if (RV_WHERE(rv) == TOP) {
	        call gctran (gp, 0.73, 0.8, x1, y1, 0, 2)
		if (pltype == FOURIER_PLOT)
	            call gtext (gp, x1, y1, "Object FFT", "")
		else
	            call gtext (gp, x1, y1, "Object PS", "")
	        call gctran (gp, 0.73, 0.77, x1, y1, 0, 1)
	    } else if (RV_WHERE(rv) == BOTTOM) {
	        call gctran (gp, 0.73, 0.43, x1, y1, 0, 1)
		if (pltype == FOURIER_PLOT)
	            call gtext (gp, x1, y1, "Template FFT", "") 
		else
	            call gtext (gp, x1, y1, "Template PS", "")
	        call gctran (gp, 0.73, 0.4, x1, y1, 0, 1)
	    }
	    if (RV_FILTER(rv) == BOTH || RV_FILTER(rv) == OBJ_ONLY) {
	        if (RVP_WHEN(rv) == BEFORE)
	            call gtext (gp, x1, y1, "Before Filter", "")
	        else
	            call gtext (gp, x1, y1, "After Filter", "")
	    }

	case NORM_PLOT, SPECTRUM_PLOT, PREPARED_PLOT, FILTER_PLOT:
	    if (RV_WHERE(rv) == TOP && dtype != SUMMARY_PLOT) {
	        call gctran (gp, 0.7, 0.55, x1, y1, 0, 1)
		switch (pltype) {
		case NORM_PLOT:
		    if (dtype == OBJECT_SPECTRUM)
	                call gtext (gp, x1, y1, "Norm. Object", "")
		    else
	                call gtext (gp, x1, y1, "Norm. Template", "")
		case SPECTRUM_PLOT:
		    if (dtype == OBJECT_SPECTRUM)
	               call gtext (gp, x1, y1, "Orig. Object", "")
		    else
	               call gtext (gp, x1, y1, "Orig. Template", "")
		case PREPARED_PLOT:
	            call gtext (gp, x1, y1, "Prepared Object", "")
		case FILTER_PLOT:
	            call gtext (gp, x1, y1, "Filtered Object", "")
		}
	    } else if (RV_WHERE(rv) == BOTTOM) {
	        call gctran (gp, 0.7, 0.175, x1, y1, 0, 1)
		switch (pltype) {
		case NORM_PLOT:
		    if (dtype == OBJECT_SPECTRUM)
	                call gtext (gp, x1, y1, "Norm. Object", "")
		    else
	                call gtext (gp, x1, y1, "Norm. Template", "")
		case SPECTRUM_PLOT:
		    if (dtype == OBJECT_SPECTRUM)
	               call gtext (gp, x1, y1, "Orig. Object", "")
		    else
	               call gtext (gp, x1, y1, "Orig. Template", "")
		case PREPARED_PLOT: 
		    call gtext (gp, x1, y1, "Prepared Temp.", "")
		case FILTER_PLOT:
		    if (dtype == OBJECT_SPECTRUM)
	                call gtext (gp, x1, y1, "Filtered Object", "")
		    else if (dtype == REFER_SPECTRUM)
	                call gtext (gp, x1, y1, "Filtered Temp.", "")
		}
	    }
	    if (dtype != SUMMARY_PLOT) {
		if (pltype != PREPARED_PLOT) {
		    if (RV_WHERE(rv) == TOP)
	 	        call rv_mark_regions (RV_OSAMPLE(rv), gp)
		    else
	 	        call rv_mark_regions (RV_RSAMPLE(rv), gp)
		}
	    } else if (dtype == SUMMARY_PLOT && RV_WHERE(rv) == TOP) {
		if (SR_COUNT(RV_OSAMPLE(rv)) != ALL_SPECTRUM) {
		    call gseti (gp, G_PLCOLOR, C_GREEN)
		    do i = 1, SR_COUNT(RV_OSAMPLE(rv)) {
            		l = SRANGE(RV_OSAMPLE(rv),i)
            		r = ERANGE(RV_OSAMPLE(rv),i)
            		if (RV_PIXCORR(rv) == NO && RV_DCFLAG(rv) != -1 &&
            		    SR_UNITS(RV_OSAMPLE(rv)) == PIXELS) {
                    		l = real (dex(RV_OW0(rv)+(l-1)*RV_OWPC(rv)))
                    		r = real (dex(RV_OW0(rv)+(r-1)*RV_OWPC(rv)))
            		}
	 	        call mark_range (gp, l, r)
		    }
		    call gseti (gp, G_PLCOLOR, C_FOREGROUND)
		}
	    }
	
	case ANTISYM_PLOT:
	    # Write the text
	    call gsview (gp, 0.05, 0.97, 0.30, 0.9)
	    call gswind (gp, 0.08, 0.97, 0.30, 0.9)
	    call gseti (gp, G_TXCOLOR, C_FOREGROUND)
	    call gtext (gp, 0.075, 0.45, "Correlation\000", "p=d")
	}
	call gseti (gp, G_TXCOLOR, C_FOREGROUND)
end


# SP_COLOR_VIEWPORT - Fill the specified viewport with the current background
# color.  For color terminals such as XGterm the background color is only
# drawn in the first graph on the screen.

procedure sp_color_viewport (gp)

pointer	gp					#I graphics pointer

real	x1, x2, y1, y2, xv[5], yv[5]

begin
	# Get the current viewport boundaries.
	call ggwind (gp, x1, x2, y1, y2)

	# Fill the polygon vector and color the area.
        xv[1] = x1;  yv[1] = y1
        xv[2] = x2;  yv[2] = y1
        xv[3] = x2;  yv[3] = y2
        xv[4] = x1;  yv[4] = y2
        xv[5] = x1;  yv[5] = y1
        call gseti (gp, G_FACOLOR, 0)
        call gfill (gp, xv, yv, 4, GF_SOLID)
	call gflush (gp)
end


# SP_SPECTRUM -  Set window boundaries and title for a spectrum plot

procedure sp_spectrum (rv, title, x_lbl, y_lbl, x1, x2, fnpts, xdata, pltype)

pointer	rv					#I RV struct pointer
pointer	title					#O Plot title pointer
pointer	x_lbl					#O Plot x label pointer
pointer	y_lbl					#O Plot y label pointer
real	x1, x2					#O Endpoints
int	fnpts					#I Npts to plot
real	xdata[fnpts]				#O X-axis data
int	pltype					#I Plot type

int	i, fft_pow2()

begin
	call get_plot_title (rv, title, fnpts)
	call strcpy ("Intensity", Memc[y_lbl], SZ_FNAME)
	if (RV_DCFLAG(rv) == -1 || pltype == PREPARED_PLOT) {
	    call strcpy ("Pixel", Memc[x_lbl], SZ_FNAME)
	    x1 = 1.
	    if (pltype == PREPARED_PLOT) {
                i = int ((RV_GLOB_W2(rv) - RV_GLOB_W1(rv)) / RV_OWPC(rv) + 1)
	        x2 = fft_pow2 (i)
		if (RV_RW0(rv) > RV_OW2(rv) || RV_OW0(rv) > RV_RW2(rv))
	    	    x2 = x2 * 2
	    } else
		x2 = real (fnpts)
	    for (i=int(x2); i>=1; i=i-1)
	  	xdata[i] = real[i]
	} else {
	    call strcpy ("Wavelength", Memc[x_lbl], SZ_FNAME)
	    if (RV_DTYPE(rv) == SUMMARY_PLOT) {
	        x1 = 10. ** (RV_OW0(rv))
	        x2 = 10. ** (RV_OW2(rv))
	    } else {
	        x1 = 10. ** (RV_GLOB_W1(rv))
	        x2 = 10. ** (RV_GLOB_W2(rv))
	    }

	    if (pltype == FILTER_PLOT || 
		pltype == NORM_PLOT || 
		pltype == SPECTRUM_PLOT) {
	            if (RV_DTYPE(rv) == REFER_SPECTRUM) {
	                do i = 1, fnpts
	                    xdata[i] = 10. ** (RV_RW0(rv) + (i-1) * RV_RWPC(rv))
		    } else {
	                do i = 1, fnpts
		            xdata[i] = 10. ** (RV_OW0(rv) + (i-1) * RV_OWPC(rv))
		    }
	    } else if (RV_WHERE(rv) == TOP) {
	        do i = 1, fnpts
		    xdata[i] = 10. ** (RV_OW0(rv) + (i-1) * RV_OWPC(rv))
	    } else {
	        do i = 1, fnpts
		    xdata[i] = 10. ** (RV_RW0(rv) + (i-1) * RV_RWPC(rv))
	    }
	}
end


# SP_VCORRELATION - Set window boundaries and titles for a velocity CCF
# plot.

procedure sp_vcorrelation (rv, title, x_lbl, y_lbl, x1, x2)

pointer	rv					#I RV struct pointer
pointer	title					#O Plot title pointer
pointer	x_lbl					#O Plot x label pointer
pointer	y_lbl					#O Plot y label pointer
real	x1, x2					#O Endpoints
double	rv_shift2vel()
real	min(), max()

begin
	if (RV_DCFLAG(rv) == -1) {
	    call strcpy ("Pixel Correlation - No velocities will be computed",
		Memc[title], SZ_LINE)
	    x1 = max (WRKPIXX(rv,1), real(RV_WINL(rv)-RV_WINDOW(rv)))
	    x2 = min (WRKPIXX(rv,RV_CCFNPTS(rv)), 
		real(RV_WINR(rv)+RV_WINDOW(rv)))
	    call strcpy ("Pixel Shift", Memc[x_lbl], SZ_FNAME)
	} else {
	    call sprintf (Memc[title], SZ_LINE,
	        "Correlation function  Template = '%s'")
		    call pargstr (TEMPNAME(rv))
	    if (RV_DTYPE(rv) == SUMMARY_PLOT) {
	        x1 = max (WRKPIXX(rv,1), real(RV_WINL(rv)-RV_WINDOW(rv)))
	        x2 = min (WRKPIXX(rv,RV_CCFNPTS(rv)), 
		    real(RV_WINR(rv)+RV_WINDOW(rv)))
	    } else {
	        x1 = max (WRKPIXX(rv,1), real(RV_WINL(rv)))
	        x2 = min (WRKPIXX(rv,RV_CCFNPTS(rv)), real(RV_WINR(rv)))
	    }
	    x1 = real (rv_shift2vel(rv,x1))
	    x2 = real (rv_shift2vel(rv,x2))
	    call strcpy ("Relative Velocity (Km/sec)", Memc[x_lbl], SZ_FNAME)
	}
	call strcpy ("Correlation", Memc[y_lbl], SZ_FNAME)
end


# SP_CORRELATION - Set window boundaries and titles for a CCF plot.

procedure sp_correlation (rv, where, title, x_lbl, y_lbl, x1, x2)

pointer	rv					#I RV struct pointer
int	where					#I Where is the plot located?
pointer	title					#O Plot title pointer
pointer	x_lbl					#O Plot x label pointer
pointer	y_lbl					#O Plot y label pointer
real	x1, x2					#O Endpoints

real	min(), max()

begin
	if (where == BOTTOM) {
	    if (RV_DCFLAG(rv) == -1) {
	        call strcpy (
		  "Pixel Correlation - No velocities will be computed",
		    Memc[title], SZ_LINE)
	    } else {
	        call sprintf (Memc[title], SZ_LINE,
	            "Correlation function  Template = '%s'")
		        call pargstr (TEMPNAME(rv))
	    }
	} else
	    call get_anplot_title (rv, title)

	call strcpy ("Correlation", Memc[y_lbl], SZ_FNAME)
	call strcpy ("Pixel Shift", Memc[x_lbl], SZ_FNAME)
	x1 = max (real(RV_WINL(rv)), WRKPIXX(rv,1)-1)
	x2 = min (real(RV_WINR(rv)), WRKPIXX(rv,RV_CCFNPTS(rv))+1)
end


# SP_ANPLOT - Set window boundaries and title for an antisymmetric noise
# plot.

procedure sp_anplot (rv, title, x_lbl, y_lbl, x1, x2)
 
pointer rv                                      #I RV struct pointer
pointer title                                   #O Plot title pointer
pointer x_lbl                                   #O Plot x label pointer
pointer y_lbl                                   #O Plot y label pointer
real    x1, x2                                  #O Endpoints
 
begin
        call sprintf (Memc[title], SZ_LINE,
            "Antisymmetric Noise Component of CCF")
        call strcpy ("", Memc[y_lbl], SZ_FNAME)
        call strcpy ("Lag", Memc[x_lbl], SZ_FNAME)
        x1 = WRKPIXX(rv,1)
        x2 = WRKPIXX(rv,RV_CCFNPTS(rv))
end
 
 
# SP_FOURIER - Set window boundaries and title for an FFT plot.

procedure sp_fourier (rv, dtype, where, rinpt, pldata, title, x_lbl, y_lbl, 
    x1, x2, npts, fnpts)

pointer	rv					#I RV struct pointer
int	dtype					#I Data type to plot
int	where					#I Where to plot the data
real	rinpt[npts]				#I Input plot array
pointer	pldata					#O Output plot array
pointer	title					#O Plot title pointer
pointer	x_lbl					#O Plot x label pointer
pointer	y_lbl					#O Plot y label pointer
real	x1, x2					#O Endpoints
int	npts					#I Npts in data
int	fnpts					#I Npts in fft

begin
	call get_fft (rv, rinpt, npts, Memr[pldata], fnpts)
	fnpts = max (RV_FFTNPTS(rv), fnpts) / 2
	if (where != BOTTOM)
	    call get_anplot_title (rv, title)
	else
	    call strcpy ("", Memc[title], SZ_FNAME)

	if (RVP_LOG_SCALE(rv) == YES)
	    call strcpy ("log(|G(k)|)", Memc[y_lbl], SZ_FNAME)
	else
	    call strcpy ("|G(k)|", Memc[y_lbl], SZ_FNAME)

	switch (where) {
	case TOP:
	    call strcpy ("Frequency", Memc[x_lbl], SZ_FNAME)
	    x1 = 0.0
	    x2 = (real (fnpts) / RVP_FFT_ZOOM(rv)) / (2. * real (fnpts))
	case BOTTOM:
	    call strcpy ("Wavenumber", Memc[x_lbl], SZ_FNAME)
	    x1 = 1.
	    x2 = real (fnpts) / RVP_FFT_ZOOM(rv)
	}
end


# SP_PSPLOT - Set window boundaries and title for a power spectrum plot.

procedure sp_psplot (rv, dtype, where, rinpt, pldata, title, x_lbl, y_lbl, 
    x1, x2, npts, fnpts)

pointer	rv					#I RV struct pointer
int	dtype					#I Data type to plot
int	where					#I Where to plot the data
real	rinpt[npts]				#I Input plot array
pointer	pldata					#O Output plot array
pointer	title					#O Plot title pointer
pointer	x_lbl					#O Plot x label pointer
pointer	y_lbl					#O Plot y label pointer
real	x1, x2					#O Endpoints
int	npts					#I Npts in data
int	fnpts					#O Npts to plot


begin
	call get_pspec (rv, rinpt, npts, Memr[pldata], fnpts)
	fnpts = max (RV_FFTNPTS(rv), fnpts) / 2
	call get_anplot_title (rv, title)

	if (RVP_LOG_SCALE(rv) == YES)
	    call strcpy ("log(|Power|)", Memc[y_lbl], SZ_FNAME)
	else
	    call strcpy ("Power", Memc[y_lbl], SZ_FNAME)

	switch (where) {
	case TOP:
	    call strcpy ("Frequency", Memc[x_lbl], SZ_FNAME)
	    x1 = 0.0
	    x2 = (real (fnpts) / RVP_FFT_ZOOM(rv)) / (2. * real (fnpts))
	case BOTTOM:
	    call strcpy ("Wavenumber", Memc[x_lbl], SZ_FNAME)
	    x1 = 1.
	    x2 = real (fnpts) / RVP_FFT_ZOOM(rv)
	}
end
