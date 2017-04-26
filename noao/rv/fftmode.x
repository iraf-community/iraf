include <gset.h>
include "rvpackage.h"
include "rvflags.h"
include "rvcomdef.h"
include "rvplots.h"
include "rvfilter.h"


# FFT_COLON - Procedure to process the colon commands defined below.  Most
# commands are for interactive editing of parameters to the task.

int procedure fft_colon (rv, cmdstr)

pointer	rv				#I RV struct pointer
char	cmdstr[SZ_LINE]			#I Command

pointer sp, cmd
int 	strdic()

begin
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)

	call sscan (cmdstr)
	call gargwrd (Memc[cmd], SZ_LINE)

	# Unpack the keyword from the string and look it up in the
	# dictionary.  Switch on command and call the appropriate routines.

        if (strdic(Memc[cmd], Memc[cmd], SZ_LINE, FILT_KEYWORDS) != 0) {
            # Process the FILTERPARS pset commands.
            call filt_colon (rv, cmdstr)

	} else { 
	    # Now process the mode specific colon commands.
	    switch (strdic(Memc[cmd], Memc[cmd], SZ_FNAME, PLOT_KEYWORDS)) {
	    case PLT_FILTER:
	        call cmd_filter (rv)

	    case PLT_FFT_ZOOM:
	        call cmd_fft_zoom (rv)

	    case PLT_LOG_SCALE:
	        call cmd_log_scale (rv)

	    case PLT_ONE_IMAGE:
	        call cmd_one_image (rv)

	    case PLT_OVERLAY:
	        call cmd_overlay (rv)

	    case PLT_PLOT:
	        call cmd_plot (rv)

	    case PLT_SPLIT_PLOT:
	        call cmd_split_plotx (rv)

	    case PLT_WHEN:
	        call cmd_when (rv)

	    default:
	        # Default action
		call rv_mode_prompt (rv)
	        call rv_errmsg ("Type '?' for a list of commands.")
	        call sfree (sp)
	        return (ERR)
	    }
	}

	call sfree (sp)
	return (OK)
end


# FFT_CURSOR - Get the next command from the user in the input cursor loop
# and perform the requested function.

int procedure fft_cursor (rv)

pointer	rv				#I RV struct pointer

pointer	gp, sp, cmd, filt
int	wcs, key
int	ofnpts, rfnpts
real	x, y
char	ckey
bool	prompt

int 	fft_colon(), clgcur(), stridx(), spc_cursor()
int	rv_parent(), fft_pow2(), rv_chk_filter()

define	exit_			99
define	replot_			98

begin
	# Update the mode counter.
	RV_MODES(rv) = (RV_MODES(rv)*10) + FFT_MODE

	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)

	# Nab some pointers.
	ofnpts = fft_pow2 (RV_NPTS(rv))
	rfnpts = fft_pow2 (RV_RNPTS(rv))
	gp = RV_GP(rv)


#	if (RVF_LASTKEY(rv) == 'p' && RV_FILTER(rv) != NONE)
#	    key = 'p'
#	else
	    key = 'b'
	repeat {

	    RV_CMD(rv) = key
	    ckey = key
    	    if (stridx(ckey,":?iqrsx") == 0)
	        RVF_LASTKEY(rv) = key
	    prompt = true
replot_     RV_NEWGRAPH(rv) = NO
	    switch (key) { 			# switch on the keystroke
	    case '?':				
	    	# List options
		call gpagefile (gp, FM_HELP, "FFT Mode Options: ")

	    case ':':				
		# Process a colon command.
	    	if (fft_colon(rv,Memc[cmd]) == OK) {
		    if (RV_NEWGRAPH(rv) == YES) {
		        key = RVF_LASTKEY(rv)
		        goto replot_
		    }
		}
		prompt = false

	    case 'b':
		# Display power spectra before filtering
		RVP_WHEN(rv) = BEFORE
		RVP_PLOT(rv) = POWER_PLOT
		call fft_plot (rv, RVP_PLOT(rv))

	    case 'f':
		# Display FFT's after filtering.
		if (RV_FILTER(rv) == NONE) {
		    call rv_mode_prompt (rv)
	    	    call rv_errmsg ("Filtering is currently disabled.")
		    prompt = false
		} else {
		    RVP_WHEN(rv) = AFTER
		    RVP_PLOT(rv) = AMPLITUDE_PLOT
		    call fft_plot (rv, RVP_PLOT(rv))
		}

	    case 'g':
		# Display FFT's before filtering.
		RVP_WHEN(rv) = BEFORE
		RVP_PLOT(rv) = AMPLITUDE_PLOT
		call fft_plot (rv, RVP_PLOT(rv))

	    case 'i':
		# Print period trend information.
		call rv_mode_prompt (rv)
		call fft_inverse (rv, x, y, wcs)
	        prompt = false

	    case 'I':
		call error (0, "Interrupt")

	    case 'o':
		# Display filtered and unfiltered object spectrum.
		if (RV_FILTER(rv) == OBJ_ONLY || RV_FILTER(rv) == BOTH) {
		    if (rv_chk_filter(rv, OBJECT_SPECTRUM) != OK) {
		        call rv_mode_prompt (rv)
	    	        call rv_errmsg ("Invalid filter specified.")
		        prompt = false
		    } else {
		        call malloc (filt, ofnpts, TY_REAL)
		        call gclear (gp)
		        if (RV_CONTINUUM(rv)==BOTH || 
			    RV_CONTINUUM(rv)==OBJ_ONLY) {
		        	call amovr (OCONT_DATA(rv,1), Memr[filt], 
				    RV_NPTS(RV))
		                call split_plot (rv, gp, TOP, OCONT_DATA(rv,1),
				    RV_NPTS(rv), OBJECT_SPECTRUM, NORM_PLOT)
		        } else {
		            call amovr (OBJPIXY(rv,1), Memr[filt], RV_NPTS(rv))
		            call split_plot (rv, gp, TOP, OBJPIXY(rv,1), 
				RV_NPTS(rv), OBJECT_SPECTRUM, SPECTRUM_PLOT)
		        }
		        call rv_do_filter (rv, RV_OSAMPLE(rv), Memr[filt], 
			    RV_NPTS(rv), Memr[filt], ofnpts, NO)
	                call split_plot (rv, gp, BOTTOM, Memr[filt], 
			    RV_NPTS(rv), OBJECT_SPECTRUM, FILTER_PLOT)
		        call mfree (filt, TY_REAL)
		    }
		} else {
		    call rv_mode_prompt (rv)
		    call rv_errmsg ("Filtering disabled for object spectrum.")
		    prompt = false
		}

	    case 'p':
		# Display power spectra after filtering.
		if (RV_FILTER(rv) == NONE) {
		    call rv_mode_prompt (rv)
	    	    call rv_errmsg ("Filtering is currently disabled.")
		    prompt = false
		} else {
		    RVP_WHEN(rv) = AFTER
		    RVP_PLOT(rv) = POWER_PLOT
		    call fft_plot (rv, RVP_PLOT(rv))
		}

	    case 'q':				
		# Quit this mode.
		break

	    case 'r':				
		# Replot.
	        if (RVF_LASTKEY(rv) != 'r')
	 	    key = RVF_LASTKEY(rv)
		goto replot_

	    case 's':
		# Display the spectra.
		if (rv_parent(rv) == SPEC_MODE) {
		    goto exit_
		} else if (spc_cursor (rv) == QUIT) {
		    RV_MODES(rv) = (RV_MODES(rv) - FFT_MODE) / 10
		    call sfree (sp)
		    return (QUIT)
		} else {
		    key = RVF_LASTKEY(rv)
		    goto replot_
	 	}

	    case 't':
		# Display filtered and unfiltered template spectrum.
		if (RV_FILTER(rv) == TEMP_ONLY || RV_FILTER(rv) == BOTH) {
		    if (rv_chk_filter(rv, REFER_SPECTRUM) != OK) {
		        call rv_mode_prompt (rv)
	    	        call rv_errmsg ("Invalid filter specified.")
		        prompt = false
		    } else {
		        call malloc (filt, rfnpts, TY_REAL)
		        call gclear (gp)
		        if (RV_CONTINUUM(rv)==BOTH || 
			    RV_CONTINUUM(rv)==TEMP_ONLY) {
		                call amovr (RCONT_DATA(rv,1), Memr[filt], 
				    RV_RNPTS(rv))
		                call split_plot (rv, gp, TOP, RCONT_DATA(rv,1),
				    RV_RNPTS(rv), REFER_SPECTRUM, NORM_PLOT)
		        } else {
		            call amovr (REFPIXY(rv,1), Memr[filt], RV_RNPTS(rv))
		            call split_plot (rv, gp, TOP, REFPIXY(rv,1), 
				RV_RNPTS(rv), REFER_SPECTRUM, SPECTRUM_PLOT)
		        }
		        call rv_do_filter (rv, RV_RSAMPLE(rv), Memr[filt], 
			    RV_RNPTS(rv), Memr[filt], rfnpts, NO)
	                call split_plot (rv, gp, BOTTOM, Memr[filt], 
			    RV_RNPTS(rv), REFER_SPECTRUM, FILTER_PLOT)
		        call mfree (filt, TY_REAL)
		    }
		} else {
		    call rv_mode_prompt (rv)
		    call rv_errmsg ("Filtering disabled for template spectrum.")
		    prompt = false
		}

	    case 'x':
		# Return to correlation mode.
		RV_MODES(rv) = (RV_MODES(rv) - FFT_MODE) / 10
		call sfree (sp)
		return (QUIT)

	    default:
	    	# Unknown command.
		call rv_mode_prompt (rv)
		call rv_errmsg ("Type '?' for a list of commands.")
		prompt = false
	    }

	    if (prompt)
	        call rv_mode_prompt (rv)
	    ckey = key
    	    if (stridx(ckey,":?iqrsx") == 0)
	        RVF_LASTKEY(rv) = key

	} until (clgcur("cursor",x,y,wcs,key,Memc[cmd],SZ_LINE) == EOF)

exit_ 	call sfree (sp)
	RV_MODES(rv) = (RV_MODES(rv) - FFT_MODE) / 10
	return (OK)
end


# FFT_PLOT - Do the plotting for the FFT plotting subpackage and determine
# the type of plot to draw.

procedure fft_plot (rv, flags)

pointer	rv				#I RV struct pointer
int	flags				#I Type of plot to draw

begin
        # Get the graphics pointer and clear the workstation.
        if (RV_GP(rv) != NULL)
            call gclear (RV_GP(rv))                             
        else
            return

        # Call the plot primitives.
        switch (flags) {
        case AMPLITUDE_PLOT:
            call fft_fplot (rv, RVP_SPLIT_PLOT(rv))
        case POWER_PLOT:
            call fft_pplot (rv, RVP_SPLIT_PLOT(rv))
        default:
            call error (0, "Invalid FFT plot specification.")
        }
end


# FFT_FLTPLOT - Plot the (filtered) spectrum to the screen.

procedure fft_fltplot (rv, gp, vec, npts)

pointer	rv				#I RV struct pointer
pointer	gp				#I graphics descriptor
real	vec[ARB]			#I Data to be plotted
int	npts				#I Npts of data to plot

pointer	sp
pointer	objx, title, idsys
int	i
real	x1, x2, y1, y2
double	dex()

begin
	if (gp == NULL)
	    return

	call smark (sp)
	call salloc (objx, npts, TY_REAL)
	call salloc (idsys, SZ_LINE, TY_CHAR)
	call salloc (title, 4*SZ_LINE, TY_CHAR)

	call gclear (gp)

	# Scale the vector.
	if (RV_DCFLAG(rv) == -1) {
	    do i = 1, npts 
	        Memr[objx+i-1] = real (i)
	} else {
	    do i = 1, npts 
	        Memr[objx+i-1] = dex (RV_OW0(rv) + (i-1) * RV_OWPC(rv))
	}

	# Scale the WCS.
	call gascale (gp, Memr[objx], npts, 1)
	call gascale (gp, vec, npts, 2)

	# Force a pretty Y scaling.
	call ggwind (gp, x1, x2, y1, y2)
	y1 = y1 - (0.02*y1)
	y2 = y2 + (0.02*y2)
	call gswind (gp, x1, x2, y1, y2)
		
	# Do the title and labeling.
	call sysid (Memc[idsys], SZ_LINE)
	call sprintf (Memc[title], 4*SZ_LINE,
	    "%s\nObject='%s' Star='%s'\nnpts=%d aperture=%d")
	         call pargstr (Memc[idsys])
	         call pargstr (IMAGE(rv))
		 call pargstr (OBJNAME(rv))
	         call pargi (npts)
		 call pargi (RV_APNUM(rv))
	call glabax (gp, Memc[title], "wavelength", "intensity")

	# Draw the vectors.
	call gpline (gp, Memr[objx], vec, npts)

	call gflush (gp)
	call sfree (sp)
end


# FFT_GFILTER - Fill an array with the requested filter function so that
# it may be overplotted on the FFT plot.

procedure fft_gfilter (rv, filt, npts, y2)

pointer	rv				#I RV struct pointer
real	filt[npts]			#U FFT data array
int	npts				#I no. elements in fft[]
real	y2				#I Upper limit of plot

pointer	sp, buf
int	i, npts2
real	tmp

begin
	call smark (sp)
	call salloc (buf, 2*npts, TY_REAL)
	call aclrr (Memr[buf], 2*npts)

	npts2 = 2 * npts 			# initializations
	if (RVP_LOG_SCALE(rv) == YES)
	    y2 = 10.0 ** y2
	call amovkr (y2, Memr[buf], npts2)

	# Now apply the filter to get the function.
	call rv_filter (rv, Memr[buf], npts)

	# Now recover the filter function.
	do i = 1, npts {
	    tmp = Memr[buf+i-1]
	    if (RVP_LOG_SCALE(rv) == YES && tmp != 0.0)
	        filt[i] = log10 (tmp) 
	    else
	        filt[i] = tmp
	}
	
	call sfree (sp)
end


# PLOT_OVERLAY - Plot the filter function overlayed on the FFT plot.

procedure fft_fltoverlay (rv, gp, fnpts, y2)

pointer	rv				#I RV struct pointer
pointer	gp				#I Graphics decriptor
int	fnpts				#I Npts in FFT
real	y2				#I Current Y2 of window

pointer	sp, filt
real	startp, endp
int	rv_chk_filter(), fft_pow2()

begin
	if (gp == NULL || RVP_OVERLAY(rv) == NO)
	    return

	# Check for a nonsensical filter specification.
	if (RV_WHERE(rv) == TOP && rv_chk_filter(rv,OBJECT_SPECTRUM) != OK)
	    return
	if (RV_WHERE(rv) == BOTTOM && rv_chk_filter(rv,REFER_SPECTRUM) != OK)
	    return

	fnpts = max (fft_pow2 (RV_NPTS(rv)), fft_pow2 (RV_RNPTS(rv)) ) / 2

	# Allocate working space.
	call smark (sp)			
	call salloc (filt, 2*fnpts, TY_REAL)

	# Get the filter to be plotted.
	y2 = y2 - (0.075 * y2)
	call fft_gfilter (rv, Memr[filt], 2*fnpts, y2)

	# Compute the endpoint.
	if (RV_WHERE(rv) == BOTTOM) {
	    startp = 1.
	    endp = real(fnpts) / RVP_FFT_ZOOM(rv)
	} else {
	    startp = 0.
	    endp = (real(fnpts) / RVP_FFT_ZOOM(rv)) / (2. * real(fnpts))
	}
	fnpts = fnpts * 2

	# Now plot the filter function.
	call gseti (gp, G_PLCOLOR, C_RED)
	call gvline (gp, Memr[filt], int(fnpts/RVP_FFT_ZOOM(rv)), startp, endp)
	call gseti (gp, G_PLCOLOR, C_FOREGROUND)

        if (DEBUG(rv)) {
            call d_printf (DBG_FD(rv),"flt_overlay:\tstart=%g end=%g\n")
		call pargr (startp) ; call pargr (endp)
            call d_printf (DBG_FD(rv),"\t\tnew=%d np=%d rnp=%d fnp=%d\n")
		call pargi (fnpts) ;call pargi (RV_NPTS(rv))
		call pargi (RV_RNPTS(rv)) ; call pargi (RV_FFTNPTS(rv)) 
	    call flush (DBG_FD(rv))
	}


	call sfree (sp)
end


# FFT_FPLOT - Plot the (two) Fourier transforms to the screen.

procedure fft_fplot (rv, flag)

pointer	rv				#I RV struct pointer
int	flag				#I Type of flag to print (SINGLE/SPLIT)

pointer	gp
pointer	sp, rfft, title, bp, ylbl
int	fnpts
int	fft_pow2()
real	x1, x2, y1, y2, startp, endp

begin
	gp = RV_GP(rv)
	if (gp == NULL)
	    return

	# Allocate working space.
	call smark (sp)			
	call salloc (rfft, 2*fft_pow2(RV_NPTS(rv)), TY_REAL)
	call salloc (title, 2*SZ_LINE, TY_CHAR)
	call salloc (ylbl, SZ_LINE, TY_CHAR)
	call salloc (bp, SZ_LINE, TY_CHAR)

	# Clear the screen.
	call gclear (gp)

	if (flag == SINGLE_PLOT) {
	    if (RVP_ONE_IMAGE(rv) == OBJECT_SPECTRUM) {
	        if (RV_CONTINUUM(rv) != NONE) {
	            call get_fft (rv,OCONT_DATA(rv,1), RV_NPTS(rv), Memr[rfft],
		        fnpts)
	        } else {
	            call get_fft (rv,OBJPIXY(rv,1), RV_NPTS(rv), Memr[rfft], 
		        fnpts)
	        }
	    } else {
	        if (RV_CONTINUUM(rv) != NONE) {
	            call get_fft (rv,RCONT_DATA(rv,1), RV_RNPTS(rv), Memr[rfft],
		        fnpts)
	        } else {
	            call get_fft (rv,REFPIXY(rv,1), RV_RNPTS(rv), Memr[rfft], 
		        fnpts)
	        }
	    }

	    # Draw the plot to the screen
	    fnpts = max (RV_FFTNPTS(rv), fnpts) / 2
	    call gascale (gp, Memr[rfft], int(fnpts/RVP_FFT_ZOOM(rv)), 2)
	    call ggwind (gp, x1, x2, y1, y2)

	    call get_anplot_title (rv, title)
	    if (RVP_LOG_SCALE(rv) == YES) 
		call strcpy ("log(|G(k)|)", Memc[ylbl], SZ_FNAME)
	    else
		call strcpy ("|G(k)|", Memc[ylbl], SZ_FNAME)

	    # Draw the top axis labels
	    startp = 0.0
	    endp = (real(fnpts) / RVP_FFT_ZOOM(rv)) / (2. * real(fnpts))
	    call gseti (gp, G_WCS, 1)
	    call gsview (gp, 0.115, 0.95, 0.125, 0.8)
	    call gswind (gp, startp, endp, y1, y2)
	    call gseti (gp, G_XDRAWAXES, 2)
	    call glabax (gp, "", "", Memc[ylbl])

	    # Draw the bottom axis labels
	    startp = 1.0
	    endp = real(fnpts) / RVP_FFT_ZOOM(rv)
	    call gseti (gp, G_WCS, 2)
	    call gsview (gp, 0.115, 0.95, 0.125, 0.8)
	    call gswind (gp, startp, endp, y1, y2)
	    call gseti (gp, G_XDRAWAXES, 1)
	    call gseti (gp, G_YDRAWAXES, 3)
	    call glabax (gp, "", "Wavenumber", Memc[ylbl])

	    # Do the plot title
	    call gseti (gp, G_WCS, 3)
	    call gsview (gp, 0.115, 0.95, 0.125, 0.845)
	    call gswind (gp, startp, endp, y1, y2)
	    call gseti (gp, G_XDRAWAXES, 0)
	    call gseti (gp, G_YDRAWAXES, 0)
	    call glabax (gp, Memc[title], "", "")
	    call gsview (gp, 0.115, 0.95, 0.125, 0.8)

	    call gvline (gp, Memr[rfft], int(fnpts/RVP_FFT_ZOOM(rv)), 
		startp, endp)

	    # Lastly, annotate ther plot so we know what we're looking at.
	    call gctran (gp, 0.73, 0.73, x1, y1, 0, 1)
	    call gseti (gp, G_TXCOLOR, RV_TXTCOLOR(rv))
	    if (RVP_ONE_IMAGE(rv) == OBJECT_SPECTRUM)
	        call gtext (gp, x1, y1, "Object FFT", "")
	    else
	        call gtext (gp, x1, y1, "Template FFT", "")
	    call gctran (gp, 0.73, 0.69, x1, y1, 0, 1)
            if (RV_FILTER(rv) == BOTH || RV_FILTER(rv) == OBJ_ONLY) {
	        call fft_fltoverlay (rv, gp, fnpts*2, y2)
                if (RVP_WHEN(rv) == BEFORE)
                    call gtext (gp, x1, y1, "Before Filter", "")
                else 
                    call gtext (gp, x1, y1, "After Filter", "")
            }
	    call gseti (gp, G_TXCOLOR, C_FOREGROUND)
	    call gseti (gp, G_XDRAWAXES, 3)		# reset gio flags
 
	} else if (flag == SPLIT_PLOT) {

	    # Plot the Object power stectrum along the top.
	    if (RV_CONTINUUM(rv) == OBJ_ONLY || RV_CONTINUUM(rv) == BOTH) {
	        call split_plot (rv, gp, TOP, OCONT_DATA(rv,1), RV_NPTS(rv), 
	            OBJECT_SPECTRUM, FOURIER_PLOT)
	    } else {
	        call split_plot (rv, gp, TOP, OBJPIXY(rv,1), RV_NPTS(rv), 
	            OBJECT_SPECTRUM, FOURIER_PLOT)
	    }

	    # Template power spectrum along the bottom.
	    if (RV_CONTINUUM(rv) == TEMP_ONLY || RV_CONTINUUM(rv) == BOTH) {
	        call split_plot (rv, gp, BOTTOM, RCONT_DATA(rv,1), 
	            RV_RNPTS(rv), REFER_SPECTRUM, FOURIER_PLOT)
	    } else {
	        call split_plot (rv, gp, BOTTOM, REFPIXY(rv,1), 
	            RV_RNPTS(rv), REFER_SPECTRUM, FOURIER_PLOT)
	    }
		
	}

	call sfree (sp)
end


# FFT_INVERSE - Print the inverse of the X-axis of the current plot.  The intent
# of this routine is to provide an easy mechanism for users to translate the
# frequency of a point on the power spectrum into a period trend in the data.
# It may also be used to translate wavelengths into approximate wavenumbers.

procedure fft_inverse (rv, x, y, wcs)

pointer	rv					#I RV struct pointer
real	x, y					#I Current cursor (x,y)
int	wcs					#I WCS of cursor read

pointer	gp
real	period
real	x1, x2, y1, y2, xx, yy
int	fnpts, fft_pow2()

begin
	gp = RV_GP(rv)
	if (gp == NULL)
	    return

	# Switch based on the plot flags.
	call ggview (gp, x1, x2, y1, y2)
	call gctran (gp, x, y, x, y, wcs, 1)
	call gctran (gp, x, y, xx, yy, 1, 0)

	fnpts = fft_pow2 (max(RV_NPTS(rv),RV_RNPTS(rv))) / 2
	if (xx < x1 || xx > x2) {		# outside plot window
	    call printf ("Period trend in the data = INDEF.")
	    return
	} else {
	    period = (1. / x) * real(fnpts * 2.)
	    call printf (
		"Period trend in the data = %.2f pixels (K = %d, f = %.3f).")
		    call pargr (period)
		    call pargi (int(x))
		    call pargr (x/real(fnpts))
	} 
end


# FFT_PPLOT - Plot the (two) Fourier power spectra to the screen.

procedure fft_pplot (rv, flag)

pointer	rv				#I RV struct pointer
int	flag				#I Type of flag to print (SINGLE/SPLIT)

pointer	gp
pointer	sp, rfft, title, bp, xlbl, ylbl
int	fnpts
int	fft_pow2()
real	startp, endp
real	x1, x2, y1, y2

begin
	gp = RV_GP(rv)
	if (gp == NULL)
	    return

	# Allocate working space.
	call smark (sp)			
	call salloc (bp, SZ_LINE, TY_CHAR)
	call salloc (xlbl, SZ_LINE, TY_CHAR)
	call salloc (ylbl, SZ_LINE, TY_CHAR)
	call salloc (title, 3*SZ_LINE, TY_CHAR)
	call salloc (rfft, fft_pow2(RV_NPTS(rv)), TY_REAL)

	# Clear the screen.
	call gclear (gp)

	if (flag == SINGLE_PLOT) {
	    if (RVP_ONE_IMAGE(rv) == OBJECT_SPECTRUM) {
	        if (RV_CONTINUUM(rv) != NONE) {
	            call get_fft (rv,OCONT_DATA(rv,1), RV_NPTS(rv), Memr[rfft],
		        fnpts)
	        } else {
	            call get_fft (rv,OBJPIXY(rv,1), RV_NPTS(rv), Memr[rfft], 
		        fnpts)
	        }
	    } else {
	        if (RV_CONTINUUM(rv) != NONE) {
	            call get_fft (rv,RCONT_DATA(rv,1), RV_RNPTS(rv), Memr[rfft],
		        fnpts)
	        } else {
	            call get_fft (rv,REFPIXY(rv,1), RV_RNPTS(rv), Memr[rfft], 
		        fnpts)
	        }
	    }

            # Draw the plot to the screen
            fnpts = max (RV_FFTNPTS(rv), fnpts) / 2
            call gascale (gp, Memr[rfft], int(fnpts/RVP_FFT_ZOOM(rv)), 2)
            call ggwind (gp, x1, x2, y1, y2)

            call get_anplot_title (rv, title)
            if (RVP_LOG_SCALE(rv) == YES)
                call strcpy ("log(|G(k)|)", Memc[ylbl], SZ_FNAME)
            else
                call strcpy ("|G(k)|", Memc[ylbl], SZ_FNAME)

            # Draw the bottom axis labels
            startp = 1.0
            endp = real(fnpts) / RVP_FFT_ZOOM(rv)
            call gseti (gp, G_WCS, 2)
            call gsview (gp, 0.115, 0.95, 0.125, 0.8)
            call gswind (gp, startp, endp, y1, y2)
            call gseti (gp, G_XDRAWAXES, 1)
            call gseti (gp, G_YDRAWAXES, 3)
            call glabax (gp, "", "Wavenumber", Memc[ylbl])
 
            # Draw the top axis labels
            startp = 0.0
            endp = (real(fnpts) / RVP_FFT_ZOOM(rv)) / (2. * real(fnpts))
            call gseti (gp, G_WCS, 1)
            call gsview (gp, 0.115, 0.95, 0.125, 0.8)
            call gswind (gp, startp, endp, y1, y2)
            call gseti (gp, G_XDRAWAXES, 2)
            call glabax (gp, "", "", Memc[ylbl])

            # Do the plot title
            call gseti (gp, G_WCS, 3)
            call gsview (gp, 0.115, 0.95, 0.125, 0.845)   
            call gswind (gp, startp, endp, y1, y2)
            call gseti (gp, G_XDRAWAXES, 0)
            call gseti (gp, G_YDRAWAXES, 0)
            call glabax (gp, Memc[title], "", "")
	    call gsview (gp, 0.115, 0.95, 0.125, 0.8)
 
	    call gvline (gp, Memr[rfft], int(fnpts/RVP_FFT_ZOOM(rv)), 
		startp, endp)

	    # Lastly, annotate the plot so we know what we're looking at.
	    call gctran (gp, 0.73, 0.73, x1, y1, 0, 1)
	    call gseti (gp, G_TXCOLOR, RV_TXTCOLOR(rv))
	    if (RVP_ONE_IMAGE(rv) == OBJECT_SPECTRUM)
	        call gtext (gp, x1, y1, "Object PS", "")
	    else
	        call gtext (gp, x1, y1, "Template PS", "")
	    call gctran (gp, 0.73, 0.69, x1, y1, 0, 1)
            if (RV_FILTER(rv) == BOTH || RV_FILTER(rv) == OBJ_ONLY) {
	        call fft_fltoverlay (rv, gp, fnpts*2, y2)
                if (RVP_WHEN(rv) == BEFORE)
                    call gtext (gp, x1, y1, "Before Filter", "")
                else 
                    call gtext (gp, x1, y1, "After Filter", "")
            }
	    call gseti (gp, G_TXCOLOR, C_FOREGROUND)
	    call gseti (gp, G_XDRAWAXES, 3)		# reset gio flags

	} else if (flag == SPLIT_PLOT) {

	    # Plot the Object power stectrum along the top.
	    if (RV_CONTINUUM(rv) == OBJ_ONLY || RV_CONTINUUM(rv) == BOTH) {
	        call split_plot (rv, gp, TOP, OCONT_DATA(rv,1), RV_NPTS(rv), 
	            OBJECT_SPECTRUM, PS_PLOT)
	    } else {
	        call split_plot (rv, gp, TOP, OBJPIXY(rv,1), RV_NPTS(rv), 
	            OBJECT_SPECTRUM, PS_PLOT)
	    }

	    # Template power spectrum along the bottom.
	    if (RV_CONTINUUM(rv) == TEMP_ONLY || RV_CONTINUUM(rv) == BOTH) {
	        call split_plot (rv, gp, BOTTOM, RCONT_DATA(rv,1), 
	            RV_RNPTS(rv), REFER_SPECTRUM, PS_PLOT)
	    } else {
	        call split_plot (rv, gp, BOTTOM, REFPIXY(rv,1), 
	            RV_RNPTS(rv), REFER_SPECTRUM, PS_PLOT)
	    }
		
	}

	call sfree (sp)
end
