include <gset.h>
include <pkg/gtools.h>
include <fset.h>
include "rvpackage.h"
include "rvflags.h"
include "rvcont.h"
include "rvplots.h"
include "rvfilter.h"

# RV_CURSOR - Get the next command from the user in the input cursor loop
# and perform the requested function.

procedure rv_cursor (rv, infile, rinfile)

pointer	rv				#I RV struct pointer
pointer	infile				#I Object input list pointer
pointer	rinfile				#I Template input list pointer

pointer	sp, cmd, buf, fname
pointer	gp, gt
int	wcs, key, nans, i
int	stat, npts, tmp_int
real	x, y, x1, x2, dx, max
char	ans[2*SZ_LINE,4]

pointer open(), gopen()
int 	rv_colon(), rv_data_check(), scan()
int	clgcur(), fstati(), fft_cursor(), spc_cursor()
int	next_spec(), next_temp(), next_ap()
int	prev_spec(), prev_temp(), prev_ap()
real	rv_maxpix()
bool	written, streq()
errchk	open, gt_init

define	LOG_PROMPT		"Log of Current Results: "
define	exit_			99
define	unknown_		98
define	redo_xcor_		97
define	replot_			96
define	refit_			95

begin
	call smark (sp)				# Allocate some space
	call salloc (cmd, SZ_LINE, TY_CHAR)
	call salloc (buf, SZ_LINE, TY_CHAR)
	call salloc (fname, SZ_FNAME, TY_CHAR)

	# Nab some pointers
	gp = RV_GP(rv)
	gt = RV_GT(rv)

	call op_debug (rv)			# Open debugger
	if (DBG_KEYSTROKE(rv) != 'x')
	    key = DBG_KEYSTROKE(rv)
	else
	    key = 'x'
	RV_NEWGRAPH(rv) = NO
	written = false
	RV_NEWXCOR(rv) = YES

	repeat {

	    RV_CMD(rv) = key
	    switch (key) { 			# Switch on the keystroke
	    case '?':				
	    	# List options
		call gpagefile (gp, XC_HELP, "FXCOR Task Options: ")

	    case ':':				
	    	# Colon command
	    	if (rv_colon(rv,Memc[cmd],written,infile,rinfile,NULL) == QUIT)
	    	    break
		if (RV_NEWXCOR(rv) == YES)
		    goto redo_xcor_
		if (RV_NEWGRAPH(rv) == YES)
		    goto replot_
		if (RV_FITDONE(rv) == NO) {
		    RV_ERRCODE(rv) = OK
		    call rv_batch_xcor (rv, RV_TEMPNUM(rv), RV_APNUM(rv), NO, 
			NO, YES)
	 	}

	    case '.':
		# Accelerator for doing continuum normalizations.
		call cmd_cont (rv)
		if (RV_NEWXCOR(rv) == YES) {
		    key = 'x'
		    goto redo_xcor_
 		} else
		    goto replot_

	    case '-':
		# Subtract a blend from the CCF.
		if (RV_FITDONE(rv) == YES) {
	            x1 = WRKPIXX(rv,1)
		    x2 = WRKPIXX(rv,RV_CCFNPTS(rv))
		    dx = 1.
		    call gctran (gp, x, y, x, y, wcs, 2)
		    call subblend (rv, gp, WRKPIXY(rv,1), x1, x2, dx, x, y)
		    goto replot_
		} else {
		    call rv_errmsg (
			"No deblended fit done yet.  Use 'd' to fit.")
		}

	    case '+':
		# Toggle status line output for ccf mode.
		RV_STATLINE(rv) = RV_STATLINE(rv) + 1
		call rv_writeln (rv, STDOUT)

	    case ',':
		# Generic Test Command (hidden from users).

	    case 'a':
		# Do a summary plot of the antisymmetric noise of the CCF.
		if (IS_DBLSTAR(rv) == YES) {
		    call rv_errmsg (
			"Antisymmetric plot invalid for a deblended fit.")
		} else if (RV_FITFUNC(rv) == CENTER1D) {
	    	    call rv_errmsg (
			"Antisymmetric plot unavailable for center1d fit.")
		} else if (RV_FITFUNC(rv) == SINC &&
		    IS_INDEF(RV_BACKGROUND(rv))) {
	    	       call rv_errmsg (
		       "Must set background for sinc fit antisym. computation.")
		} else {
		    call rv_anplot (rv, RV_GP(rv))
		    call rv_pause ("Hit any key to return to the correlation.")
		    goto replot_
		}

	    case 'b':
		# Set "background" for FWHM calculation.
		i = RV_WINCENTER(rv) - RV_WINDOW(rv)
		max = rv_maxpix (WRKPIXY(rv,i), 2*RV_WINDOW(rv))
		call gctran (gp, x, y, x, y, wcs, 2)
		if (y >= max) {
		    call rv_errmsg ("Cannot set background above CCF peak.")
	 	} else {
		    if (RV_FITFUNC(rv) == GAUSSIAN || 
			RV_FITFUNC(rv) == LORENTZIAN ||
			RV_FITFUNC(rv) == SINC) {
		            call reset_errcom (rv)
		            call rv_erase_fit (rv, false)
	                    RV_BACKGROUND(rv) = y
		            RV_ERRCODE(rv) = OK
			    IS_DBLSTAR(rv) = NO
		            call rv_batch_xcor (rv, RV_TEMPNUM(rv), 
				RV_APNUM(rv), NO, NO, NO)
				#RV_APNUM(rv), NO, NO, YES)
		    } else {
			call rv_errmsg ("Cannot set background for a %s fit.")
			if (IS_DBLSTAR(rv) == YES)
			    call pargstr ("deblended")
			else if (RV_FITFUNC(rv) == PARABOLA)
			    call pargstr ("parabolic")
			else
			    call pargstr ("center1d")
		    }
		}

	    case 'c':				
		# Read cursor poistion
		call rv_rdcursor (rv, gp, x, y, wcs)

	    case 'd':				
		# Fit the peak with up to four Gaussians.
		call reset_errcom (rv)
	        x1 = WRKPIXX(rv,1)
		x2 = WRKPIXX(rv,RV_CCFNPTS(rv))
		dx = 1.
		call gctran (gp, x, y, x, y, wcs, 2)
		call deblend (rv, gp, x1, x2, dx, x, y, WRKPIXY(rv,1), ans, 
		    nans)

	    case 'e':
		# Show the summary plot after the fit.
	        call rv_eplot (rv, gp)
		call rv_pause ("Hit any key to return to the correlation.")
		goto replot_

	    case 'f':
		# Enter the Fourier Mode command stream.
		stat = fft_cursor(rv)
		if (RV_NEWXCOR(rv) == YES) {
		    key = 'x'
		    goto redo_xcor_
		} else
		    goto replot_

	    case 'g':
	    	# Fit the correlation plot based on the X cursor values
		call reset_errcom (rv)
		call gctran (gp, x, y, x, y, wcs, 2)
		call rv_xfit (rv, x, YES)

	    case 'I':
		# Fatal interrupt.
	        call fatal (0, "Interrupt")

	    case 'j':
		# Residual plot of fit.
		call rv_plot (rv, RESIDUAL_PLOT)

	    case 'l':
	    	# Page the logfile. Pointer is closed to be VMS compatable.
	    	if (RV_TXFD(rv) != NULL) {
		    if (fstati(RV_TXFD(rv),F_FILESIZE) == 0) 
			call rv_errmsg ("Nothing yet written to logfile.")
		    else {
		        call sprintf (Memc[buf], SZ_FNAME, "%s.txt")
			    call pargstr (SPOOL(rv))
		        call flush (RV_TXFD(rv))
		        call close (RV_TXFD(rv))
		        call gpagefile (gp, Memc[buf], LOG_PROMPT)
		        RV_TXFD(rv) = open (Memc[buf], APPEND, TEXT_FILE)
		    }
	    	} else
		    call rv_errmsg ("No output file specified.")

	    case 'm':				
		# Plot polymarkers of the actual CCF points.
		call gseti (gp, G_WCS, 2)
		i = RV_WINCENTER(rv) - RV_WINDOW(rv)
		npts = 2 * RV_WINDOW(rv)
		call gpmark (gp,WRKPIXX(rv,i), WRKPIXY(rv,i), npts, 4, 2.0, 2.0)
		call gflush (gp)

	    case 'n':
		# Go to the next (template --> aperture --> object) after 
		# saving results.
		if (RV_TEMPNUM(rv) < RV_NTEMPS(rv) ||
		    CURAPNUM(rv) < NUMAPS(rv) ||
		    RV_IMNUM(rv) < RV_NOBJS(rv))
		    	call rv_do_save (rv, written)

		# Now do the "next" operation as specified. The logic sequence
		# is complicated but handles all cases.
		if (RV_TEMPNUM(rv) < RV_NTEMPS(rv)) {
		    if (next_temp(rv, rinfile, written) == ERR_READ)
			call rv_errmsg ("Error reading next template.")
		    else
			goto redo_xcor_
		} else {
		    # Get the next aperture
		    if (CURAPNUM(rv) < NUMAPS(rv)) {
		        if (RV_NTEMPS(rv) > 1) {	# Reset templates
		            RV_TEMPNUM(rv) = 0
		            if (next_temp(rv, rinfile, written) == ERR_READ)
			        call rv_errmsg ("Error reading next template.")
		        }
			if (next_ap(rv, written) == ERR_READ)
			    call rv_errmsg ("Errror reading next aperture.")
		    	else
			    goto redo_xcor_
		    } else {
			# get the next object
			if (RV_IMNUM(rv) < RV_NOBJS(rv)) {
			    if (NUMAPS(rv) > 1) {
			        CURAPNUM(rv) = 0	# Reset apertures
			        if (next_ap(rv, written) == ERR_READ) {
			            call rv_errmsg (
					"Errror reading next aperture.")
				}
			    }
		            if (RV_NTEMPS(rv) > 1) {
		                RV_TEMPNUM(rv) = 0	# Reset templates
		                if (next_temp(rv, rinfile, written) ==ERR_READ){
			            call rv_errmsg (
					"Error reading next template.")
				}
			    }
			    if (next_spec(rv, infile, written) == ERR_READ)
				call rv_errmsg ("Error reading next object.")
		    	    else
				goto redo_xcor_
			} else {
			    call rv_errmsg ("No more spectra to process.")
			}
		    }
		}

	    case 'o':
		# Fit or refit the object spectrum continuum for subtraction.
		tmp_int = CON_INTERACTIVE(rv)
		CON_INTERACTIVE(rv) = YES
		call do_continuum (rv, OBJECT_SPECTRUM)
		CON_INTERACTIVE(rv) = tmp_int
		RV_FITDONE(rv) = NO
		RV_NEWXCOR(rv) = YES
		IS_DBLSTAR(rv) = NO
		goto redo_xcor_

	    case 'p':
		# Go to the previous (template --> aperture --> object) after 
		# saving results.
		if (RV_TEMPNUM(rv) > 1 || CURAPNUM(rv) > 1 || RV_IMNUM(rv) > 1)
		    call rv_do_save (rv, written)

		# Now do the "previous" operation as specified. The logic 
		# sequence is complicated but handles all cases.
		if (RV_TEMPNUM(rv) > 1 && RV_NTEMPS(rv) > 1) {
		    if (prev_temp(rv, rinfile, written) == ERR_READ)
			call rv_errmsg ("Error reading previous template.")
		    else
			goto redo_xcor_
		} else {
		    # Do previous aperture
		    if (CURAPNUM(rv) > 1 && NUMAPS(rv) > 1) {
		        if (RV_NTEMPS(rv) > 1 && RV_TEMPNUM(rv) > 1) {
		            RV_TEMPNUM(rv) = RV_NTEMPS(rv) + 1 # Reset templates
		            if (prev_temp(rv, rinfile, written) == ERR_READ) {
			        call rv_errmsg (
				    "Error reading previous template.")
			    }
		        }
			if (prev_ap(rv, written) == ERR_READ)
			    call rv_errmsg ("Errror reading previous aperture.")
			else
		            goto redo_xcor_
		    } else {
			# Do previous object image
			if (RV_NOBJS(rv) > 1 && RV_IMNUM(rv) > 1) {
			    if (NUMAPS(rv) > 1) {
			        CURAPNUM(rv) = NUMAPS(rv) + 1
			        if (prev_ap(rv, written) == ERR_READ) {
			            call rv_errmsg (
				        "Errror reading previous aperture.")
			        }
			    }
			    if (RV_NTEMPS(rv) > 1) {
			        RV_TEMPNUM(rv) = RV_NTEMPS(rv) + 1
		                if (prev_temp(rv, rinfile, written) ==ERR_READ){
			            call rv_errmsg (
				        "Error reading previous template.")
			        }
			    }
			    if (prev_spec(rv, infile, written) == ERR_READ)
				call rv_errmsg("Error reading previous object.")
			    else
		                goto redo_xcor_
			} else {
			    call rv_errmsg ("At the start of the input list.")
			}
		    }
		}
		
	    case 'q': 				
	    	# Quit, possibly saving results before hand.
		if (!written || RV_UPDATE(rv) == YES) {
		    if (RV_AUTOWRITE(rv) == YES && !streq(SPOOL(rv),""))
		        call cmd_write (rv, written)
		    else
		        call rv_query_save (rv, written, QUIT)
		}
		break

	    case 'r':				
	    	# Replot the data.
replot_	    	call rv_plot (rv, CORRELATION_PLOT)
	 	if (IS_DBLSTAR(rv) == NO)
		    call rv_writeln (rv, STDOUT)

	    case 's':				
	    	# Examine object and template spectra.
		stat = spc_cursor (rv)
		if (RV_NEWXCOR(rv) == YES) {
		    key = 'x'
		    goto redo_xcor_
 		} else
		    goto replot_

	    case 't': 
		# Fit or refit the template spectrum continuum for subtraction.
		tmp_int = CON_INTERACTIVE(rv)
		CON_INTERACTIVE(rv) = YES
		call do_continuum (rv, REFER_SPECTRUM)
		CON_INTERACTIVE(rv) = tmp_int
		RV_FITDONE(rv) = NO
		RV_NEWXCOR(rv) = YES
		IS_DBLSTAR(rv) = NO
		goto redo_xcor_

	    case 'v':
		# Suspend graphics and show verbose fit/xcor output.
		call rv_verbose_fit (rv, STDOUT)

	    case 'w':
		# Write current correlation results to log file.
		if (!written || RV_UPDATE(rv) == YES) {
	            if (RV_TXFD(rv) == NULL) {
		        call strcpy ("\0", Memc[fname], SZ_FNAME)
		        while (Memc[fname] == '\0') {
	    	            call printf ("Root output filename: ")
	    	            call flush (STDOUT)
	    	            stat = scan()
		                call gargstr (Memc[fname], SZ_FNAME)
		        }
		        call init_files (rv, DEVICE(rv), Memc[fname], true)
        	        RV_MGP(rv) = gopen ("stdvdm", APPEND, RV_GRFD(rv))
			written = false
	            }
		    call cmd_write (rv, written)
		    written = true
		} else
		    call rv_query_save (rv, written, NULL)

	    case 'x':				
	    	# Re-do the correlation and check params.
		RV_NEWXCOR(rv) = YES
redo_xcor_	RV_ERRCODE(rv) = OK
		if (rv_data_check(rv) == OK && RV_NEWXCOR(rv) == YES) {
		    call rv_batch_xcor (rv, RV_TEMPNUM(rv), RV_APNUM(rv), YES, 
			YES, YES)
		} else 
		    goto replot_

	    case 'y':
		# Same as 'g' but get endpoints from Y value.
		i = RV_WINCENTER(rv) - RV_WINDOW(rv)
		max = rv_maxpix (WRKPIXY(rv,i), 2*RV_WINDOW(rv))
		call gctran (gp, x, y, x, y, wcs, 2)
		if (y >= max) {
		    call rv_errmsg ("Cannot set cursor above CCF peak.")
	 	} else {
		    call reset_errcom (rv)
		    RV_FITHGHT(rv) = y
		    call rv_yfit (rv, y, YES)
	 	}

	    case 'z':
		# Zoom in on a new peak in the entire CCF.
		RV_ERRCODE(rv) = OK
		call rv_zplot (rv, gp, x, y, wcs)

	    case '\n', '\r', ' ':
		# No-op.

	    default:
	    	# Unknown command
unknown_	call rv_errmsg ("Type '?' for a list of commands.")
	    }

	    RV_NEWXCOR(rv) = NO
	    RV_NEWGRAPH(rv) = NO

	} until (clgcur("cursor",x,y,wcs,key,Memc[cmd],SZ_LINE) == EOF)

exit_	call sfree (sp)
end


# RV_DO_SAVE - Check that a write is called for, and prompt for a filename
# if necessary.

procedure rv_do_save (rv, written)

pointer	rv				#I RV struct pointer
bool	written				#U Data write flag

begin
	if (RV_AUTOWRITE(rv) == YES && (!written || RV_UPDATE(rv) == YES))
  	    call cmd_write (rv, written)
	else if (!written || RV_UPDATE(rv) == YES)
	    call rv_query_save (rv, written, MOVE)
end


# RV_MODE_PROMPT - For a given sub-mode, prompt the user appropriately, giving
# also the parent mode.

procedure rv_mode_prompt (rv)

pointer	rv				#I RV struct pointer

begin
	# Switch on the legal modes (i.e. make it simple)
	switch (RV_MODES(rv)) {
	case 1:
	    # No-op
	case 12:
	    call printf ("ccf.fft mode: ") 	# CCF -> FFT
	case 13:
	    call printf ("ccf.spec mode: ") 	# CCF -> SPEC
	case 123:
	    call printf ("ccf.fft.spec mode: ") # CCF -> FFT -> SPEC
	case 132:
	    call printf ("ccf.spec.fft mode: ") # CCF -> SPEC -> FFT
	default:
	}
	call flush (STDOUT)
end


# RV_PARENT - Utility to return the parent mode of the current mode.

int procedure rv_parent (rv)

pointer	rv				#I RV struct pointer

begin
	# Switch on the legal modes (i.e. make it simple)
	switch (RV_MODES(rv)) {
	case 123:
	    return (FFT_MODE)
	case 132:
	    return (SPEC_MODE)
	default:
	    return (INDEFI)
	}
end


# RV_QUERY_SAVE - Query the user to save the results, and possibly a file
# name.

procedure rv_query_save (rv, written, type)

pointer	rv				#I RV struct pointer
bool	written				#I Results written flag
int	type				#I Type of prompt

pointer	sp, resp, gopen()
int	stat, scan()
bool	answer, streq()

data	answer	/true/

begin
	call smark (sp)
	call salloc (resp, SZ_LINE, TY_CHAR)

	# Prompt the user.
	if (type == QUIT) {
	    call printf ("Save results before quitting? (%b) ")
		call pargb (answer)
	} else if (type == MOVE) {
	    call printf ("Save results before moving? (%b) ")
		call pargb (answer)
	} else {
	    call printf ("Results already written.  Write again? (%b) ")
		call pargb (answer)
	}
	call flush (STDOUT)

	stat = scan()
	    call gargstr (Memc[resp], SZ_FNAME)
	if (Memc[resp] == 'n' || Memc[resp] == 'N')
	    answer = false
	else if (Memc[resp] == 'y' || Memc[resp] == 'Y')
	    answer = true

	if (answer) {
	    if (RV_TXFD(rv) == NULL) {
		call strcpy ("\0", Memc[resp], SZ_FNAME)
		while (Memc[resp] == '\0' && !streq(Memc[resp],"\"\"")) {
	    	    call printf ("Root output filename: ")
	    	    call flush (STDOUT)
	    	    stat = scan()
		        call gargstr (Memc[resp], SZ_FNAME)
		}
		if (!streq(Memc[resp],"\"\"")) {
		    call init_files (rv, DEVICE(rv), Memc[resp], true)
        	    RV_MGP(rv) = gopen ("stdvdm", APPEND, RV_GRFD(rv))
		}
	    }
	    if (!streq(Memc[resp],"\"\"")) {
	        written = false
	        call cmd_write (rv, written)
	        answer = true
	    } else {
		call printf ("Results not saved.\n")
	        call sfree (sp)
		return
	    }

	} else {
	    answer = false
	    call sfree (sp)
	    return
	}

	call sfree (sp)
end


# RV_RDCURSOR - Read and print out the current cursor position.  This routine
# figures out where the user is pointing and print out in the correct WCS.

procedure rv_rdcursor (rv, gp, x, y, wcs)

pointer	rv				#I RV struct pointer
pointer	gp				#I Graphics pointer
real	x				#I X cursor position
real	y				#I Y cursor position
int	wcs				#I WCS of cursor position

real	x1, y1, vel
double	rv_shift2vel()

begin
	# Check for boundary coordinates
	call gctran (gp, x, y, x1, y1, wcs, 0)
	if (y1 < 0.725)					# in bottom ccf plot
	    call gctran (gp, x, y, x1, y1, wcs, 2)
	else
	    call gctran (gp, x, y, x1, y1, wcs, 3)

	if (RV_DCFLAG(rv) == -1) {
	    call printf ("Cursor:  lag = %.3f  y = %.3f\n")
	        call pargr (x1)
	        call pargr (y1)
	} else {
	    vel = real (rv_shift2vel(rv,x1))
	    call printf ("Cursor:  lag = %.3f  velocity = %.3f  y = %.3f\n")
	        call pargr (x1)
	        call pargr (vel)
	        call pargr (y1)
	}
end
