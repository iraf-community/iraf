include "rvpackage.h"
include "rvflags.h"
include "rvcont.h"

# RV_BATCH - Process the input list in batch mode with fixed parameters.

procedure rv_batch (rv, infile, rinfile)

pointer	rv				#I RV struct pointer
pointer	infile				#I Object input file list pointer
pointer	rinfile				#I Template input list pointer

pointer	sp, fd, rim
int	ntemps, naps, tcount, apcount, stat
bool	init_hdr, written

int	imtrgetim(), next_spec(), next_ap(), get_spec()
int	rv_data_check()

define	write_		99

begin
	# Open debugging log
	call op_debug (rv)			

	call smark (sp)
	call salloc (rim, SZ_FNAME, TY_CHAR)

	RV_RECORD(rv) = 0
	RV_TEMPNUM(rv) = 0
	ntemps = RV_NTEMPS(rv)
	naps = NUMAPS(rv)

	RV_APNUM(rv) = 1
	RV_IMNUM(rv) = 1
	init_hdr = true
	repeat {				# For each of the object spectra

	    # For each aperture in the list
	    do apcount = 1, naps {

	        if ((RV_CONTINUUM(rv) == OBJ_ONLY || 
		    RV_CONTINUUM(rv) == BOTH) && OBJCONT(rv) == NO) {
		        call do_continuum (rv, OBJECT_SPECTRUM)
		}
		RV_APNUM(rv) = APLIST(rv,apcount)

	        # For each template spectrum
	        do tcount = 1, ntemps {	

		    # Check the data before continuing
		    RV_TEMPNUM(rv) = tcount
		    if (rv_data_check(rv) != OK)
			break

		    # Reset some parameters
		    call reset_errcom (rv)

		    #REFCONT(rv) = NO
	            if ((RV_CONTINUUM(rv) == TEMP_ONLY || 
			RV_CONTINUUM(rv) == BOTH) && REFCONT(rv) == NO)
		            call do_continuum (rv, REFER_SPECTRUM)

		    # Jump right into it and get the correlation
		    call rv_batch_xcor (rv, tcount, apcount, YES, YES, YES)

		    # Initialize the output header file.
		    if (init_hdr) {
		        fd = RV_TXFD(rv)			
		        if (fd != NULL) {
            	    	    call rv_param (rv, fd, "fxcor")
            	    	    call rv_tempcodes (rv, fd)
	    	    	    call rv_prdeltav (rv, fd)
	    	    	    call fprintf (fd, "# \n")
	    	    	    call rv_hdr (rv, fd)
		        }
			init_hdr = false
		    }

write_	            call rv_imtitle (RIMAGE(rv), TEMPNAME(rv), SZ_FNAME)
		    if (RV_VERBOSE(rv) == OF_SHORT || 
			RV_VERBOSE(rv) == OF_STXTONLY) {
		            call rv_write_short (rv, fd)
		    } else {
			call rv_verbose_fit (rv, RV_VBFD(rv))
		 	call rv_write_long (rv, fd)
	 	    }
        	    if (RV_IMUPDATE(rv) == YES) 	# update image header
            	        call rv_imupdate (rv)
	            call rv_eplot (rv, RV_MGP(rv))
		    written = TRUE
	            RV_RECORD(rv) = RV_RECORD(rv) + 1

		    # Get the next template image to use
		    if (tcount+1 <= ntemps) {
	              if (imtrgetim(rinfile,tcount+1,Memc[rim],SZ_FNAME)!=EOF) {
		            call strcpy (Memc[rim], RIMAGE(rv), SZ_FNAME)
		            call rv_imtitle (Memc[rim],TEMPNAME(rv),SZ_FNAME)
			    RV_TEMPNUM(rv) = tcount + 1
			    if (get_spec(rv,Memc[rim],REFER_SPECTRUM)==ERR_READ)
				call error (0, "Error reading next template.")
			    RV_TEMPCODE(rv) = 'A' + tcount
		      }
		    }
    
	        }			# End of template loop

		# Update the image apertures
		if (apcount < naps)
		    stat = next_ap (rv, written)
	    
		# Here we need to reset the template stuff
		if (ntemps == 1 || naps == 1)
		    next
	    	if (imtrgetim(rinfile, 1, Memc[rim], SZ_FNAME) != EOF) {
		    RV_TEMPNUM(rv) = 1
		    call strcpy (Memc[rim], RIMAGE(rv), SZ_FNAME)
		    call rv_imtitle (Memc[rim], TEMPNAME(rv), SZ_FNAME)
		    if (get_spec(rv,Memc[rim],REFER_SPECTRUM)==ERR_READ)
		    	call error (0, "Error reading next template.")
		    RV_TEMPCODE(rv) = 'A'
	        } else {
		    call rv_errmsg("Error getting template name from list.")
		    break
	        }
	    }				# End of aperture loop

	    # Now we need to reset the apertures for the next images
	    CURAPNUM(rv) = 1
	    RV_APNUM(rv) = APLIST(rv,1)

	    # Check to see if we can get another image, otherwise quit
	    if (RV_IMNUM(rv)+1 <= RV_NOBJS(rv)) {
		# Try to read the image if no error
		if (next_spec (rv, infile, written) != OK) {
		    call rv_errmsg ("Error reading next object image.")
		    break
		}
	    } else
		break					# That's it folks!

	    # We got another object so we need to reset the template stuff
	    if (imtrgetim(rinfile, 1, Memc[rim], SZ_FNAME) != EOF) {
		RV_TEMPNUM(rv) = 1
		call strcpy (Memc[rim], RIMAGE(rv), SZ_FNAME)
		call rv_imtitle (Memc[rim], TEMPNAME(rv), SZ_FNAME)
		if (get_spec(rv,Memc[rim],REFER_SPECTRUM)==ERR_READ)
		    call error (0, "Error reading next template.")
		RV_TEMPCODE(rv) = 'A'
	    } else {
		call rv_errmsg ("Error getting template name from list.")
		break
	    }
	} 						# End of object loop

	call sfree (sp)
end


# RV_BATCH_XCOR - Process the input list in batch mode with fixed parameters

procedure rv_batch_xcor (rv, tcount, apcount, do_comp, do_plot, comp_win)

pointer	rv				#I RV struct pointer
int	tcount				#I Template number in list
int	apcount				#I Aperture number in list
int	do_comp				#I Do xcor computation?
int	do_plot				#I Draw the ccf plot?
int	comp_win			#I Compute a new window center?

real	shift, sigma, max
int	ishift, npts, istart, iend, stat
real	rv_maxpix()
int	rv_getshift(), rv_rvcorrect()

begin
	# Set some things up
	call reset_errcom (rv)
	RV_FITDONE(rv) = NO

	# Let 'em know what we're up to.
	if (RV_INTERACTIVE(rv) == YES && do_comp == YES) {
	    call printf ("Cross-Correlating %s[%d] with %s[%d].\n")
	        call pargstr(IMAGE(rv))
	        call pargi(RV_OAPNUM(rv))
	        call pargstr(RIMAGE(rv))
	        call pargi(RV_RAPNUM(rv))
	    call flush (STDOUT)
	}

	# Now do the debug output
	if (DBG_DEBUG(rv) == YES) {
	    call d_printf (DBG_FD(rv), "rvxbatch: imnum=%d ap=%d temp=%d ")
	        call pargi(RV_IMNUM(rv));call pargi(apcount);call pargi(tcount)
	    call d_printf (DBG_FD(rv), " object=:%s: temp=:%s:\n")
	        call pargstr(IMAGE(rv));call pargstr(RIMAGE(rv))
	}

	# Jump right into it and get the correlation
	if (do_comp == YES)
	    call rv_fftcorr (rv, NO)

	# Compute window center and size
	call rv_gwindow (rv, comp_win, istart, npts)

	# If interactive, draw the ccf plot here now that everything's computed
	if (RV_INTERACTIVE(rv) == YES && do_plot == YES)
	    call rv_plot (rv, CORRELATION_PLOT)

        # Get the endpoints to fit
        ishift = rv_getshift (WRKPIXY(rv,istart), npts, MAXIMUM) + istart - 1
	max = rv_maxpix (WRKPIXY(rv,istart), npts)
	if (!IS_INDEF(RV_FITWIDTH(rv))) {
	    if (RV_FITWIDTH(rv) < RV_MINWIDTH(rv))
	        RV_FITWIDTH(rv) = int (RV_MINWIDTH(rv))
	    else if (RV_FITWIDTH(rv) > RV_MAXWIDTH(rv))
	        RV_FITWIDTH(rv) = int (RV_MAXWIDTH(rv))

	    istart = ishift - int (RV_FITWIDTH(rv) / 2)
	    if (((RV_FITWIDTH(rv)/2.)-int(RV_FITWIDTH(rv)/2.)) > 0.0)
	        iend = ishift + int (RV_FITWIDTH(rv) / 2)
	    else
	        iend = ishift + int (RV_FITWIDTH(rv) / 2 - 1)
	    npts = int (iend - istart + 1)

            # Call the fitting routine and get center of fit and sigma
	    call rv_fit (rv, WRKPIXX(rv,1), WRKPIXY(rv,1), istart, iend, 
	        npts, ishift, shift, sigma)
	    if (RV_ERRCODE(rv) == ERR_FIT) {
		if (RV_INTERACTIVE(rv) == YES)
	            call rv_errmsg ("Fit did not converge.")
		else
	            call rv_err_comment (rv, "Fit did not converge.", "")
	        return
	    }
	    RV_SHIFT(rv) = shift
	    RV_SIGMA(rv) = sigma

	    # do velocity corrections
    	    stat = rv_rvcorrect (rv, shift, sigma, RV_VOBS(rv), RV_VCOR(rv),
	        RV_ERROR(rv))

	    if (RV_INTERACTIVE(rv) == YES)
                call rv_writeln (rv, STDOUT)

        } else if (IS_INDEF(RV_FITWIDTH(rv))) {
	    if (RV_PEAK(rv) == NO) {
		if (max < RV_FITHGHT(rv) && RV_INTERACTIVE(rv) == YES) {
		    call printf ("No points fit - height set too high.\n")
		    return
		}
		call rv_yfit (rv, RV_FITHGHT(rv), YES)
	    } else
		call rv_yfit (rv, (RV_FITHGHT(rv)*WRKPIXY(rv,ishift)), YES)
	}
end


# RV_GWINDOW - Get the window size and center sizes from the input parameters
# or current setting.  Does some bounds checking to limit the window to the
# actual ccf plot.  Explanation of the window parameters:
#
#	RV_WINPAR	- Input "window" parameter
#	RV_WINCENPAR	- Input "wincenter" parameter
#	RV_WINDOW	- Size of window (in lags)
#	RV_WINCENTER	- Array index into CCF
#	RV_WINL		- Left edge of window in +/- lags
#	RV_WINR		- Right edge of window in +/- lags

procedure rv_gwindow (rv, comp_win, istart, npts)

pointer	rv					#I RV struct pointer
int	comp_win				#I Compute a new window center?
int	istart					#O Start of window
int	npts					#O Npts in window

int	rv_getshift()
real	rv_vel2shift

begin
	# Get the window size parameters
	if (IS_INDEF(RV_WINPAR(rv))) {
	    RV_WINDOW(rv) = 20
	} else {
	    if (RV_DCFLAG(rv) == -1)
	        RV_WINDOW(rv) = RV_WINPAR(rv)
	    else
	        RV_WINDOW(rv) = max (2, nint (rv_vel2shift(rv,RV_WINPAR(rv))))
	}
        npts = 2 * RV_WINDOW(rv)

	# Now compute the window center
	if (comp_win == YES) {
	    if (IS_INDEF(RV_WINCENPAR(rv))) {
	        RV_WINCENTER(rv) = rv_getshift (WRKPIXY(rv,1), RV_CCFNPTS(rv), 
		    MAXIMUM)
	        istart =  RV_WINCENTER(rv) + 1 - RV_WINDOW(rv)
	    } else {
	        if (RV_DCFLAG(rv) == -1) {
	            RV_WINCENTER(rv) = int (RV_CCFNPTS(rv)/2 + 
			RV_WINCENPAR(rv)) + 1
	        } else {
	            RV_WINCENTER(rv) = int (RV_CCFNPTS(rv)/2 + 
	        	nint (rv_vel2shift(rv,RV_WINCENPAR(rv)))) + 1
		    if (!IS_INDEF(TEMPVEL(rv,RV_TEMPNUM(rv)))) {
		        RV_WINCENTER(rv) = RV_WINCENTER(rv) -
			    rv_vel2shift(rv,TEMPVEL(rv,RV_TEMPNUM(rv)))
		    }
	        }
	        istart = RV_WINCENTER(rv) + 1 - RV_WINDOW(rv)
	    }
	} else
	    istart = RV_WINCENTER(rv) + 1 - RV_WINDOW(rv)

	# Boundary checks
	if (RV_WINDOW(rv) > RV_CCFNPTS(rv)/2) {
	    RV_WINDOW(rv) = 20
	    call rv_err_comment (rv, 
		"Warning: Window too large - reset to 20 lags.", "")
	}
	if (RV_WINCENTER(rv) > RV_CCFNPTS(rv)) {
            RV_WINCENTER(rv) = rv_getshift (WRKPIXY(rv,1), RV_CCFNPTS(rv),
                    MAXIMUM)
	    call rv_err_comment (rv, 
		"Warning: Wincenter too large - reset to max peak.", "")
	    istart =  RV_WINCENTER(rv) + 1 - RV_WINDOW(rv)
	}
	if ((RV_WINCENTER(rv)-RV_WINDOW(rv)) < 1) {
	    RV_WINL(rv) = WRKPIXX(rv,1)
	    RV_WINR(rv) = RV_WINL(rv) + RV_WINDOW(rv)
	    istart = 1
	} else if ((RV_WINCENTER(rv)+RV_WINDOW(rv)) > RV_CCFNPTS(rv)) {
	    RV_WINL(rv) = WRKPIXX(rv,RV_CCFNPTS(rv) - RV_WINDOW(rv))
	    RV_WINR(rv) = WRKPIXX(rv,RV_CCFNPTS(rv))
	    istart = RV_CCFNPTS(rv) - RV_WINDOW(rv)
	} else {
	    RV_WINL(rv) = WRKPIXX(rv,RV_WINCENTER(rv) - RV_WINDOW(rv))
	    RV_WINR(rv) = WRKPIXX(rv,RV_WINCENTER(rv) + RV_WINDOW(rv))
	}
	npts = RV_WINR(rv) - RV_WINL(rv) + 1

	if (DBG_DEBUG(rv) == YES) {
	    call d_printf(DBG_FD(rv), "rv_gwindow:\n")
	    call d_printf (DBG_FD(rv), "\twcent,window=%f,%f wl,wr=%f,%f\n")
		call pargi(RV_WINCENTER(rv));	call pargi(RV_WINDOW(rv))
		call pargi(RV_WINL(rv));	call pargi(RV_WINR(rv))
	}
end
