include <gset.h>
include "rvpackage.h"
include "rvcomdef.h"
include "rvflags.h"
include "rvsample.h"

# SPEC_COLON - Procedure to process the colon commands defined below.  Most
# commands are for interactive editing of parameters to the task.

int procedure spc_colon (rv, cmdstr)

pointer	rv				#I RV struct pointer
char	cmdstr[SZ_LINE]			#I Command

pointer sp, cmd
int 	cmd_regions(), strdic()

begin
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)

	call sscan (cmdstr)
	call gargwrd (Memc[cmd], SZ_LINE)

	# Unpack the keyword from the string and look it up in the
	# dictionary.  Switch on command and call the appropriate routines.

        if (strdic(Memc[cmd], Memc[cmd], SZ_LINE, CONT_KEYWORDS) != 0) {
            # Process the CONTPARS pset commands.
            call cont_colon (rv, cmdstr)
 
	} else {
	    # Now process the mode specific colon commands.
	    switch (strdic(Memc[cmd], Memc[cmd], SZ_FNAME, RVX_KEYWORDS)) {
	    case RVX_DISP:
	        # Print the rebinned wpc
	        call cmd_prtdisp (rv)

	    case RVX_OSAMPLE:
	        # Set/Show the object sample region for correlation
	        if (cmd_regions(rv, RV_OSAMPLE(rv)) == ERR_CORREL) {
		    call sfree (sp)
		    return (ERR_CORREL)
		}

	    case RVX_RSAMPLE:
	        # Set/Show the template sample region for correlation
	        if (cmd_regions(rv,RV_RSAMPLE(rv)) == ERR_CORREL) {
		    call sfree (sp)
		    return (ERR_CORREL)
		}

	    case RVX_SHOW:
                # List the current values of all parameters.
                call rv_show (rv, STDOUT)

	    case RVX_VERSION:
	        # Show the task version
	        call cmd_version ()

	    default:
	        # Default action.
	        call rv_errmsg (" Type '?' for a list of commands.")
	    }
	}

	call sfree (sp)
	return (OK)
end


# SPEC_CURSOR - Get the next command from the user in the input cursor loop
# and perform the requested function.

int procedure spc_cursor (rv)

pointer	rv				#I RV struct pointer

pointer	gp, sp
pointer	cmd, buf
int	wcs, key, stat
char	ckey
real	x, y, x1, x2, stdlam
bool	prompt

int 	spc_colon(), clgcur(), scan()
int	fft_cursor(), rv_parent()#, stridx()

define	replot_			99
define	exit_			98

begin
	# Update mode counter.
	RV_MODES(rv) = (RV_MODES(rv) * 10) + SPEC_MODE

	# Allocate some space.
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)
	call salloc (buf, SZ_LINE, TY_CHAR)

	# Nab some pointers and initialize
	key = RV_SPMKEY(rv)
	gp = RV_GP(rv)

	RV_NEWXCOR(rv) = NO
	RV_NEWGRAPH(rv) = NO
	repeat {

	    prompt = true
replot_	    switch (key) { 			# switch on the keystroke
	    case '?':				
	    	# List options.
		call gpagefile (gp, SM_HELP, "Spectrum Mode Options:")

	    case ':':
		# Process a colon command.
	    	if (spc_colon(rv,Memc[cmd]) == QUIT)
	    	    break
		prompt = false

            case 'b':
                # Mark the sample regions for both spectra.
                call gctran (gp, x, y, x1, x2, wcs, 1)
                call rv_cut (rv, x1, x1, x2)
                call gctran (gp, x, y, x, y, wcs, 0)

                call append_range (rv, RV_OSAMPLE(rv), x1, x2)
                SR_MODIFY(RV_OSAMPLE(rv)) = YES
                call append_range (rv, RV_RSAMPLE(rv), x1, x2)
                SR_MODIFY(RV_RSAMPLE(rv)) = YES

                RV_NEWXCOR(rv) = YES

	    case 'd':
		# Print relative velocity between two different positions.
	 	call gctran (gp, x, y, x, y, wcs, 1)
		call rv_prshift (rv, x)
		prompt = false

            case 'e':
                # Show the summary plot after the fit.
                call rv_eplot (rv, gp)
                call rv_pause ("Hit any key to continue....")
	        key = RV_SPMKEY(rv)
                goto replot_

	    case 'f':
		# FFT Mode.
		if (rv_parent(rv) == FFT_MODE) {
		    goto exit_
		} else if (fft_cursor(rv) == QUIT) {
		    RV_MODES(rv) = (RV_MODES(rv) - SPEC_MODE) / 10
		    call sfree (sp)
		    return (QUIT)
		} else {
	 	    key = RV_SPMKEY(rv)
		    goto replot_
		}

	    case 'i':
		# Display original input spectra.
		call rv_splot (rv, SPLIT_PLOT)

	    case 'I':
		# Interrupt command.
		call error (0, "Interrupt")

	    case 'n':
		# Display continuum subtracted spectra.
		call rv_nplot (rv, SPLIT_PLOT)

	    case 'p':
		# Display the FFT prepared spectra.
		call rv_fftcorr (rv, YES)
                call rv_pause ("Hit any key to continue....")
	        key = RV_SPMKEY(rv)
                goto replot_

	    case 'q':
		# Quit.
	  	break

	    case 'r':
		# Replot.
	        key = RV_SPMKEY(rv)
		goto replot_

	    case 's':
	        # Mark the sample regions.
	 	call gctran (gp, x, y, x1, x2, wcs, 1)
	        call rv_cut (rv, x1, x1, x2)
	 	call gctran (gp, x, y, x, y, wcs, 0)
        	if (y > 0.5) {                # Cut from the top
            	    call append_range (rv, RV_OSAMPLE(rv), x1, x2)
            	    call rv_mark_regions (RV_OSAMPLE(rv), RV_GP(rv))
            	    SR_MODIFY(RV_OSAMPLE(rv)) = YES
        	} else {
            	    call append_range (rv, RV_RSAMPLE(rv), x1, x2)
            	    call rv_mark_regions (RV_RSAMPLE(rv), RV_GP(rv))
            	    SR_MODIFY(RV_RSAMPLE(rv)) = YES
        	}
		RV_NEWXCOR(rv) = YES

	    case 'u':
	        # Unselect a sample region.
	 	call gctran (gp, x, y, x1, x2, wcs, 1)
	 	call gctran (gp, x, y, x, y, wcs, 0)
        	if (y > 0.5) {                # Cut from the top
            	    call delete_samp (rv, RV_OSAMPLE(rv), x1)
            	    SR_MODIFY(RV_OSAMPLE(rv)) = YES
        	} else {
            	    call delete_samp (rv, RV_RSAMPLE(rv), x1)
            	    SR_MODIFY(RV_RSAMPLE(rv)) = YES
        	}
		RV_NEWXCOR(rv) = YES

	    case 'v':
	        # Fit the line and compute a velocity based on standard 
		# wavelength.
		if (RV_DCFLAG(rv) == -1) {
		    call eprintf (
		        "No dispersion available for velocity computation.")
		} else {
	 	    call gctran (gp, x, y, x1, x2, wcs, 1)
	            call rv_cut (rv, x1, x1, x2)
	 	    call gctran (gp, x, y, x, y, wcs, 0)
		    stdlam = 0.0
		    while (stdlam == 0.0) {
                        call printf ("Standard Wavelength: ")
                        call flush (STDOUT)
                        stat = scan()
                            call gargr (stdlam)
		    }

        	    if (y > 0.5)                 	# Fit at the top
		        call rv_linefit (rv, x1, x2, stdlam, OBJECT_SPECTRUM)
		    else
		        call rv_linefit (rv, x1, x2, stdlam, REFER_SPECTRUM)
		}
		prompt = false

	    case 'x':
		# Return to the correlation mode.
		RV_MODES(rv) = (RV_MODES(rv) - SPEC_MODE) / 10
		call sfree (sp)
		return (QUIT)

	    default:
	    	# Unknown command.
		prompt = false
		call rv_mode_prompt (rv)
		call rv_errmsg ("  Type '?' for a list of commands.")
	    }

	    if (prompt)
	        call rv_mode_prompt (rv)
	    ckey = key
	    #if (stridx(ckey,"?:bdepfsuvxqr\0") != 0)
	    if (ckey == 'n' || ckey == 'i')
	        RV_SPMKEY(rv) = key
	} until (clgcur("cursor",x,y,wcs,key,Memc[cmd],SZ_LINE) == EOF)

exit_ 	call sfree (sp)
	RV_MODES(rv) = (RV_MODES(rv) - SPEC_MODE) / 10
	return (OK)
end
