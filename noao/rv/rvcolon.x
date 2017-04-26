include <gset.h>
include <gio.h>
include "rvpackage.h"
include "rvflags.h"
include "rvcomdef.h"

# RV_COLON -- Process the task colon commands.

int procedure rv_colon (rv, cmdstr, written, infile, ref_infile, res_infile)

pointer	rv				#I pointer to the RV structure
char	cmdstr[SZ_LINE]			#I Command string
bool	written				#I Results written yet?
pointer	infile				#U Input file list pointer
pointer	ref_infile			#U Template file list pointer
pointer	res_infile			#U Results file list pointer

pointer	sp, cmd
int	strdic()
int	rv_xcor_colon()

begin
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)

	# Get the command.
	call sscan (cmdstr)
	call gargwrd (Memc[cmd], SZ_LINE)
	if (Memc[cmd] == EOS) {
	    call sfree (sp)
	    return (OK)
	}

	# Process the command.
	if (strdic(Memc[cmd], Memc[cmd], SZ_LINE, CONT_KEYWORDS) != 0) { 
	    # Process the CONTPARS pset commands 
	    call cont_colon (rv, cmdstr)

	} else if (strdic(Memc[cmd], Memc[cmd], SZ_LINE, KEY_KEYWORDS) != 0) {
	    # Process the RVKEYWORDS pset commands
	    call keyw_colon (rv, cmdstr)

	} else if (strdic(Memc[cmd], Memc[cmd], SZ_LINE, FILT_KEYWORDS) != 0) {
	    # Process the FILTERPARS pset commands
	    call filt_colon (rv, cmdstr)

	} else if (strdic(Memc[cmd], Memc[cmd], SZ_LINE, DEBUG_KEYWORDS) != 0) {
	    # Process the DEBUG commands
	    call rv_debug (rv, cmdstr)

	} else { 	
	    # Now punt and send it off to the task
	    if (rv_xcor_colon(rv,cmdstr,written,infile,ref_infile) == QUIT)
	        return (QUIT)
	}

	call sfree (sp)
	return (OK)
end


# RV_XCOR_COLON - Procedure to process the colon commands defined below.  Most
# commands are for interactive editing of parameters to the task.

int procedure rv_xcor_colon (rv, cmdstr, written, infile, rinfile)

pointer	rv				#I RV struct pointer
char	cmdstr[SZ_LINE]			#I Command
bool	written				#I Results written yet?
int	infile				#U Input list file pointer
int	rinfile				#U Input list file pointer

pointer sp, cmd, buf
int 	strdic()
int	cmd_regions(), cmd_objects(), cmd_tnum()
int	cmd_next(), cmd_previous(), cmd_refspec()

define	error_		99

begin
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)
	call salloc (buf, SZ_LINE, TY_CHAR)

	call sscan (cmdstr)
	call gargwrd (Memc[cmd], SZ_LINE)

	# Unpack the keyword from the string and look it up in the
	# dictionary.  Switch on command and call the appropriate routines.
	switch (strdic(Memc[cmd], Memc[cmd], SZ_FNAME, RVX_KEYWORDS)) {
	case RVX_APERTURES:
	    # Set/Show the aperture processing list 
	    call cmd_aplist (rv, written)

	case RVX_APNUM:
	    # Set/Show the current aperture number to process
	    call cmd_apnum (rv, written)

	case RVX_APODIZE:
	    # Set/Show the apodize percentage
	    call cmd_apodize (rv)

	case RVX_AUTODRAW:
	    # Set/Show the autowrite parameter toggle
	    call cmd_autodraw (rv)

	case RVX_AUTOWRITE:
	    # Set/Show the autowrite parameter toggle
	    call cmd_autowrite (rv)

	case RVX_BACKGROUND:
	    # Set/Show the background parameter
	    call cmd_background (rv)

	case RVX_CCFTYPE:
	    # Set/Show the ccf output type
	    call cmd_out_type (rv)
	
	case RVX_COMMENT:
	    # Add a comment to the output logs
	    call cmd_add_comment (rv)
	
	case RVX_CONTINUUM:
	    # Set/Show the continuum flag
	    call cmd_continuum (rv)

	case RVX_CORRECTION:
	    # Convert a pixel shift to a velocity
	    call cmd_correction (rv)

	case RVX_DELTAV:
	    # Print the velocity dispersion
	    call cmd_deltav (rv)

	case RVX_DISP:
	    # Print the rebinned dispersion info.
	    call cmd_prtdisp (rv)

	case RVX_FILTER:
	    # Set/Show the filter flag
	    call cmd_filter (rv)

	case RVX_FUNCTION:
	    # Set/Show the fitting function
	    call cmd_fitfunc (rv)

	case RVX_HEIGHT:
	    # Set/Show the fit height
	    call cmd_height (rv)

	case RVX_IMUPDATE:
	    # Set/Show the image update flag
	    call cmd_imupdate (rv)

	case RVX_LINECOLOR:
	    # Set/Show the overlay vector line color
	    call cmd_linecolor (rv)

	case RVX_MINWIDTH:
	    # Set/Show the minwidth parameter
	    call cmd_minwidth (rv)

	case RVX_MAXWIDTH:
	    # Set/Show the maxwidth parameter
	    call cmd_maxwidth (rv)

	case RVX_NBANG:
	    # Move on to next spectrum in list ignoring the data write
	    written = true
	    if (cmd_next(rv,infile,rinfile,written,cmdstr) == ERR_READ)
		goto error_

	case RVX_NEXT:
	    # Move on to next spectrum in list
	    if (cmd_next(rv,infile,rinfile,written,cmdstr) == ERR_READ)
		goto error_

	case RVX_OBJECTS:
	    # Set/Show the object image list
	    if (cmd_objects(rv,infile,written) == ERR_READ)
		goto error_

	case RVX_OUTPUT:
	    # Set/Show the output root filename
	    call cmd_output (rv)

	case RVX_OSAMPLE:
	    # Set/Show the object  sample region for correlation
	    if (cmd_regions(rv, RV_OSAMPLE(rv)) == ERR_CORREL)
		goto error_

	case RVX_PBANG:
	    # Move to previous image, ignoring the data write
	    written = true
	    if (cmd_previous(rv,infile,rinfile,written,cmdstr) == ERR_READ)
		goto error_

	case RVX_PEAK:
	    # Set/Show the peak parameter
	    call cmd_peak (rv)

	case RVX_PIXCORR:
	    # Set/Show the pixcorr parameter
	    call cmd_pixcorr (rv)

	case RVX_PREVIOUS:
	    # Move to previous image
	    if (cmd_previous(rv,infile,rinfile,written,cmdstr) == ERR_READ)
		goto error_

	case RVX_PRINTZ:
	    # Toggle output of Z valuesj
	    call cmd_printz (rv)

	case RVX_REBIN:
	    # Set/Show the rebin param
	    call cmd_rebin (rv)

	case RVX_RESULTS:
	    # Page a logfile of results
	    call cmd_result (rv)

	case RVX_RSAMPLE:
	    # Set/Show the template sample region for correlation
	    if (cmd_regions(rv, RV_RSAMPLE(rv)) == ERR_CORREL)
		goto error_

	case RVX_SHOW:
             # List the current values of all parameters
             call rv_show (rv, STDOUT)

	case RVX_TEMPLATES:
	    # Reset the template list pointer
	    if (cmd_refspec(rv, rinfile, written) == ERR_READ)
		goto error_

	case RVX_TEMPVEL:
	    # Set/Show the template velocity
	    call cmd_tempvel (rv, RV_TEMPNUM(rv))

	case RVX_TEXTCOLOR:
	    # Set/Show the graphics text color.
	    call cmd_textcolor (rv)

	case RVX_TNUM:
	    # Move on to next spectrum in list
	    if (cmd_tnum(rv, rinfile, written, cmdstr) == ERR_READ)
		goto error_

	case RVX_UNLEARN:
	    # Unlearn the task parameters
	    call rv_unlearn (rv)

	case RVX_UPDATE:
	    # Update the task with current interactive parameters
	    call rv_update (rv)

	case RVX_VERBOSE:
	    # Set/Show the verbose flag
	    call cmd_verbose (rv)

	case RVX_VERSION:
	    # Show the task version (Hidden Command)
	    call cmd_version ()

	case RVX_WCCF:
	    # Write the CCF as an image or text file
	    call write_ccf (rv)

	case RVX_WEIGHTS:
	    # Set/Show the weights flag
	    call cmd_weights (rv)

	case RVX_WIDTH:
	    # Set/Show the width parameter
	    call cmd_width (rv)

	case RVX_WINCENTER:
	    # Set/Show the window center
	    call cmd_wincenter (rv)

	case RVX_WINDOW:
	    # Set/Show the ccf window width
	    call cmd_window (rv)

	case RVX_YMIN:
	    # Set/Show the ccf window bottom
	    call cmd_ymin (rv)

	case RVX_YMAX:
	    # Set/Show the ccf window top
	    call cmd_ymax (rv)

	default:
	    # Default action
	    call rv_errmsg ("fxcor: Type '?' for a list of commands.")
	}

	call sfree (sp)
	return (OK)

error_  call sfree (sp)
	return (ERR_READ)
end
