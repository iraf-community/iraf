include <gset.h>
include <gio.h>
include "rvpackage.h"
include "rvflags.h"
include "rvsample.h"

# RV_PARAM - File containing procedures for updating, unlearning, and 
# displaying the task or associated pset values.  Currrently only the
# FILTERPARS, CONTINPARS, or RVKEYWORDS psets are supported, as is the
# parameter list for the FXCOR task.


# RV_SHOW -- Process the ":show" command.  Optional arguments to
# this command include 'contin|filter|keywords' to show the pset 
# parameters.  The default is to show the current task parameters.

procedure rv_show (rv, fd)

pointer	rv				#I pointer to the RV structure
pointer	fd				#I File descriptor

pointer	sp, cmd

begin
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)

	# Get the command.
	call gargstr (Memc[cmd], SZ_LINE)
	if (Memc[cmd] == EOS) {
            call gdeactivate (RV_GP(rv), AW_CLEAR)
	    call rv_task_show (rv, fd)
            call greactivate (RV_GP(rv), AW_PAUSE)

	} else {
	    # Process the command.
	    switch (Memc[cmd+1]) {
	    case 'f':				# show 'filterpars' pset
                call gdeactivate (RV_GP(rv), AW_CLEAR)
	        call filt_show (rv, fd)
                call greactivate (RV_GP(rv), AW_PAUSE)
	    case 'k':				# show 'rvkeywords' pset
                call gdeactivate (RV_GP(rv), AW_CLEAR)
	        call keyw_show (rv, fd)
                call greactivate (RV_GP(rv), AW_PAUSE)
	    case 'c':				# show 'continpars' pset
                call gdeactivate (RV_GP(rv), AW_CLEAR)
	        call cont_show (rv, fd)
                call greactivate (RV_GP(rv), AW_PAUSE)
	    default:
		call printf (
		    "Choose one of 'contin|filter|keywords'.")
	    }
	}

	call sfree (sp)
end


# RV_TASK_SHOW - Show the current state of all task parameters.

procedure rv_task_show (rv, fd)

pointer	rv			#I RV struct pointer
pointer	fd			#I output fd (usually STDOUT)

pointer	sp, str, str2
bool	itob()
errchk	open

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)
	call salloc (str2, SZ_LINE, TY_CHAR)

	call fprintf (fd, "%21tFXCOR Current Parameter Settings:\n\n")

	# Print the Current Image Names
	call header_show (rv, fd)

	# Print some parameters
	call fprintf (fd,"\n")
	call fprintf (fd, "Apodize%20t= %f\n")
	    call pargr (RV_APODIZE(rv))
	call fprintf (fd, "Autowrite%20t= %b\n")
	    call pargb (itob(RV_AUTOWRITE(rv)))
	call fprintf (fd, "Autodraw%20t= %b\n")
	    call pargb (itob(RV_AUTODRAW(rv)))
	call fprintf (fd, "Background%20t= %f\n")
	    call pargr (RV_BACKGROUND(rv))
	call fprintf (fd, "Height%20t= %f\n")
	    call pargr (RV_FITHGHT(rv))
	call fprintf (fd, "Width%20t= %f\n")
	    call pargr (RV_FITWIDTH(rv))
	call fprintf (fd, "Imupdate%20t= %b\n")
	    call pargb (itob(RV_IMUPDATE(rv)))
	call fprintf (fd, "Minwidth%20t= %f\n")
	    call pargr (RV_MINWIDTH(rv))
	call fprintf (fd, "Maxwidth%20t= %f\n")
	    call pargr (RV_MAXWIDTH(rv))
	call fprintf (fd, "Peak%20t= %b\n")
	    call pargb (itob(RV_PEAK(rv)))
	call nam_verbose (rv, Memc[str2])
	call fprintf (fd, "Verbose%20t= `%s'\n")
	    call pargstr (Memc[str2])
	call fprintf (fd, "Weights%20t= %f\n")
	    call pargr (RV_WEIGHTS(rv))
	call fprintf (fd, "Window%20t= %f\n")
	    call pargr (RV_WINPAR(rv))
	call fprintf (fd, "Wincenter%20t= %f\n")
	    call pargr (RV_WINCENPAR(rv))

	call fprintf (fd,"\n")
	call rv_make_range_string (RV_OSAMPLE(rv), Memc[str])
	call fprintf (fd,"Object regions selected%25t= '%s'\n")
		call pargstr (Memc[str])
	call rv_make_range_string (RV_RSAMPLE(rv), Memc[str])
	call fprintf (fd,"Temp. regions selected%25t= '%s'\n\n\n")
		call pargstr (Memc[str])

	call sfree (sp)
end


# RV_UNLEARN -- Process the ":unlearn" command.  Optional arguments to
# this command include 'contin|filter|keywords' to reset the pset 
# parameters.  The default is to unlearn the current task parameters.

procedure rv_unlearn (rv)

pointer	rv				#I pointer to the RV structure

pointer	sp, cmd

begin
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)

	# Get the command.
	call gargstr (Memc[cmd], SZ_LINE)
	if (Memc[cmd] == EOS)
	    call rv_task_unlearn (rv)

	else {
	    # Process the command.
	    switch (Memc[cmd+1]) {
	    case 'f':				# unlearn 'filterpars' pset
	        call filt_unlearn (rv)
	    case 'k':				# unlearn 'rvkeywords' pset
	        call keyw_unlearn (rv)
	    case 'c':				# unlearn 'continpars' pset
		call cont_unlearn (rv)
	    default:
   		call printf (
		    "Choose one of 'contin|filter|keywords'.")
	    }
	}

	call sfree (sp)
end


# RV_TASK_UNLEARN - Reset the parameter values to their defaults.

procedure rv_task_unlearn (rv)

pointer	rv			# RV struct pointer

begin
	RV_APODIZE(rv) = 0.2
	RV_AUTOWRITE(rv) = YES
	RV_AUTODRAW(rv) = YES
	RV_BACKGROUND(rv) = 0.0
	RV_CCFTYPE(rv) = OUTPUT_IMAGE
	RV_CONTINUUM(rv) = BOTH
	RV_FILTER(rv) = NONE
	RV_FITFUNC(rv) = GAUSSIAN
	RV_FITHGHT(rv) = 0.0
	RV_FITWIDTH(rv) = INDEF
	RV_IMUPDATE(rv) = NO
	RV_INTERACTIVE(rv) = YES
	RV_MINWIDTH(rv) = 3.
	RV_MAXWIDTH(rv) = 21.
	RV_PEAK(rv) = NO
	RV_REBIN(rv) = RB_SMALL
	ORCOUNT(rv) = ALL_SPECTRUM
	RRCOUNT(rv) = ALL_SPECTRUM
	RV_VERBOSE(rv) = OF_LONG
	RV_WEIGHTS(rv) = 1
	RV_WINDOW(rv) = 20
	RV_WINCENPAR(rv) = INDEFI
end


# RV_UPDATE -- Process the ":update" command.  Optional arguments to
# this command include 'contin|filter|keywords' to reset the pset 
# parameters.  The default is to update the current task parameters.

procedure rv_update (rv)

pointer	rv				#I pointer to the RV structure

pointer	sp, cmd

begin
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)

	# Get the command.
	call gargstr (Memc[cmd], SZ_LINE)
	if (Memc[cmd] == EOS)
	    call rv_task_parupdate (rv)

	else {
	    # Process the command.
	    switch (Memc[cmd+1]) {
	    case 'f':				# update 'filterpars' pset
	        call filt_parupdate (rv)
	    case 'k':				# update 'rvkeywords' pset
	        call keyw_parupdate (rv)
	    case 'c':				# update 'continpars' pset
	        call cont_parupdate (rv)
	    default:
		call printf (
		    "Choose one of 'contin|filter|keywords'.")
	    }
	}

	call sfree (sp)
end


# RV_TASK_PARUPDATE - Update the parameter file with the current values in the
# structure.

procedure rv_task_parupdate (rv)

pointer	rv				#I RV struct pointer

pointer	sp, str, func, vb, rb
pointer	filt, cont, ap
bool	itob()

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)
	call salloc (cont, SZ_FNAME, TY_CHAR)
	call salloc (filt, SZ_FNAME, TY_CHAR)
	call salloc (func, SZ_FNAME, TY_CHAR)
	call salloc (ap, SZ_FNAME, TY_CHAR)
	call salloc (vb, SZ_FNAME, TY_CHAR)
	call salloc (rb, SZ_FNAME, TY_CHAR)

	iferr {
	    call clputr ("apodize", RV_APODIZE(rv))
	    call clputb ("autowrite", itob(RV_AUTOWRITE(rv)))
	    call clputb ("autodraw", itob(RV_AUTODRAW(rv)))
	    call clputr ("background", RV_BACKGROUND(rv))
	    call clputr ("height", RV_FITHGHT(rv))
	    call clputr ("width", RV_FITWIDTH(rv))
	    call clputb ("imupdate", itob(RV_IMUPDATE(rv)))
	    call clputr ("minwidth", RV_MINWIDTH(rv))
	    call clputr ("maxwidth", RV_MAXWIDTH(rv))
	    call clputb ("peak", itob(RV_PEAK(rv)))
	    call clputr ("weights", RV_WEIGHTS(rv))
	    call clputr ("window", RV_WINPAR(rv))
	    call clputr ("wincenter", RV_WINCENPAR(rv))

	    call rv_make_range_string (RV_OSAMPLE(rv), Memc[str])
	    call clpstr ("osample", Memc[str])
	    call rv_make_range_string (RV_RSAMPLE(rv), Memc[str])
	    call clpstr ("rsample", Memc[str])

	    call nam_verbose (rv, Memc[vb])
	    call clpstr ("verbose", Memc[vb])

	    call nam_fitfunc (rv, Memc[func])
	    call clpstr ("function", "gaussian")   

	    call nam_which (RV_CONTINUUM(rv), Memc[cont])
	    call clpstr ("continuum", Memc[cont])

	    call nam_rebin (rv, Memc[rb])
	    call clpstr ("rebin", Memc[rb])

	    call nam_which (RV_FILTER(rv), Memc[filt])
	    call clpstr ("filter", Memc[filt])

	    call nam_fitfunc (rv, Memc[func])
	    call clpstr ("function", Memc[func])

	    if (RV_CCFTYPE(rv) == OUTPUT_IMAGE)
	        call clpstr ("ccftype", "image")
	    else
	        call clpstr ("ccftype", "text")

	} then {
	    call sfree (sp)
	    call error (0, "Error updating parameters.")
	}

	call sfree (sp)
end


# HEADER_SHOW - show the current state of all parameters in the common
# header.

procedure header_show (rv, fd)

pointer	rv			#I RV struct pointer
int	fd			#I output file descriptor (usually STDOUT)

bool	streq()

begin
	# Print the Current Image Names
	call fprintf (fd,"Current Object Image%25t= '%.10s'\n")
		call pargstr (IMAGE(rv))
	call fprintf (fd,"Reference Image%25t= '%.10s'\n")
		call pargstr (RIMAGE(rv))
	call fprintf (fd,"Spool root Name%25t= '%.10s'\n")
		if (streq(SPOOL(rv),""))
		    call pargstr ("          ")
		else
		    call pargstr (SPOOL(rv))    
	call fprintf (fd,"Fitting Function%25t= %s\n")
	    	if (RV_FITFUNC(rv) == GAUSSIAN)
		    call pargstr ("'gaussian'")
	    	else if (RV_FITFUNC(rv) == LORENTZIAN)
		    call pargstr ("'lorentzian'")
	    	else	
		    call pargstr ("'parabola'")
end
