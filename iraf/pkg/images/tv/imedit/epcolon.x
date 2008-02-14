include	"epix.h"
 
# List of colon commands.
define	CMDS "|angh|angv|aperture|autodisplay|autosurface|buffer|command|\
	|display|eparam|graphics|input|output|radius|search|sigma|\
	|value|minvalue|maxvalue|width|write|xorder|yorder|"
 
define	ANGH		1	# Horizontal viewing angle
define	ANGV		2	# Vertical viewing angle
define	APERTURE	3	# Aperture type
define 	AUTODISPLAY	4	# Automatic display?
define	AUTOSURFACE	5	# Automatic surface graph?
define	BUFFER		6	# Background buffer width
define	COMMAND		7	# Display command
define	DISPLAY		9	# Display image?
define	EPARAM		10	# Eparam
define	GRAPHICS	11	# Graphics device
define	INPUT		12	# Input image
define	OUTPUT		13	# Output image
define	RADIUS		14	# Aperture radius
define	SEARCH		15	# Search radius
define	SIGMA		16	# Noise sigma
define	VALUE		18	# Constant substitution value
define	MINVALUE	19	# Minimum value for replacement
define	MAXVALUE	20	# Maximum value for replacement
define	WIDTH		21	# Background width
define	WRITE		22	# Write output
define	XORDER		23	# X order
define	YORDER		24	# Y order
 
# EP_COLON -- Respond to colon commands.
# The changed parameters are written to the parameter file and
# to the optional log file.
 
procedure ep_colon (ep, cmdstr, newimage)
 
pointer	ep			# EPIX structure
char	cmdstr[ARB]		# Colon command
int	newimage		# New image?
 
int	ival, ncmd
real	rval
bool	bval
pointer	sp, cmd
 
bool	strne()
int	nscan(), strdic(), btoi(), imaccess()
pointer	immap()
 
begin
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)
 
	# Scan the command string and get the first word.
	call sscan (cmdstr)
	call gargwrd (Memc[cmd], SZ_LINE)
	ncmd = strdic (Memc[cmd], Memc[cmd], SZ_LINE, CMDS)
 
	switch (ncmd) {
	case ANGH:
	    call gargr (rval)
	    if (nscan() == 1) {
	        call printf ("angh %g\n")
		    call pargr (EP_ANGH(ep))
	    } else {
		EP_ANGH(ep) = rval
	        call clputr ("angh", EP_ANGH(ep))
	    }
	case ANGV:
	    call gargr (rval)
	    if (nscan() == 1) {
	        call printf ("angv %g\n")
		    call pargr (EP_ANGV(ep))
	    } else {
		EP_ANGV(ep) = rval
	        call clputr ("angv", EP_ANGV(ep))
	    }
	case APERTURE:
	    call gargwrd (Memc[cmd], SZ_FNAME)
	    if (nscan() == 1) {
	        call printf ("aperture %s\n")
		    switch (EP_APERTURE(ep)) {
		    case APCIRCULAR:
			call pargstr ("circular")
		    case APSQUARE:
			call pargstr ("square")
		    }
	    } else {
		ncmd = strdic (Memc[cmd], Memc[cmd], SZ_LINE, APTYPES)
		if (ncmd > 0) {
		    EP_APERTURE(ep) = ncmd
		    call clpstr ("aperture", Memc[cmd])
		    if (EP_LOGFD(ep) != NULL) {
			call fprintf (EP_LOGFD(ep), ":aperture %s\n")
			    call pargstr (Memc[cmd])
		    }
		} else
		    call printf ("Unknown aperture type\n")
	    }
	case AUTODISPLAY:
	    call gargb (bval)
	    if (nscan() == 1) {
		if (EP_AUTODISPLAY(ep) == YES)
	            call printf ("autodisplay yes\n")
		else
	            call printf ("autodisplay no\n")
	    } else {
		EP_AUTODISPLAY(ep) = btoi (bval)
	        call clputb ("autodisplay", bval)
	    }
	case AUTOSURFACE:
	    call gargb (bval)
	    if (nscan() == 1) {
		if (EP_AUTOSURFACE(ep) == YES)
	            call printf ("autosurface yes\n")
		else
	            call printf ("autosurface no\n")
	    } else {
		EP_AUTOSURFACE(ep) = btoi (bval)
	        call clputb ("autosurface", bval)
	    }
	case BUFFER:
	    call gargr (rval)
	    if (nscan() == 1) {
	        call printf ("buffer %g\n")
		    call pargr (EP_BUFFER(ep))
	    } else {
		EP_BUFFER(ep) = rval
	        call clputr ("buffer", EP_BUFFER(ep))
		if (EP_LOGFD(ep) != NULL) {
		    call fprintf (EP_LOGFD(ep), ":buffer %g\n")
			call pargr (EP_BUFFER(ep))
		}
	    }
	case COMMAND:
	    call gargwrd (Memc[cmd], SZ_FNAME)
	    if (nscan() == 1) {
	        call printf ("command %s\n")
		    call pargstr (EP_COMMAND(ep))
	    } else {
		call strcpy (Memc[cmd], EP_COMMAND(ep), EP_SZLINE)
	        call gargstr (Memc[cmd], SZ_FNAME)
	        call strcat (Memc[cmd], EP_COMMAND(ep), EP_SZFNAME)
	        call clpstr ("command", EP_COMMAND(ep))
	    }
	case DISPLAY:
	    call gargb (bval)
	    if (nscan() == 1) {
		if (EP_DISPLAY(ep) == YES)
	            call printf ("display yes\n")
		else
	            call printf ("display no\n")
	    } else {
		EP_DISPLAY(ep) = btoi (bval)
	        call clputb ("display", bval)
	    }
	case EPARAM:
	    call clcmdw ("eparam imedit")
	    call ep_setpars (ep)
	case GRAPHICS:
	    call gargwrd (Memc[cmd], SZ_FNAME)
	    if (nscan() == 1) {
	        call printf ("graphics %s\n")
		    call pargstr (EP_GRAPHICS(ep))
	    } else {
		call strcpy (Memc[cmd], EP_GRAPHICS(ep), EP_SZFNAME)
	        call clpstr ("graphics", EP_GRAPHICS(ep))
	    }
	case INPUT:
	    call gargwrd (Memc[cmd], SZ_FNAME)
	    if (nscan() == 1) {
	        call printf ("input %s\n")
		    call pargstr (EP_INPUT(ep))
	    } else if (strne (Memc[cmd], EP_INPUT(ep))) {
		call strcpy (Memc[cmd], EP_INPUT(ep), SZ_LINE)
		newimage = YES
	    }
	case OUTPUT:
	    call gargwrd (Memc[cmd], SZ_FNAME)
	    if (nscan() == 1) {
	        call printf ("output %s\n")
		    call pargstr (EP_OUTPUT(ep))
	    } else if (strne (Memc[cmd], EP_INPUT(ep))) {
	    	if (imaccess (Memc[cmd], READ_ONLY) == YES) {
		    call eprintf ("%s: Output image %s exists\n")
		        call pargstr (EP_INPUT(ep))
		        call pargstr (Memc[cmd])
		} else
		    call strcpy (Memc[cmd], EP_OUTPUT(ep), EP_SZFNAME)
	    }
	case RADIUS:
	    call gargr (rval)
	    if (nscan() == 1) {
	        call printf ("radius %g\n")
		    call pargr (EP_RADIUS(ep))
	    } else {
		EP_RADIUS(ep) = rval
	        call clputr ("radius", EP_RADIUS(ep))
		if (EP_LOGFD(ep) != NULL) {
		    call fprintf (EP_LOGFD(ep), ":radius %g\n")
			call pargr (EP_RADIUS(ep))
		}
	    }
	case SEARCH:
	    call gargr (rval)
	    if (nscan() == 1) {
	        call printf ("search %g\n")
		    call pargr (EP_SEARCH(ep))
	    } else {
		EP_SEARCH(ep) = rval
	        call clputr ("search", EP_SEARCH(ep))
		if (EP_LOGFD(ep) != NULL) {
		    call fprintf (EP_LOGFD(ep), ":search %g\n")
			call pargr (EP_SEARCH(ep))
		}
	    }
	case SIGMA:
	    call gargr (rval)
	    if (nscan() == 1) {
	        call printf ("sigma %g\n")
		    call pargr (EP_SIGMA(ep))
	    } else {
		EP_SIGMA(ep) = rval
	        call clputr ("sigma", EP_SIGMA(ep))
		if (EP_LOGFD(ep) != NULL) {
		    call fprintf (EP_LOGFD(ep), ":sigma %g\n")
			call pargr (EP_SIGMA(ep))
		}
	    }
	case VALUE:
	    call gargr (rval)
	    if (nscan() == 1) {
	        call printf ("value %g\n")
		    call pargr (EP_VALUE(ep))
	    } else {
		EP_VALUE(ep) = rval
	        call clputr ("value", EP_VALUE(ep))
		if (EP_LOGFD(ep) != NULL) {
		    call fprintf (EP_LOGFD(ep), ":value %g\n")
			call pargr (EP_VALUE(ep))
		}
	    }
	case MINVALUE:
	    call gargr (rval)
	    if (nscan() == 1) {
	        call printf ("minvalue %g\n")
		    call pargr (EP_MINVALUE(ep))
	    } else {
		EP_MINVALUE(ep) = rval
	        call clputr ("minvalue", EP_MINVALUE(ep))
		if (EP_LOGFD(ep) != NULL) {
		    call fprintf (EP_LOGFD(ep), ":minvalue %g\n")
			call pargr (EP_MINVALUE(ep))
		}
	    }
	case MAXVALUE:
	    call gargr (rval)
	    if (nscan() == 1) {
	        call printf ("maxvalue %g\n")
		    call pargr (EP_MAXVALUE(ep))
	    } else {
		EP_MAXVALUE(ep) = rval
	        call clputr ("maxvalue", EP_MAXVALUE(ep))
		if (EP_LOGFD(ep) != NULL) {
		    call fprintf (EP_LOGFD(ep), ":maxvalue %g\n")
			call pargr (EP_MAXVALUE(ep))
		}
	    }
	case WIDTH:
	    call gargr (rval)
	    if (nscan() == 1 || rval < 1.) {
	        call printf ("width %g\n")
		    call pargr (EP_WIDTH(ep))
	    } else {
		EP_WIDTH(ep) = max (1., rval)
	        call clputr ("width", EP_WIDTH(ep))
		if (EP_LOGFD(ep) != NULL) {
		    call fprintf (EP_LOGFD(ep), ":width %g\n")
			call pargr (EP_WIDTH(ep))
		}
	    }
	case WRITE:
	    call gargwrd (Memc[cmd], SZ_FNAME)
	    ival = YES
	    if (nscan() == 1)
		call strcpy (EP_OUTPUT(ep), Memc[cmd], SZ_FNAME)
	    else if (strne (Memc[cmd], EP_INPUT(ep))) {
	    	if (imaccess (Memc[cmd], READ_ONLY) == YES) {
		    call eprintf ("Image %s exists\n")
		        call pargstr (Memc[cmd])
		    ival = NO
		}
	    }
 
	    if (ival == YES) {
	        call printf ("output %s\n")
		    call pargstr (Memc[cmd])
		if (imaccess (Memc[cmd], READ_ONLY) == YES)
		    call imdelete (Memc[cmd])
		call imunmap (EP_IM(ep))
	        call ep_imcopy (EP_WORK(ep), Memc[cmd])
		EP_IM(ep) = immap (EP_WORK(ep), READ_WRITE, 0)
	    }
	case XORDER:
	    call gargi (ival)
	    if (nscan() == 1) {
	        call printf ("xorder %d\n")
		    call pargi (EP_XORDER(ep))
	    } else {
		EP_XORDER(ep) = max (0, ival)
	        call clputi ("xorder", EP_XORDER(ep))
		if (EP_LOGFD(ep) != NULL) {
		    call fprintf (EP_LOGFD(ep), ":xorder %d\n")
			call pargi (EP_XORDER(ep))
		}
	    }
	case YORDER:
	    call gargi (ival)
	    if (nscan() == 1) {
	        call printf ("yorder %d\n")
		    call pargi (EP_YORDER(ep))
	    } else {
		EP_YORDER(ep) = max (0, ival)
	        call clputi ("yorder", EP_YORDER(ep))
		if (EP_LOGFD(ep) != NULL) {
		    call fprintf (EP_LOGFD(ep), ":yorder %d\n")
			call pargi (EP_YORDER(ep))
		}
	    }
	default:
	    call printf ("Unrecognized or ambiguous command\007")
	}
 
	call sfree (sp)
end
