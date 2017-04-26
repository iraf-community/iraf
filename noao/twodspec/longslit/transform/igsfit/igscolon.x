include	<gset.h>

# List of colon commands
define	CMDS "|show|function|xorder|yorder|corners|"

define	SHOW		1	# Show parameters
define	FUNCTION	2	# Set or show function type
define	XORDER		3	# Set or show x order of function
define	YORDER		4	# Set or show y order of function
define	CORNERS		5	# Show corners

# IGS_COLON -- Processes colon commands.

procedure igs_colon (cmdstr, gp, sf)

char	cmdstr[ARB]			# Command string
pointer	gp				# GIO pointer
pointer	sf				# Surface pointer

char	cmd[SZ_LINE]
int	ncmd, ival

int	nscan(), strdic()
real	xgseval()

string	funcs "|chebyshev|legendre|"

include	"igsfit.com"

begin
	# Use formated scan to parse the command string.
	# The first word is the command and it may be minimum match
	# abbreviated with the list of commands.

	call sscan (cmdstr)
	call gargwrd (cmd, SZ_LINE)
	ncmd = strdic (cmd, cmd, SZ_LINE, CMDS)

	switch (ncmd) {
	case SHOW: # :show - Show the values of the fitting parameters.
	    call gdeactivate (gp, AW_CLEAR)
	    call printf ("function %s\n")
		call pargstr (function)
	    call printf ("xorder %d\n")
		call pargi (xorder)
	    call printf ("yorder %d\n")
		call pargi (yorder)
	    call printf ("Fitted coordinates at the corners of the images:\n")
	    call printf ("    (%d, %d) = %g  (%d, %d) = %g\n")
		call pargr (xmin)
		call pargr (ymin)
		call pargr (xgseval (sf, xmin, ymin))
		call pargr (xmax)
		call pargr (ymin)
		call pargr (xgseval (sf, xmax, xmin))
	    call printf ("    (%d, %d) = %g  (%d, %d) = %g\n")
		call pargr (xmin)
		call pargr (ymax)
		call pargr (xgseval (sf, xmin, ymax))
		call pargr (xmax)
		call pargr (ymax)
		call pargr (xgseval (sf, xmax, ymax))
	    call printf ("rms %g\n")
		call pargr (rms)
	    call greactivate (gp, AW_PAUSE)

	case FUNCTION: # :function - List or set the fitting function.
	    call gargwrd (cmd, SZ_LINE)
	    if (nscan() == 1) {
		call printf ("function = %s\n")
		    call pargstr (function)
	    } else {
		if (strdic (cmd, cmd, SZ_LINE, funcs) > 0)
		    call strcpy (cmd, function, SZ_LINE)
		else
		    call printf ("Unknown or ambiguous function\n")
	    }

	case XORDER: # xorder: List or set the function order.
	    call gargi (ival)
	    if (nscan() == 1) {
		call printf ("xorder %d\n")
		    call pargi (xorder)
	    } else if (ival < 2)
		call printf ("xorder must be at least 2\n")
	    else
		xorder = ival

	case YORDER: # yorder: List or set the function order.
	    call gargi (ival)
	    if (nscan() == 1) {
		call printf ("yorder %d\n")
		    call pargi (yorder)
	    } else if (ival < 2)
		call printf ("yorder must be at least 2\n")
	    else
		yorder = ival
	case CORNERS: # corners: List coordinates at corners.
	    call printf ("(%d,%d)=%g (%d,%d)=%g (%d,%d)=%g (%d,%d)=%g\n")
		call pargr (xmin)
		call pargr (ymin)
		call pargr (xgseval (sf, xmin, ymin))
		call pargr (xmax)
		call pargr (ymin)
		call pargr (xgseval (sf, xmax, xmin))
		call pargr (xmin)
		call pargr (ymax)
		call pargr (xgseval (sf, xmin, ymax))
		call pargr (xmax)
		call pargr (ymax)
		call pargr (xgseval (sf, xmax, ymax))
	default:
	    call printf ("Unrecognized or ambiguous command\007")
	}
end
