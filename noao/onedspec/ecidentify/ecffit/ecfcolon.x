include	<error.h>
include	<gset.h>

# List of colon commands
define	CMDS "|show|function|xorder|yorder|niterate|lowreject|highreject|"

define	SHOW		1	# Show parameters
define	FUNCTION	2	# Set or show function type
define	XORDER		3	# Set or show x order of function
define	YORDER		4	# Set or show y order of function
define	NITERATE	5	# Set or show rejection iterations
define	LOW		6	# Set or show low rejection threshold
define	HIGH		7	# Set or show high rejection threshold

# ECF_COLON -- Processes colon commands.

procedure ecf_colon (cmdstr, gp)

char	cmdstr[ARB]			# Command string
pointer	gp				# GIO pointer

double	dval
int	ncmd, ival
int	nscan(), strdic()
include	"ecffit.com"

begin
	# Use formated scan to parse the command string.
	# The first word is the command and it may be minimum match
	# abbreviated with the list of commands.

	call sscan (cmdstr)
	call gargwrd (ecfstr, SZ_LINE)
	ncmd = strdic (ecfstr, ecfstr, SZ_LINE, CMDS)

	switch (ncmd) {
	case SHOW: # :show - Show the values of the fitting parameters.
	    call gdeactivate (gp, AW_CLEAR)
	    call printf ("function %s\nxorder %d\nyorder %d\n")
		call pargstr (function)
		call pargi (xorder)
		call pargi (yorder)
	    call printf ("niterate %d\nlowreject %g\nhighreject\nnreject %d\n")
		call pargi (niterate)
		call pargd (low)
		call pargd (high)
		call pargi (nreject)
	    call printf ("slope %d\noffset %d\nshift %g\n")
		call pargi (slope)
		call pargi (offset)
		call pargd (shift)
	    call printf ("rms %g\n")
		call pargd (rms)
	    call greactivate (gp, AW_PAUSE)
	case FUNCTION: # :function - List or set the fitting function.
	    call gargwrd (ecfstr, SZ_LINE)
	    if (nscan() == 1) {
		call printf ("function = %s\n")
		    call pargstr (function)
	    } else {
		iferr (call ecf_sets ("function", ecfstr))
		    call erract (EA_WARN)
	    }
	case XORDER: # xorder: List or set the function order.
	    call gargi (ival)
	    if (nscan() == 1) {
		call printf ("xorder %d\n")
		    call pargi (xorder)
	    } else
		xorder = ival
	case YORDER: # yorder: List or set the function order.
	    call gargi (ival)
	    if (nscan() == 1) {
		call printf ("yorder %d\n")
		    call pargi (yorder)
	    } else
		yorder = ival
	case NITERATE: # niterate: List or set rejection iterations.
	    call gargi (ival)
	    if (nscan() == 1) {
		call printf ("niterate %d\n")
		    call pargi (niterate)
	    } else
		niterate = ival
	case LOW: # low: List or set low rejection threshold.
	    call gargd (dval)
	    if (nscan() == 1) {
		call printf ("lowreject %g\n")
		    call pargd (low)
	    } else
		low = dval
	case HIGH: # highreject: List or set high rejection threshold.
	    call gargd (dval)
	    if (nscan() == 1) {
		call printf ("highreject %g\n")
		    call pargd (high)
	    } else
		high = dval
	default:
	    call printf ("Unrecognized or ambiguous command\007")
	}
end
