include	<error.h>
include	<gset.h>

# List of colon commands
define	CMDS "|show|function|xorder|yorder|"

define	SHOW		1	# Show parameters
define	FUNCTION	2	# Set or show function type
define	XORDER		3	# Set or show x order of function
define	YORDER		4	# Set or show y order of function

# ECF_COLON -- Processes colon commands.

procedure ecf_colon (cmdstr, gp, sf)

char	cmdstr[ARB]			# Command string
pointer	gp				# GIO pointer
pointer	sf				# Surface pointer

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
	    call printf ("function %s\n")
		call pargstr (function)
	    call printf ("xorder %d\n")
		call pargi (xorder)
	    call printf ("yorder %d\n")
		call pargi (yorder)
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
	default:
	    call printf ("\07\n")
	}
end
