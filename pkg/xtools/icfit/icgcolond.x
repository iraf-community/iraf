# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<error.h>
include	<pkg/gtools.h>
include	"icfit.h"
include	"names.h"

# List of colon commands.
define	CMDS "|function|order|sample|naverage|niterate|low_reject|high_reject\
	|grow|markrej|color|show|vshow|xyshow|errors|evaluate\
	|graph|help|gui|"

define	FUNCTION	1	# Set or show function type
define	ORDER		2	# Set or show function order
define	SAMPLE		3	# Set or show sample ranges
define	NAVERAGE	4	# Set or show sample averaging or medianing
define	NITERATE	5	# Set or show rejection iterations
define	LOW_REJECT	6	# Set or show lower rejection factor
define	HIGH_REJECT	7	# Set or show upper rejection factor
define	GROW		8	# Set or show rejection growing radius
define	MARKREJ		9	# Mark rejected points
define	COLOR		10	# Fit color
define	SHOW		11	# Show values of parameters
define	VSHOW		12	# Show verbose information
define	XYSHOW		13	# Show x-y-fit-wts values
define	ERRORS		14	# Show errors of fit
define	EVALUATE	15	# Evaluate fit at specified value
define	GRAPH		16	# Define graph
define	HELP		17	# Set help file
define	GUI		18	# Send GUI command

# ICG_COLON -- Processes colon commands.  The common flags and newgraph
# signal changes in fitting parameters or the need to redraw the graph.

procedure icg_colond (ic, cmdstr, newgraph, gp, gt, cv, x, y, wts, npts)

pointer	ic				# ICFIT pointer
char	cmdstr[ARB]			# Command string
int	newgraph			# New graph?
pointer	gp				# GIO pointer
pointer	gt				# GTOOLS pointer
pointer	cv				# CURFIT pointer for error listing
double	x[npts], y[npts], wts[npts]	# Data arrays for error listing
int	npts				# Number of data points

double	val, dcveval()
char	key, xtype, ytype
bool	bval
int	ncmd, ival
real	rval
pointer	sp, cmd

int	nscan(), strdic(), btoi()

string	funcs "|chebyshev|legendre|spline1|spline3|power|"

begin
	# Check for GTOOLS command.
	if (cmdstr[1] == '/') {
	    call gt_colon (cmdstr, gp, gt, newgraph)
	    return
	}

	# Use formated scan to parse the command string.
	# The first word is the command and it may be minimum match
	# abbreviated with the list of commands.

	call smark (sp)
	call salloc (cmd, IC_SZSAMPLE, TY_CHAR)

	call sscan (cmdstr)
	call gargwrd (Memc[cmd], IC_SZSAMPLE)
	ncmd = strdic (Memc[cmd], Memc[cmd], IC_SZSAMPLE, CMDS)

	switch (ncmd) {
	case FUNCTION: # :function - List or set the fitting function.
	    call gargwrd (Memc[cmd], IC_SZSAMPLE)
	    if (nscan() == 1) {
		call printf ("function = %s\n")
		    call ic_gstr (ic, "function", Memc[cmd], IC_SZSAMPLE)
		    call pargstr (Memc[cmd])
	    } else {
		if (strdic (Memc[cmd], Memc[cmd], IC_SZSAMPLE, funcs) > 0) {
		    call ic_pstr (ic, "function", Memc[cmd])
		    IC_NEWFUNCTION(ic) = YES
		} else
		    call printf ("Unknown or ambiguous function\n")
	    }

	case ORDER: # :order - List or set the function order.
	    call gargi (ival)
	    if (nscan() == 1) {
		call printf ("order = %d\n")
		    call pargi (IC_ORDER(ic))
	    } else if (ival < 1) {
		call printf ("Order must be greater than zero\n")
	    } else {
		call ic_puti (ic, "order", ival)
		IC_NEWFUNCTION(ic) = YES
	    }

	case SAMPLE: # :sample - List or set the sample points.
	    call gargstr (Memc[cmd], IC_SZSAMPLE)
	    if (Memc[cmd] == EOS) {
	        call printf ("sample = %s\n")
		    call pargstr (Memc[IC_SAMPLE(ic)])
	    } else {
		call ic_pstr (ic, "sample", Memc[cmd])
		IC_NEWX(ic) = YES
	    }

	case NAVERAGE: # :naverage - List or set the sample averging.
	    call gargi (ival)
	    if (nscan() == 1) {
		call printf ("naverage = %d\n")
		    call pargi (IC_NAVERAGE(ic))
	    } else {
		call ic_puti (ic, "naverage", ival)
		IC_NEWX(ic) = YES
	    }

	case NITERATE: # :niterate - List or set the rejection iterations.
	    call gargi (ival)
	    if (nscan() == 1) {
		call printf ("niterate = %d\n")
		    call pargi (IC_NITERATE(ic))
	    } else
		call ic_puti (ic, "niterate", ival)


	case LOW_REJECT: # :low_reject - List or set lower rejection factor.
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("low_reject = %g\n")
		    call pargr (IC_LOW(ic))
	    } else
		call ic_putr (ic, "low", rval)

	case HIGH_REJECT: # :high_reject - List or set high rejection factor.
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("high_reject = %g\n")
		    call pargr (IC_HIGH(ic))
	    } else
		call ic_putr (ic, "high", rval)

	case GROW: # :grow - List or set the rejection growing.
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("grow = %g\n")
		    call pargr (IC_GROW(ic))
	    } else
		call ic_putr (ic, "grow", rval)

	case MARKREJ: # :markrej - Mark rejected points
	    call gargb (bval)
	    if (nscan() == 1) {
		call printf ("markrej = %b\n")
		    call pargi (IC_MARKREJ(ic))
	    } else
		call ic_puti (ic, "markrej", btoi (bval))

	case COLOR: # :color - List or set the fit color.
	    call gargi (ival)
	    if (nscan() == 1) {
		call printf ("color = %d\n")
		    call pargi (IC_COLOR(ic))
	    } else
		call ic_puti (ic, "color", ival)

	case SHOW, VSHOW, XYSHOW, ERRORS:
	    call ic_guishowd (ic, cmdstr, cv, x, y, wts, npts)

	case EVALUATE: # :evaluate x - evaluate fit at x.
	    call gargd (val)
	    if (nscan() == 1)
		call printf ("evaluate requires a value to evaluate\n")
	    else {
		call printf ("fit(%g) = %g\n")
		    call pargd (val)
		    call pargd (dcveval (cv, val))
	    }

	case GRAPH: # :graph key xtype ytpe
	    call gargc (key)
	    call gargc (xtype)
	    call gargc (ytype)
	    if (nscan() != 4) {
		ival = IC_GKEY(ic)
		call printf ("graph %c %c %c\n")
		    call pargi ('h'+ival-1)
		    call pargi (IC_AXES(ic,ival,1))
		    call pargi (IC_AXES(ic,ival,2))
	    } else {
		ival = key - 'h' + 1
		IC_GKEY(ic) = ival
		call ic_pkey (ic, ival, int(xtype), int(ytype))
		newgraph = YES
	    }

	case HELP: # :help file
	    call gargwrd (Memc[cmd], IC_SZSAMPLE)
	    if (Memc[cmd] == EOS) {
	        call printf ("help = %s\n")
		    call pargstr (Memc[IC_HELP(ic)])
	    } else
		call ic_pstr (ic, "help", Memc[cmd])

        case GUI: # :gui command - Update, unlearn or set the options.
	    call gargstr (Memc[cmd], IC_SZSAMPLE)
	    call ic_gui (ic, Memc[cmd])

	default: # Unrecognized command.
	    call printf ("Unrecognized command or ambiguous abbreviation\007")
	}

	call sfree (sp)
end
