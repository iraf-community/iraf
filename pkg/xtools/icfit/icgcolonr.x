# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gset.h>
include	<error.h>
include	"icfit.h"
include	"names.h"

# List of colon commands.
define	CMDS "|show|sample|naverage|function|order|low_reject|high_reject|\
	|niterate|grow|errors|vshow|"

define	SHOW		1	# Show values of parameters
define	SAMPLE		2	# Set or show sample ranges
define	NAVERAGE	3	# Set or show sample averaging or medianing
define	FUNCTION	4	# Set or show function type
define	ORDER		5	# Set or show function order
define	LOW_REJECT	6	# Set or show lower rejection factor
define	HIGH_REJECT	7	# Set or show upper rejection factor
# newline		8
define	NITERATE	9	# Set or show rejection iterations
define	GROW		10	# Set or show rejection growing radius
define	ERRORS		11	# Show errors of fit
define	VSHOW		12	# Show verbose information

# ICG_COLON -- Processes colon commands.  The common flags and newgraph
# signal changes in fitting parameters or the need to redraw the graph.

procedure icg_colonr (ic, cmdstr, newgraph, gp, gt, cv, x, y, wts, npts)

pointer	ic				# ICFIT pointer
char	cmdstr[ARB]			# Command string
int	newgraph			# New graph?
pointer	gp				# GIO pointer
pointer	gt				# GTOOLS pointer
pointer	cv				# CURFIT pointer for error listing
real	x[npts], y[npts], wts[npts]	# Data arrays for error listing
int	npts				# Number of data points

char	cmd[SZ_LINE]
int	ncmd, ival
real	rval

int	nscan(), strdic()

string	funcs "|chebyshev|legendre|spline1|spline3|power|"

begin
	# Use formated scan to parse the command string.
	# The first word is the command and it may be minimum match
	# abbreviated with the list of commands.

	call sscan (cmdstr)
	call gargwrd (cmd, SZ_LINE)
	ncmd = strdic (cmd, cmd, SZ_LINE, CMDS)

	switch (ncmd) {
	case SHOW: # :show - Show the values of the fitting parameters.
	    call gargwrd (cmd, SZ_LINE)
	    if (nscan() == 1) {
		call gdeactivate (gp, AW_CLEAR)
		call ic_show (ic, "STDOUT", gt)
		call greactivate (gp, AW_PAUSE)
	    } else {
		iferr (call ic_show (ic, cmd, gt))
		    call erract (EA_WARN)
	    }

	case SAMPLE: # :sample - List or set the sample points.
	    call gargstr (cmd, SZ_LINE)
	    if (cmd[1] == EOS) {
	        call printf ("sample = %s\n")
		    call pargstr (Memc[IC_SAMPLE(ic)])
	    } else {
		call strcpy (cmd, Memc[IC_SAMPLE(ic)], SZ_LINE)
		IC_NEWX(ic) = YES
	    }

	case NAVERAGE: # :naverage - List or set the sample averging.
	    call gargi (ival)
	    if (nscan() == 1) {
		call printf ("naverage = %d\n")
		    call pargi (IC_NAVERAGE(ic))
	    } else {
		IC_NAVERAGE(ic) = ival
		IC_NEWX(ic) = YES
	    }

	case FUNCTION: # :function - List or set the fitting function.
	    call gargwrd (cmd, SZ_LINE)
	    if (nscan() == 1) {
		call printf ("function = %s\n")
		    call ic_gstr (ic, "function", cmd, SZ_LINE)
		    call pargstr (cmd)
	    } else {
		if (strdic (cmd, cmd, SZ_LINE, funcs) > 0) {
		    call ic_pstr (ic, "function", cmd)
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
		IC_ORDER(ic) = ival
		IC_NEWFUNCTION(ic) = YES
	    }

	case LOW_REJECT: # :low_reject - List or set lower rejection factor.
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("low_reject = %g\n")
		    call pargr (IC_LOW(ic))
	    } else
		IC_LOW(ic) = rval

	case HIGH_REJECT: # :high_reject - List or set high rejection factor.
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("high_reject = %g\n")
		    call pargr (IC_HIGH(ic))
	    } else
		IC_HIGH(ic) = rval

	case NITERATE: # :niterate - List or set the rejection iterations.
	    call gargi (ival)
	    if (nscan() == 1) {
		call printf ("niterate = %d\n")
		    call pargi (IC_NITERATE(ic))
	    } else
		IC_NITERATE(ic) = ival

	case GROW: # :grow - List or set the rejection growing.
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("grow = %g\n")
		    call pargr (IC_GROW(ic))
	    } else
		IC_GROW(ic) = rval

	case ERRORS: # :errors - print errors analysis of fit
	    call gargwrd (cmd, SZ_LINE)
	    if (nscan() == 1) {
		call gdeactivate (gp, AW_CLEAR)
		call ic_show (ic, "STDOUT", gt)
		call ic_errorsr (ic, "STDOUT", cv, x, y, wts, npts)
		call greactivate (gp, AW_PAUSE)
	    } else {
		iferr {
		    call ic_show (ic, cmd, gt)
		    call ic_errorsr (ic, cmd, cv, x, y, wts, npts)
		} then
		    call erract (EA_WARN)
	    }
	case VSHOW: # :vshow - Verbose list of the fitting parameters. 
	    call gargwrd (cmd, SZ_LINE)
	    if (nscan() == 1) {
		call gdeactivate (gp, AW_CLEAR)
		call ic_vshowr (ic, "STDOUT", cv, x, y, wts, npts, gt)
		call greactivate (gp, AW_PAUSE)
	    } else {
		iferr {
		    call ic_vshowr (ic, cmd, cv, x, y, wts, npts, gt)
		} then 
		    call erract (EA_WARN)
	    }
	}
end
