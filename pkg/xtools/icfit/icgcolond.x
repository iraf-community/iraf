# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gset.h>
include	<error.h>
include	"icfit.h"
include	"names.h"

# List of colon commands.
define	CMDS "|show|sample|naverage|function|order|low_reject|high_reject\
	|niterate|grow|markrej|errors|vshow|xyshow|"

define	SHOW		1	# Show values of parameters
define	SAMPLE		2	# Set or show sample ranges
define	NAVERAGE	3	# Set or show sample averaging or medianing
define	FUNCTION	4	# Set or show function type
define	ORDER		5	# Set or show function order
define	LOW_REJECT	6	# Set or show lower rejection factor
define	HIGH_REJECT	7	# Set or show upper rejection factor
define	NITERATE	8	# Set or show rejection iterations
define	GROW		9	# Set or show rejection growing radius
define	MARKREJ		10	# Mark rejected points
define	ERRORS		11	# Show errors of fit
define	VSHOW		12	# Show verbose information
define	XYSHOW		13	# Show x-y-fit values

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

bool	bval
int	ncmd, ival
real	rval
pointer	sp, cmd

int	nscan(), strdic(), btoi()

string	funcs "|chebyshev|legendre|spline1|spline3|power|"

begin
	# Use formated scan to parse the command string.
	# The first word is the command and it may be minimum match
	# abbreviated with the list of commands.

	call smark (sp)
	call salloc (cmd, IC_SZSAMPLE, TY_CHAR)

	call sscan (cmdstr)
	call gargwrd (Memc[cmd], IC_SZSAMPLE)
	ncmd = strdic (Memc[cmd], Memc[cmd], IC_SZSAMPLE, CMDS)

	switch (ncmd) {
	case SHOW: # :show - Show the values of the fitting parameters.
	    call gargwrd (Memc[cmd], IC_SZSAMPLE)
	    if (nscan() == 1) {
		call gdeactivate (gp, AW_CLEAR)
		call ic_show (ic, "STDOUT", gt)
		call greactivate (gp, AW_PAUSE)
	    } else {
		iferr (call ic_show (ic, Memc[cmd], gt))
		    call erract (EA_WARN)
	    }

	case SAMPLE: # :sample - List or set the sample points.
	    call gargstr (Memc[cmd], IC_SZSAMPLE)
	    if (Memc[cmd] == EOS) {
	        call printf ("sample = %s\n")
		    call pargstr (Memc[IC_SAMPLE(ic)])
	    } else {
		call strcpy (Memc[cmd], Memc[IC_SAMPLE(ic)], IC_SZSAMPLE)
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

	case MARKREJ: # :markrej - Mark rejected points
	    call gargb (bval)
	    if (nscan() == 1) {
		call printf ("markrej = %b\n")
		    call pargi (IC_MARKREJ(ic))
	    } else {
		IC_MARKREJ(ic) = btoi (bval)
	    }

	case ERRORS: # :errors - print errors analysis of fit
	    call gargwrd (Memc[cmd], IC_SZSAMPLE)
	    if (nscan() == 1) {
		call gdeactivate (gp, AW_CLEAR)
		call ic_show (ic, "STDOUT", gt)
		call ic_errorsd (ic, "STDOUT", cv, x, y, wts, npts)
		call greactivate (gp, AW_PAUSE)
	    } else {
		iferr {
		    call ic_show (ic, Memc[cmd], gt)
		    call ic_errorsd (ic, Memc[cmd], cv, x, y, wts, npts)
		} then
		    call erract (EA_WARN)
	    }
	case VSHOW: # :vshow - Verbose list of the fitting parameters. 
	    call gargwrd (Memc[cmd], IC_SZSAMPLE)
	    if (nscan() == 1) {
		call gdeactivate (gp, AW_CLEAR)
		call ic_vshowd (ic, "STDOUT", cv, x, y, wts, npts, gt)
		call greactivate (gp, AW_PAUSE)
	    } else {
		iferr {
		    call ic_vshowd (ic, Memc[cmd], cv, x, y, wts, npts, gt)
		} then 
		    call erract (EA_WARN)
	    }
	case XYSHOW: # :xyshow - Show data points and fit.
	    call gargwrd (Memc[cmd], IC_SZSAMPLE)
	    if (nscan() == 1) {
		call gdeactivate (gp, AW_CLEAR)
		call ic_xyshowd ("STDOUT", cv, x, y, npts)
		call greactivate (gp, AW_PAUSE)
	    } else {
		iferr {
		    call ic_xyshowd (Memc[cmd], cv, x, y, npts)
		} then 
		    call erract (EA_WARN)
	    }
	default: # Unrecognized command.
	    call printf ("Unrecognized command or abiguous abbreviation\007")
	}

	call sfree (sp)
end
