include	<error.h>
include	<gset.h>
include	"hdicfit.h"

define	EB_WTS	10
define	EB_SDEV	11

# List of colon commands.
define	CMDS "|show|sample|naverage|function|order|low_reject|high_reject|\
              |niterate|grow|errors|vshow|transform|fog|reset|quit|ebars|"

define	SHOW		1	# Show values of parameters
define	SAMPLE		2	# Set or show sample ranges
define	NAVERAGE	3	# Set or show sample averaging or medianing
define	FUNCTION	4	# Set or show function type
define	ORDER		5	# Set or show order
define	LOW_REJECT	6	# Set or show lower rejection factor
define	HIGH_REJECT	7	# Set or show upper rejection factor
# newline		8
define	NITERATE	9	# Set or show rejection iterations
define	GROW		10	# Set or show rejection growing radius
define	ERRORS		11	# Show errors of fit
define	VSHOW		12	# Show verbose information
define	TRANSFORM	13	# Set or show transformation
define	FOG		14	# Set or show value of fog
define	RESET		15	# Reset x, y, wts, npts to original values
define	QUIT		16	# Terminate without updating database
define	EBARS		17	# Set error bars to represent weights or
				#     standard deviations

# ICG_COLON -- Processes colon commands.  

procedure icg_colond (ic, cmdstr, gp, gt, cv, x, y, wts, npts)

pointer	ic				# ICFIT pointer
char	cmdstr[ARB]			# Command string
pointer	gp				# GIO pointer
pointer	gt				# GTOOLS pointer
pointer	cv				# CURFIT pointer for error listing
double	x[npts], y[npts], wts[npts]	# Data arrays for error listing
int	npts				# Number of data points

real	rval
char	cmd[SZ_LINE]
int	ncmd, ival, ip, junk

int	nscan(), strdic(), strncmp(), ctor()
string	funcs "|chebyshev|legendre|spline1|spline3|power|"
string	tform "|none|logopacitance|k50|k75|"

begin
	# Use formated scan to parse the command string.
	# The first word is the command and it may be minimum match
	# abbreviated with the list of commands.

	call sscan (cmdstr)
	call gargwrd (cmd, SZ_LINE)
	ncmd = strdic (cmd, cmd, SZ_LINE, CMDS)

	switch (ncmd) {
	case SHOW:
	    # show: Show the values of the fitting parameters.  The terminal
	    # is cleared and paged using the gtools paging procedures.

	    call gargwrd (cmd, SZ_LINE)
	    if (nscan() == 1) {
	        call gdeactivate (gp, AW_CLEAR)
		call ic_show (ic, "STDOUT", gt)
	        call greactivate (gp, AW_PAUSE)
	    } else {
		iferr (call ic_show (ic, cmd, gt))
		    call erract (EA_WARN)
	    }

	case SAMPLE:
	    # sample: List or set the sample points.

	    call gargwrd (cmd, SZ_LINE)
	    if (cmd[1] == EOS) {
	        call printf ("sample = %s\n")
		    call pargstr (Memc[IC_SAMPLE(ic)])
	    } else {
		call strcpy (cmd, Memc[IC_SAMPLE(ic)], SZ_LINE)
		IC_NEWX(ic) = YES
	    }

	case NAVERAGE:
	    # naverage: List or set the sample averging.

	    call gargi (ival)
	    if (nscan() == 1) {
		call printf ("naverage = %d\n")
		    call pargi (IC_NAVERAGE(ic))
	    } else {
		IC_NAVERAGE(ic) = ival
		IC_NEWX(ic) = YES
	    }

	case FUNCTION:
	    # function: List or set the fitting function.

	    call gargwrd (cmd, SZ_LINE)
	    if (cmd[1] == EOS) {
		call printf ("function = %s\n")
		    call ic_gstr (ic, "function", cmd, SZ_LINE)
		    call pargstr (cmd)
	    } else {
		if (strdic (cmd, cmd, SZ_LINE, funcs) > 0) {
		    call ic_pstr (ic, "function", cmd)
		    IC_NEWFUNCTION(ic) = YES
		} else
		    call printf ("Unknown or ambiguous function")
	    }

	case ORDER:
	    # order: List or set the function order.

	    call gargi (ival)
	    if (nscan() == 1) {
		call printf ("order = %d\n")
		    call pargi (IC_ORDER(ic))
	    } else {
		IC_ORDER(ic) = ival
		IC_NEWFUNCTION(ic) = YES
	    }

	case LOW_REJECT:
	    # low_reject: List or set the lower rejection threshold.

	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("low_reject = %g\n")
		    call pargr (IC_LOW(ic))
	    } else
		IC_LOW(ic) = rval

	case HIGH_REJECT:
	    # high_reject: List or set the high rejection threshold.

	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("high_reject = %g\n")
		    call pargr (IC_HIGH(ic))
	    } else
		IC_HIGH(ic) = rval

	case NITERATE:
	    # niterate: List or set the number of rejection iterations.

	    call gargi (ival)
	    if (nscan() == 1) {
		call printf ("niterate = %d\n")
		    call pargi (IC_NITERATE(ic))
	    } else
		IC_NITERATE(ic) = ival

	case GROW:
	    # grow: List or set the rejection growing.

	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("grow = %g\n")
		    call pargr (IC_GROW(ic))
	    } else
		IC_GROW(ic) = rval

	case ERRORS:
	    call gargwrd (cmd, SZ_LINE)
	    if (nscan() == 1) {
	        call gdeactivate (gp, AW_CLEAR)
		call ic_show (ic, "STDOUT", gt)
		call ic_errorsd (ic, "STDOUT", cv, x, y, wts, npts)
	        call greactivate (gp, AW_PAUSE)
	    } else {
		iferr {
		    call ic_show (ic, cmd, gt)
		    call ic_errorsd (ic, cmd, cv, x, y, wts, npts)
		} then
		    call erract (EA_WARN)
	    }
	case VSHOW:
	    # verbose show:  Show the values of the fitting parameters. 
	    # The terminal is paged using the gtools paging procedure. 

	    call gargwrd (cmd, SZ_LINE)
	    if (cmd[1] == EOS) {
		call gdeactivate (gp, AW_CLEAR)
		call ic_vshowd (ic, "STDOUT", cv, x, y, wts, npts, gt)
		call greactivate (gp, AW_PAUSE)
	    } else {
		iferr {
		    call ic_vshowd (ic, cmd, cv, x, y, wts, npts, gt)
		} then 
		    call erract (EA_WARN)
	    }
	case TRANSFORM:
	    # transform: List or set the transformation type.  This
	    # option applies to HDTOI procedures only.

	    call gargwrd (cmd, SZ_LINE)
	    if (cmd[1] == EOS) {
		call printf ("transform = %s\n")
		    call ic_gstr (ic, "transform", cmd, SZ_LINE)
		    call pargstr (cmd)
	    } else {
		ival= strdic (cmd, cmd, SZ_LINE, tform) 
		if (ival > 0) {
		    call ic_pstr (ic, "transform", cmd)
		    IC_NEWTRANSFORM(ic) = YES
		    IC_NEWX(ic) = YES
		    switch (IC_TRANSFORM(ic)) {
		    case HD_NONE:
	    		call ic_pstr (ic, "xlabel", "Density")
		    case HD_LOGO:
	    		call ic_pstr (ic, "xlabel", 
			    "Log Opacitance: log (10**Den - 1)")
		    case HD_K50:
	    		call ic_pstr (ic, "xlabel", 
		           "Den + 0.50 * Log (1 - (10 ** -Den))")
		    case HD_K75:
	    		call ic_pstr (ic, "xlabel", 
			    "Den + 0.75 * Log (1 - (10 ** -Den))")
		    }
		} else
		    call printf ("Unknown or ambiguous transform")
	    }

	case FOG:
	    # fog: DTOI ONLY - change or reset the value of the fog level

	    call gargwrd (cmd, SZ_LINE)
	    if (cmd[1] == EOS) {
		call printf ("fog = %g\n")
		    call pargr (IC_FOG(ic))
	    } else {
		if (strncmp (cmd, "reset", 1) == 0)
		    IC_FOG(ic) = IC_RFOG(ic)
		else {
		    ip = 1
		    junk = ctor (cmd, ip, rval)
		    IC_FOG(ic) = rval
		}
		IC_NEWFOG(ic) = YES
		IC_NEWX(ic) = YES
	    }

	case RESET:
	    # Set flag to reset x, y, wts and npts to original values.
	    IC_RESET(ic) = YES
	    IC_NEWX(ic) = YES
	    IC_NEWY(ic) = YES
	    IC_NEWWTS(ic) = YES
	    IC_NEWFUNCTION(ic) = YES
	    IC_NEWTRANSFORM(ic) = YES

	case QUIT:
	    # Set update flag to know
	    IC_UPDATE(ic) = NO

	case EBARS:
	    # [HV]BAR marker can indicate either errors or weights
	    call gargwrd (cmd, SZ_LINE)
	    if (cmd[1] == EOS) {
		if (IC_EBARS(ic) == EB_WTS)
		    call printf ("ebars = Weights\n")
		else if (IC_EBARS(ic) == EB_SDEV)
		    call printf ("ebars = Errors\n")
	    } else {
		if (strncmp (cmd, "weights", 1) == 0 || 
		    strncmp (cmd, "WEIGHTS", 1) == 0)
		    IC_EBARS(ic) = EB_WTS
		else if (strncmp (cmd, "errors", 1) == 0 || 
		    strncmp (cmd, "ERRORS", 1) == 0)
		    IC_EBARS(ic) = EB_SDEV
		else
		    call printf ("Unrecognized value for ebars '%s'\n")
			call pargstr (cmd)
	    }

	default:
	    call eprintf ("Unrecognized command '%s'\n")
		call pargstr (cmd)
	}
end
