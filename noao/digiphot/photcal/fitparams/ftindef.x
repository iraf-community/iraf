include	<error.h>
include	"../lib/fitparams.h"
include	"../lib/parser.h"


# FT_INDEF - Set zero weight for all undefined input data.

procedure ft_indef (sym, otable, rtable, wtable)

int	sym			# equation symbol
pointer	otable			# 2d observation table (modified)
pointer	rtable			# 1d reference table (modified)
pointer	wtable			# 1d weight table (modified)

#bool	clgetb()

begin
	# Debug ?
	#if (clgetb ("debug.fitcode")) {
	    #call eprintf (
	        #"ft_indef: (sym=%d) (otable=%d) (rtable=%d) (wtable=%d)\n")
		#call pargi (sym)
		#call pargi (otable)
		#call pargi (rtable)
		#call pargi (wtable)
	#}

	# Check for INDEF values in reference table.
	call ft_indefr (rtable, wtable)

	# Check for INDEF values in fitting equation.
	call ft_indefo (sym, otable, wtable)
end


# FT_INDEFR - Check reference table for INDEF values. If an INDEF value is
# found, its corresponding weight (in the weight table) is set to zero, and
# the INDEF value (in the refence table) replaced by a more suitable one.
# The latter is because the INLFIT package does not handle INDEF values at
# all, and it's better to feed it with reasonable values to avoid an overflow
# or underflow condition.

procedure ft_indefr (rtable, wtable)

pointer	rtable			# reference table
pointer	wtable			# weight table (modified)

int	npts			# number of points
int	n
real	rval, replace

#bool	clgetb()
int	mct_nrows()
real	mct_getr()

begin
	# Debug ?
	#if (clgetb ("debug.fitcode")) {
	    #call eprintf ("ft_indefr: (rtable=%d) (wtable=%d)\n")
		#call pargi (rtable)
		#call pargi (wtable)
	#}

	# Get the number of points.
	npts = mct_nrows (rtable)

	# Initialize replace value to first non-INDEF value,
	# if any. Otherwise set it t zero.
	replace = 0.0
	do n = 1, npts {
	    rval = mct_getr (rtable, n, 1)
	    if (!IS_INDEFR (rval)) {
		replace = rval
		break
	    }
	}

	# Loop over all data in the table.
	do n = 1, npts {

	    # Replace values if is INDEF. Otherwise just
	    # update the replace value.
	    rval = mct_getr (rtable, n, 1)
	    if (IS_INDEFR (rval)) {
		call mct_putr (wtable, n, 1, 0.0)
		call mct_putr (rtable, n, 1, replace)
	    } else
		replace = rval
	}

	# Debug ?
	#call dg_dweights ("from ft_indefr", wtable)
	#call dg_dref ("from ft_indefr", rtable)
end


# FT_INDEFO - Check fitting equation for INDEF values. If an INDEF value is
# found, its corresponding weight (in the weight table) is set to zero.
# Undefined values in the table are set to more suitable values, so there
# won't be problems when plotting data.

procedure ft_indefo (sym, otable, wtable)

int	sym			# equation symbol
pointer	otable			# observation table (modified)
pointer	wtable			# weight table (modified)

int	i, n
int	npts			# number of points
int	nvars			# number of variables
real	rval
pointer	code			# fitting equation code
pointer	parval			# parameter values
pointer	replace			# replace values
pointer	sp

#bool	clgetb()
int	mct_nrows(), mct_maxcol()
real	mct_getr()
real	pr_eval()
pointer	mct_getrow()
pointer	pr_gsymp()

begin
	# Debug ?
	#if (clgetb ("debug.fitcode")) {
	    #call eprintf ("ft_indef: (sym=%d) (otable=%d) (wtable=%d)\n")
		#call pargi (sym)
		#call pargi (otable)
		#call pargi (wtable)
	#}

	# Get the number of variables and points.
	npts = mct_nrows (otable)
	nvars = mct_maxcol (otable)

	# Allocate space for replace values.
	call smark (sp)
	call salloc (replace, nvars, TY_REAL)

	# Initialize replace values to first non-undefined
	# value, if any. Otherwise set it t zero.
	call aclrr (Memr[replace], nvars)
	do i = 1, nvars {
	    do n = 1, npts {
		rval = mct_getr (otable, n, i)
		if (!IS_INDEFR (rval)) {
		    Memr[replace + i - 1] = rval
		    break
		}
	    }
	}

	# Get the parameter values, and equation code.
	parval = pr_gsymp (sym, PTEQSPARVAL)
	code = pr_gsymp (sym, PTEQRPNFIT)

	# Iterate over all the observations.
	do n = 1, npts {

	    # Evaluate fitting equation.
	    rval = pr_eval (code, Memr[mct_getrow (otable, n)], Memr[P2R(parval)])

	    # Substitute weight.
	    if (IS_INDEFR (rval))
		call mct_putr (wtable, n, 1, 0.0)

	    # Substitude undefined variable values.
	    do i = 1, nvars {
		rval = mct_getr (otable, n, i)
		if (IS_INDEFR (rval))
		    call mct_putr (otable, n, i, Memr[replace + i - 1])
		else
		    Memr[replace + i - 1] = rval
	    }
	}

	# Free memory.
	call sfree (sp)

	# Debug ?
	#call dg_dweights ("from ft_indefo", wtable)
	#call dg_dcatobs ("from ft_indefo", otable)
end
