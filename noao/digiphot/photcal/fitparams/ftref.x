include	<error.h>
include	"../lib/parser.h"


# FT_RFEVAL - Evaluate reference equation (left hand side) for all observations
# and store them into a one column table, called the reference table (rtable).

procedure ft_rfeval (code, otable, rtable)

pointer	code			# equation code
pointer	otable			# 2d observation table
pointer	rtable			# 1d reference table (output)

int	n
real	rval
real	dummy[1]

#bool	clgetb()
int	mct_nrows()
real	pr_eval()
pointer	mct_getrow()

begin
	# Debug ?
	#if (clgetb ("debug.fitcode")) {
	    #call eprintf ("ft_rfeval (code=%d) (otable=%d)\n")
		#call pargi (code)
		#call pargi (otable)
	#}

	# Allocate space for reference table.
	# call mct_alloc (rtable, mct_nrows (otable), 1, TY_REAL)

	# Loop over all data in the table.
	do n = 1, mct_nrows (otable) {

	    # Evaluate the equation.
	    iferr (rval = pr_eval (code, Memr[mct_getrow (otable, n)], dummy)) {
		call eprintf ("ft_ref (%d)\n")
		    call pargi (n)
		call erract (EA_ERROR)
	    }

	    # Put data into reference table.
	    call mct_putr (rtable, n, 1, rval)
	}

	# Debug ?
	#call dg_dref ("from ft_rfeval", rtable)
end
