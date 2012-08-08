include	<error.h>
include <fset.h>		# to check whether input is redirected
include <ctype.h>
include	<tbset.h>

# T_INTEGRATE -- integrate one column of a table wrt another
# using simple trapezoid rule, ignoring INDEF values
#
# D. Giaretta, 01-Aug-1987	Original SPP version
# Phil Hodge,   7-Sep-1988	Change parameter name for table.
# Phil Hodge,  26-Jan-1996	Add option to just sum the values.
# Phil hodge,   8-Jun-1999	Set input to STDIN if redirected;
#				open the table READ_ONLY, not READ_WRITE.

procedure t_tintegrate()

char	inname[SZ_FNAME]	# input table name
char	col1[SZ_COLNAME]	# integrand
char	col2[SZ_COLNAME]	# independent var.
#--
pointer	sp
pointer	pt1, pt2, nullpt1, nullpt2
pointer	tp
pointer	colptr1, colptr2
int	i
long	numrows
long	numused
long	lastgood, firstgood	# zero indexed
double	integ
int	fstati()
int	tbpsta()
pointer tbtopn()
bool	isblank()

errchk	tbtopn, clgstr, tbpsta, clputr, clputl

begin

	call smark(sp)

	integ	  = 0.0d0
	numused   = 0
	lastgood  = -1
	firstgood = -1

	if (fstati (STDIN, F_REDIR) == YES)
	    call strcpy ("STDIN", inname, SZ_FNAME)
	else
	    call clgstr ("table", inname, SZ_FNAME)

	tp      = tbtopn( inname, READ_ONLY, 0)
	numrows = tbpsta(tp, TBL_NROWS)

	call clgstr( "integrand",   col1, SZ_COLNAME)
	call clgstr( "independent", col2, SZ_COLNAME)

	call tbcfnd( tp, col1, colptr1, 1)
	if (colptr1 == NULL)
	    call error (0, "integrand not found in table")

	if (isblank (col2)) {
	    colptr2 = NULL
	} else {
	    call tbcfnd( tp, col2, colptr2, 1)
	    if (colptr2 == NULL)
		call error (0, "independent variable not found in table")
	}

	# Get dependent variable values.
	call salloc( pt1,     numrows, TY_DOUBLE)
	call salloc( nullpt1, numrows, TY_BOOL)
	call tbcgtd(tp, colptr1, Memd[pt1], Memb[nullpt1], 1, numrows)

	# Get independent variable values.
	if (colptr2 != NULL) {
	    call salloc( pt2,     numrows, TY_DOUBLE)
	    call salloc( nullpt2, numrows, TY_BOOL)
	    call tbcgtd(tp, colptr2, Memd[pt2], Memb[nullpt2], 1, numrows)
	}

	# Find first non-INDEF row.
	if (colptr2 == NULL) {
	    do i = 0, numrows-1 {
		if (!Memb[nullpt1+i]) {
		    firstgood = i
		    break
		}
	    }
	} else {
	    do i = 0, numrows-1 {
		if ( !Memb[nullpt1+i] && !Memb[nullpt2+i] ) {
		    firstgood = i
		    break
		}
	    }
	}
	if (firstgood == -1) {
	    call tbtclo (tp)
	    call error (1, "no data in table")
	}
	lastgood = firstgood

	# apply simple trapezoid rule - 
	# ignore INDEF values, in other words linearly interpolate
	# between adjacent good values
	# note also that the independent must be non-decreasing

	if (colptr2 == NULL) {

	    # No independent variable; just sum the values.
	    numused = 0
	    do i= firstgood, numrows-1 {
	    	if ( !Memb[nullpt1+i] ) {
		    integ = integ + Memd[pt1+i]
		    lastgood = i
		    numused  = numused + 1
		}
	    }

	} else {

	    # Integrate with respect to an independent variable.
	    numused = 1
	    do i= firstgood+1, numrows-1 {
	    	if ( !Memb[nullpt1+i] && !Memb[nullpt2+i] ) {
		    if ( Memd[pt2+i] < Memd[pt2+lastgood] )
			call error(0, "independent variable not increasing")
		    integ = integ +
			    0.5*(Memd[pt1+i] + Memd[pt1+lastgood]) *
				(Memd[pt2+i] - Memd[pt2+lastgood])
		    lastgood = i
		    numused  = numused + 1
		}
	    }
	}

	# output integral both as parameter and to STDOUT
	# also record the number of good points used

	if (numused < 2) {
	    call printf (" integral = INDEF, at least 2 good points required")
	    call clputd ("integral", INDEFD)
	} else {
	    call printf (" integral= %g using %d points\n")
		call pargd (integ)
		call pargi (numused)
	    call clputd ("integral", integ)
	}

	call clputl ("ptsused", numused)

	call tbtclo (tp)

	call sfree (sp)
end
