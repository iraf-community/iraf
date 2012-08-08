include "tjoin.h"

# B.Simon	16-Apr-99	first code

# REMOVE_JCOL -- Remove join columns from list of data columns

procedure remove_jcol (tj, tol)

pointer	tj		# i: Descriptor of table information
pointer	tol		# i: Vector of tolerances used in equality test
#--
bool	match
int	icol, jcol, kcol

begin
	kcol = 0
	do icol = 1, TJ_DNUM(tj) {
	    # Determine if this column is a join column
	    # with strict equality testing

	    match = false
	    do jcol = 1, TJ_JNUM(tj) {
		if (TJ_DCOL(tj,icol) == TJ_JCOL(tj,jcol) &&
		    TOL_VAL(tol,jcol) == 0.0) {
		    match = true
		    break
		}
	    }

	    # Don't copy these columns as they duplicate the values
	    # in the join column in the other table. Also don't copy 
	    # if icol == kcol in order to save time

	    if (! match) {
		kcol = kcol + 1
		if (kcol < icol)
		    TJ_DCOL(tj,kcol) = TJ_DCOL(tj,icol)
	    }
	}

	TJ_DNUM(tj) = kcol
end

