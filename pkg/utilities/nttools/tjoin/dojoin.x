include <tbset.h>
include "tjoin.h"

# DOJOIN -- Compute the relational join of two tables
#
# B.Simon	03-Nov-87	First Code
# B.Simon	16-Dec-87	Changed to handle table subsets
# B.Simon	06-Feb-90	Changed to use tbtsrt
# B.Simon	06-Feb-90	Revised to do outer joins

procedure dojoin (tj1, tj2, tjo, tol, extra, casesens)

pointer	tj1		# i: Table info descriptor for first input table
pointer	tj2		# i: Table info descriptor for second input table
pointer	tjo		# i: Table info descriptor for output table
pointer	tol		# i: Descriptor of vector of tolerance values
int	extra		# i: Include non-joined columns in output
bool    casesens	# i: Join is case sensitive
#--
int	nrow1, nrow2, irow, jrow, krow
pointer	match1, match2

bool	is_same()
int	tbpsta() 

begin
	# Allocate arrays to hold unmatched rows 
	# in case extrarows is set

	nrow1 = tbpsta (TJ_TAB(tj1), TBL_NROWS)
	nrow2 = tbpsta (TJ_TAB(tj2), TBL_NROWS)

	if (extra > 0) {
	    call calloc (match1, nrow1, TY_INT)
	    call calloc (match2, nrow2, TY_INT)
	}

	# Naive approach to join compares every row in first table
	# to second. This is slower than sorting first (N^2 vs. N log N)
	# but the code is much simpler, especially with the extra 
	# problem of joining on row number and supporting inner and 
	# outer joins.

	krow = 1
	do irow  = 1, nrow1 {
	    do jrow = 1, nrow2 {
		# Equality test includes case insensitive string matches
		# and fuzzy matching for numbers

		if (is_same (tj1, tj2, irow, jrow, tol, casesens)) {
		    # If match, write rows to output table

		    call tbrcsc (TJ_TAB(tj1), TJ_TAB(tjo), TJ_DCOL(tj1,1),
				 TJ_DCOL(tjo,1), irow, krow, TJ_DNUM(tj1))
		    call tbrcsc (TJ_TAB(tj2), TJ_TAB(tjo), TJ_DCOL(tj2,1),
				 TJ_DCOL(tjo,TJ_DNUM(tj1)+1), jrow, krow, 
				 TJ_DNUM(tj2))

		    if (extra > 0) {
			Memi[match1+irow-1] = jrow
			Memi[match2+jrow-1] = irow
		    }

		    krow = krow + 1
		}
	    }
	}

	# Write the extra rows to the output table

	if (extra >= 1) {
	    do irow = 1, nrow1 {
		if (Memi[match1+irow-1] == 0) {
		    call tbrcsc (TJ_TAB(tj1), TJ_TAB(tjo), TJ_DCOL(tj1,1),
				 TJ_DCOL(tjo,1), irow, krow, TJ_DNUM(tj1))
		    krow = krow + 1
		}
	    }
	}

	if (extra == 2) {
	    do jrow = 1, nrow2 {
		if (Memi[match2+jrow-1] == 0) {
		    call tbrcsc (TJ_TAB(tj2), TJ_TAB(tjo), TJ_DCOL(tj2,1),
				 TJ_DCOL(tjo,TJ_DNUM(tj1)+1), jrow, krow, 
				 TJ_DNUM(tj2))
		    krow = krow + 1
		}
	    }
	}

	if (extra > 0) {
	    call mfree (match1, TY_INT)
	    call mfree (match2, TY_INT)
	}

end
