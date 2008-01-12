
include <tbset.h>

# INVERT -- Create the complement (inverse) of an array of column pointers
#
# B.Simon	20-Oct-87	First Code

procedure invert (tp, numptr, colptr)

pointer	tp		# i:  Table descriptor
int	numptr		# io: Number of column pointers
pointer	colptr[ARB]	# io: Array of column pointers

bool	match
int	numcol, icol, iptr, jptr
pointer newptr, cp

int	tbpsta(), tbcnum()

begin
	# Create a temporary array to hold the pointers

	numcol = tbpsta (tp, TBL_NCOLS)
	call malloc (newptr, numcol, TY_INT)

	jptr = 0
	do icol = 1, numcol {

	    # Get each pointer in the table and 
	    # see if it is in the original array

	    cp = tbcnum (tp, icol)
	    match = false
	    do iptr = 1, numptr {
		if (cp == colptr[iptr]) {
		    match = true
		    break
		}
	    }

	    # If not, add it to the temporary array

	    if (! match) {
		Memi[newptr+jptr] =  cp
		jptr = jptr + 1
	    }
	}

#	Copy the temporary array to the output array

	numptr = jptr
	call amovi (Memi[newptr], colptr, numptr)
	call mfree (newptr, TY_INT)

end
