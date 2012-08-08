include <tbset.h>

# NEXTUNIQ -- Retrieve the next unique row from a table

procedure nextuniq (tp, numptr, colptr, irow)

pointer tp              # i: Table descriptor
int     numptr          # i: Number of column pointers
pointer colptr[ARB]     # i: Array of column pointers
int     irow		# u: Current unique row
#--
bool	fold
int	jrow, krow, nrow

data	fold  / false /

int	tbpsta(), tbrcmp()

begin
	# Get number of rows in table

	nrow = tbpsta (tp, TBL_NROWS)

	# Loop until a row that does not match the preceding rows is found

	for (jrow = irow+1; jrow <= nrow; jrow = jrow + 1) {
	    for (krow = 1; krow < jrow; krow = krow + 1) {
		if (tbrcmp (tp, numptr, colptr, fold, jrow, krow) == 0)
		    break
	    }

	    if (krow == jrow)
		break
	}

	# Set irow to the first row that does not match any preceding row

	irow = jrow
end
