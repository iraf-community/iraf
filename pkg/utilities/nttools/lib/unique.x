# UNIQUE -- Find unique rows in a table
#
# First, the table is sorted on columns input in the colptr array. The
# results are stored in the index array. Then each row in the index array
# is compared to the most recent unique row in the index array, column by
# column. If any column differs, the row is also considered to be unique.
# The index array is updated to reflect the new unique row and the number of
# unique rows is incremented.
#
# B.Simon	19-Oct-87	First Code
# B.Simon	14-Dec-87	Changed to handle table subsets
# B.Simon	06-Feb-90	Changed to use tbtsrt and tbrcmp

procedure unique (tp, numptr, colptr, nindex, index)

pointer	tp		#  i: Table descriptor
int	numptr		#  i: Number of column pointers
pointer	colptr[ARB]	#  i: Array of column pointers
int	nindex		# io: Number of unique row indices returned
int	index[ARB]	# io: Array of unique indices
#--
bool	fold
int	order, idx, jdx, n, i

int	tbrcmp()

begin

	# Sort the array on the selected columns. The sort is in ascending
	# order and case sensitive

	fold = false
	call tbtsrt (tp, numptr, colptr, fold, nindex, index)

	# Search for unique rows

	jdx = 0
	n = nindex
	nindex = 0

	do i = 1, n {
	    idx = index[i]

	    # First row is always unique

	    if (i == 1)
		order = 1
	    else
		order = tbrcmp (tp, numptr, colptr, fold, idx, jdx)

	    # Update pointer to most recent unique row and modify index
	    # array in place

	    if (order != 0) {
		jdx = idx
		nindex = nindex + 1
		index[nindex] = idx
	    }
	}

	do i = nindex+1, n
	    index[i] = 0

end
