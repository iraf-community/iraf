# TBL_SORT -- Sort a table on selected table columns
#
# B.Simon	06-Fab-90	First Code

procedure tbl_sort (ascend, casesens, tp, numptr, colptr, nindex, index)

bool	ascend		#  i: Sort in ascending order
bool	casesens	#  i: Sort is case sensitive
pointer	tp		#  i: Table descriptor
int	numptr		#  i: Number of columns to sort on
pointer	colptr[ARB]	#  i: Array of column descriptors
int	nindex		#  i: Number of elements in index array
int	index[ARB]	# io: Array of row indices to sort
#--
bool	fold
int	idx, jdx, temp

begin
	# Call the sort routine in the table library

	fold = ! casesens
	call tbtsrt (tp, numptr, colptr, fold, nindex, index)

	# Reorder the index array if ascend is false

	if (! ascend) {
	    idx = 1
	    jdx = nindex
	    while (idx < jdx) {
		temp = index[idx]
		index[idx] = index[jdx]
		index[jdx] = temp
		idx = idx + 1
		jdx = jdx - 1
	    }
	}


end
