# TBL_INTSORT -- Internal table sort for small tables
#
# B.Simon	21-Feb-91	First Code
# B.Simon	30-Jan-96	Close table in this routine

procedure tbl_intsort (tp, numptr, colptr, casesens, ascend)

pointer	tp		# u: Table descriptor
int	numptr		# i: Number of columns to sort on
pointer	colptr[ARB]	# i: Array of column descriptors
bool	casesens	# i: Sort is case sensitive
bool	ascend		# i: Sort in ascending order
#--
bool	fold
int	nindex, idx, jdx, temp
pointer	index

begin
	# Call the sort routine in the table library

	fold = ! casesens
	call allrows (tp, nindex, index)
	call tbtsrt (tp, numptr, colptr, fold, nindex, Memi[index])

	# Reorder the index array if ascend is false

	if (! ascend) {
	    idx = 1
	    jdx = nindex
	    while (idx < jdx) {
		temp = Memi[index+idx-1]
		Memi[index+idx-1] = Memi[index+jdx-1]
		Memi[index+jdx-1] = temp
		idx = idx + 1
		jdx = jdx - 1
	    }
	}

	# Reorder the table according to the values in the index array

	call reorder (tp, nindex, Memi[index])

	    # Close the table and free dynamic memory

	call tbtclo (tp)
	call mfree (index, TY_INT)

end
