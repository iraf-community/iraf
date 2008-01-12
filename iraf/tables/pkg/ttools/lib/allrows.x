include	<tbset.h>

# ALLROWS -- Return a pointer to an array containing the indices of all
# the rows in a table. The calling procedure must free the array when it
# is through with it.
#
# B.Simon	11-Dec-87	First Code

procedure allrows (tp, numrow, rowptr)

pointer	tp		# i: Table descriptor
int	numrow		# o: Number of rows in the table
pointer	rowptr		# o: Pointer to array of indices

int	irow

int	tbpsta()

errchk	tbpsta, malloc

begin
	
	numrow = tbpsta (tp, TBL_NROWS)
	call malloc (rowptr, numrow, TY_INT)

	do irow = 1, numrow
		Memi[rowptr+irow-1] = irow

end
