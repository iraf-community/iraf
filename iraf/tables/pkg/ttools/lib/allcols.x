include	<tbset.h>

# ALLCOLS -- Return a pointer to an array containing the indices of all
# the columns in a table. The calling procedure must free the array when it
# is through with it.
#
# B.Simon	11-Dec-87	First Code

procedure allcolumns (tp, numcol, colptr)

pointer	tp		# i: Table descriptor
int	numcol		# o: Number of columns in the table
pointer	colptr		# o: Pointer to array of indices

int	icol

int	tbpsta()
pointer	tbcnum()

errchk	tbpsta, malloc

begin
	
	numcol = tbpsta (tp, TBL_NCOLS)
	call malloc (colptr, numcol, TY_POINTER)

	do icol = 1, numcol
		Memp[colptr+icol-1] = tbcnum (tp, icol)

end
