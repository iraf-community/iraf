# utrudf -- set elements to undefined
# This procedure sets one or more elements in a row to undefined.
#
# P.E. Hodge, 16-Sep-87  Delete section to write error message.

procedure utrudf (tp, colptr, numcols, rownum, istat)

pointer tp			# i: Pointer to table descriptor
pointer colptr[ARB]		# i: Array of pointers to column descriptors
int	numcols			# i: Number of column pointers in array colptr
int	rownum			# i: Row number
int	istat			# o: Return status
#--
int	errcode()

begin
	istat = OK

	iferr (call tbrudf (tp, colptr, numcols, rownum)) {
	    istat = errcode()
	}
end
