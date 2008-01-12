# utcnum -- get column pointer from number
# This procedure returns the column pointer corresponding to a given
# column number, or zero if the column number is out of range.
#
# P.E. Hodge, 7-Aug-87  istat included in calling sequence.

procedure utcnum (tp, colnum, colptr, istat)

pointer tp		# i: Pointer to table descriptor
pointer colptr		# i: Pointer to descriptor for column or zero
int	colnum		# o: Column number (not pointer)
int	istat		# o: Return status:  -1 if colptr=0
#--
pointer tbcnum()

begin
	colptr = tbcnum (tp, colnum)
	if (colptr == NULL)
	    istat = -1
	else
	    istat = OK
end
