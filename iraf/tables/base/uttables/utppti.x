# utppti -- set a parameter
# This routine may be used to specify such things as the record length
# or number of rows for a table.
#
# P.E. Hodge, 14-Aug-87  Change the name from utpset; use errcode.
# P.E. Hodge, 16-Sep-87  Delete section to write error message.

procedure utppti (tp, setwhat, value, istat)

pointer tp			# i: pointer to table descriptor
int	setwhat			# i: specifies what parameter is to be set
int	value			# i: the value that is to be assigned
int	istat			# o: return status
#--
int	errcode()

begin
	istat = OK

	iferr (call tbpset (tp, setwhat, value)) {
	    istat = errcode()
	}
end
