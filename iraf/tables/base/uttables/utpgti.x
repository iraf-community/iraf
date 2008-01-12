# utpgti -- get the value of a parameter
# This routine may be used to find integer-valued information about
# the table.
#
# P.E. Hodge, 14-Aug-87  Change the name from utpsta; use errcode.
# P.E. Hodge, 16-Sep-87  Delete section to write error message.

procedure utpgti (tp, param, value, istat)

pointer tp			# i: pointer to table descriptor
int	param			# i: specifies what parameter is to be gotten
int	value			# o: the value of the parameter
int	istat			# o: return status
#--
int	tbpsta(), errcode()

begin
	istat = OK

	iferr (value = tbpsta (tp, param)) {
	    istat = errcode()
	}
end
