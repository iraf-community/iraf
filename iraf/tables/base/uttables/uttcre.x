# uttcre -- open a (new) table
# This is the routine for opening a new table after calling one of the
# two initialization routines.  The table will be created, and size
# information and descriptions of whatever columns have already been
# defined will be written into the table.
#
# P.E. Hodge, 16-Sep-87  Delete section to write error message.

procedure uttcre (tp, istat)

pointer tp			# i: pointer to table descriptor
int	istat			# o: return status
#--
int	errcode()

begin
	istat = OK

	iferr (call tbtcre (tp)) {
	    istat = errcode()
	}
end
