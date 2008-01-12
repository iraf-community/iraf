# uttclo -- close a table
# The files themselves are closed, and memory that was allocated for
# descriptors is released.
#
# P.E. Hodge, 16-Sep-87  Delete section to write error message.

procedure uttclo (tp, istat)

pointer tp			# i: Pointer to descriptor of table to be closed
int	istat			# o: Return status
#--
int	errcode()

begin
	istat = OK

	iferr (call tbtclo (tp)) {
	    istat = errcode()
	}
end
