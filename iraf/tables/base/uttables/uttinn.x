# uttinn -- Initialize for a new table
# Create table descriptor and fill in some default values without
# actually creating the table.  The table must be opened by a separate
# call to UTTCRE.
#
# P.E. Hodge, 7-Aug-87  Delete call to uttext.
# P.E. Hodge, 16-Sep-87  Delete section to write error message.

procedure uttinn (f77nam, tp, istat)

				# i: table name
%      character*(*) f77nam
pointer tp			# o: pointer to table descriptor
int	istat			# o: return status
#--
char	tablename[SZ_FNAME]
pointer tbtopn()
int	errcode()

begin
	istat = OK

	call f77upk (f77nam, tablename, SZ_FNAME)
	if (tablename[1] == EOS) {
	    tp = NULL
	    istat = 1
	    return
	}
	iferr (tp = tbtopn (tablename, NEW_FILE, 0)) {
	    tp = NULL
	    istat = errcode()
	}
end
