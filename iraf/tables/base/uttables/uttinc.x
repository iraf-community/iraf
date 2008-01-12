# uttinc -- Initialize for a copy of a table
# Create table descriptor and fill in some default values without
# actually creating the table.  A template table is used to define
# columns; other columns may be defined after calling this routine.
# The table must be opened by a separate call to UTTCRE.
#
# P.E. Hodge, 7-Aug-87  Delete call to uttext.
# P.E. Hodge, 16-Sep-87  Delete section to write error message.

procedure uttinc (f77nam, template, tp, istat)

				# i: table name
%      character*(*) f77nam
pointer template		# i: pointer to descriptor for template table
pointer tp			# o: pointer to table descriptor
int	istat			# o: return status
#--
char	tablename[SZ_FNAME]	# table name as an SPP string
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
	if (template == NULL) {
	    tp = NULL
	    istat = 1
	    return
	}

	iferr (tp = tbtopn (tablename, NEW_COPY, template)) {
	    tp = NULL
	    istat = errcode()
	}
end
