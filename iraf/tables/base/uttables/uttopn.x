# uttopn -- open an existing table
# Create table descriptor and fill in some default values, and then open
# the table.
#
# P.E. Hodge, 7-Aug-87  I/O mode changed from character string to integer.
# P.E. Hodge, 8-Sep-87  Delete declaration of streq().
# P.E. Hodge, 16-Sep-87  Delete section to write error message.

procedure uttopn (f77nam, iomode, tp, istat)

				# i: table name
%      character*(*) f77nam
pointer tp			# o: pointer to table descriptor
int	iomode			# i: READ_ONLY, READ_WRITE, or WRITE_ONLY
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
	iferr (tp = tbtopn (tablename, iomode, 0)) {
	    tp = NULL
	    istat = errcode()
	}
end
