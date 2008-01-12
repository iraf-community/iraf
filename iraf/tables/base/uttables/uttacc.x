# uttacc -- check whether a table exists
# This procedure determines whether the specified table exists.  The
# default extension will be appended if no extension is present.
#
# P.E. Hodge, 7-Aug-87  Subroutine created
# P.E. Hodge, 7-Sep-87  Delete declaration of flen.
# P.E. Hodge, 16-Sep-87  Delete section to write error message.

procedure uttacc (f77nam, table_exists, istat)

				# i: the table name
%      character*(*) f77nam
bool	table_exists		# o: true if the table exists
int	istat			# o: Return status
#--
pointer tname			# scratch for the table name as an SPP string
pointer sp
int	tbtacc(), errcode()

begin
	istat = OK

	call smark (sp)
	call salloc (tname, SZ_FNAME, TY_CHAR)

	call f77upk (f77nam, Memc[tname], SZ_FNAME)
	if (Memc[tname] == EOS) {
	    istat = 1
	    table_exists = false
	    call sfree (sp)
	    return
	}
	iferr (table_exists = (tbtacc (Memc[tname]) == YES)) {
	    istat = errcode()
	}
	call sfree (sp)
end
