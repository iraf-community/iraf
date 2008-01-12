# uttdel -- delete a table
# This is a simple file-delete routine.  If the extension is absent from
# the table name, the default extension will be appended.
#
# P.E. Hodge, 16-Sep-87  Delete section to write error message.

procedure uttdel (f77nam, istat)

				# i: name of table to be deleted
%      character*(*) f77nam
int	istat			# o: return status
#--
pointer tname			# scratch for table name
pointer sp
int	errcode()

begin
	istat = OK

	call smark (sp)
	call salloc (tname, SZ_FNAME, TY_CHAR)
	call f77upk (f77nam, Memc[tname], SZ_FNAME)

	iferr (call tbtdel (Memc[tname])) {
	    istat = errcode()
	}
	call sfree (sp)
end
