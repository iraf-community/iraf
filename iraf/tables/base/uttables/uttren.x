# uttren -- rename a table
# This is a simple file-rename routine.  If the extension is absent from
# either the input or output table name, the default extension will be
# appended.
#
# P.E. Hodge, 16-Sep-87  Delete section to write error message.

procedure uttren (f77in, f77out, istat)

				# i: old table name
%      character*(*) f77in
				# i: new table name
%      character*(*) f77out
int	istat			# o: return status
#--
pointer in, out			# scratch for input & output table names
pointer sp
int	errcode()

begin
	istat = OK

	call smark (sp)
	call salloc (in, SZ_FNAME, TY_CHAR)
	call salloc (out, SZ_FNAME, TY_CHAR)
	call f77upk (f77in, Memc[in], SZ_FNAME)
	call f77upk (f77out, Memc[out], SZ_FNAME)

	iferr (call tbtren (Memc[in], Memc[out])) {
	    istat = errcode()
	}
	call sfree (sp)
end
