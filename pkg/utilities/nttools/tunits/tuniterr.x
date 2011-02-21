#* HISTORY *
#* B.Simon	07-Jan-99	Original

# TUNITERR -- Print error message for tunits

procedure tuniterr (errstr, errval)

char	errstr[ARB]	# i: error message string
char	errval[ARB]	# i: value which caused error
#--
pointer	sp, errmsg

begin
	call smark (sp)
	call salloc (errmsg, SZ_LINE, TY_CHAR)

	call sprintf (Memc[errmsg], SZ_LINE, "%s (%s)")
	call pargstr (errstr)
	call pargstr (errval)

	call error (1, Memc[errmsg])

	call sfree (sp)
end
