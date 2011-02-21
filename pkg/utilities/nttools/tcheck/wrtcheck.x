# WRT_CHECK -- Write the table values that pass the check

procedure wrt_check (tp, irow, keylist, command, title)

pointer	tp		#  i: Table descriptor
int	irow		#  i: Table row number
char	keylist[ARB]	#  i: List of keywords to print
char	command[ARB]	# io: Expression used in check
bool	title		# io: Print title?
#--
int	ic
pointer	sp, tabname, ldir, keyword, newcmd, value, root, col

int	fnldir(), gstrcpy(), word_fetch()

begin
	call smark (sp)
	call salloc (tabname, SZ_FNAME, TY_CHAR)
	call salloc (ldir, SZ_FNAME, TY_CHAR)
	call salloc (keyword, SZ_FNAME, TY_CHAR)
	call salloc (newcmd, SZ_FNAME, TY_CHAR)
	call salloc (value, SZ_LINE, TY_CHAR)

	# Print title if this is the first error found in this table

	if (title) {
	    title = false
	    call tbtnam (tp, Memc[tabname], SZ_FNAME)
	    root = tabname + fnldir (Memc[tabname], Memc[ldir], SZ_FNAME)

	    call printf ("#\n#%11t%-60s\n#\n")
	    call pargstr (Memc[root])
	}

	# Truncate command to 60 characters

	if (gstrcpy (command, Memc[newcmd], 60) == 60)
	    call strcat (" ...", Memc[newcmd], SZ_FNAME)

	# Print each keyword name, value, and associated command

	ic = 1
	while (word_fetch (keylist, ic, Memc[keyword], SZ_FNAME) > 0) {
	    call tbcfnd (tp, Memc[keyword], col, 1)
	    if (col != NULL) {
		call tbegtt (tp, col, irow, Memc[value], SZ_LINE)

		call printf ("%-5d%-20s%-20s%-30s\n")
		call pargi (irow)
		call pargstr (Memc[keyword])
		call pargstr (Memc[value])
		call pargstr (Memc[newcmd])

	    } else {
		call printf ("%-5d%-20s missing\n")
		call pargi (irow)
		call pargstr (Memc[keyword])
	    }
	}
	call sfree (sp)
end
