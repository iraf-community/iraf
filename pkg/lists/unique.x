# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# UNIQUE -- Pass only unique lines from the (presumably sorted) standard
# input to the standard output.  In other words, if a sequence of identical
# lines are found in the input, only one copy is passed to the output.

procedure t_unique()

int	list, fd
pointer	sp, fname, old_line, new_line, temp
bool	streq()
int	getline(), clpopni(), clgfil(), clplen(), open()

begin
	call smark (sp)
	call salloc (fname, SZ_FNAME, TY_CHAR)
	call salloc (old_line, SZ_LINE, TY_CHAR)
	call salloc (new_line, SZ_LINE, TY_CHAR)

	list = clpopni ("files")

	while (clgfil (list, Memc[fname], SZ_FNAME) != EOF) {
	    fd = open (Memc[fname], READ_ONLY, TEXT_FILE)
	    if (clplen (list) > 1) {
		call printf ("\n\n==> %s <==\n")
		    call pargstr (Memc[fname])
	    }

	    Memc[old_line] = EOS

	    while (getline (fd, Memc[new_line]) != EOF) {
		if (streq (Memc[old_line], Memc[new_line]))
		    next
		call putline (STDOUT, Memc[new_line])

		# Swap buffers.
		temp = old_line
		old_line = new_line
		new_line = temp
	    }

	    call close (fd)
	}

	call sfree (sp)
end
