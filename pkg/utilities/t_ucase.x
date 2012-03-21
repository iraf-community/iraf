# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# UCASE -- Convert the standard input or a list of files to upper case.

procedure t_ucase()

pointer sp, line, in_file, out_file
int	list, in, out
bool	strne()
int	open(), clpopni(), clgfil(), getline(), clplen()

begin
	call smark (sp)
	call salloc (line, SZ_LINE, TY_CHAR)
	call salloc (in_file, SZ_FNAME, TY_CHAR)
	call salloc (out_file, SZ_FNAME, TY_CHAR)

	list = clpopni ("files")

	# Multiple files are converted to files of the same name with the
	# extension ".uc".

	while (clgfil (list, Memc[in_file], SZ_FNAME) != EOF) {
	    in = open (Memc[in_file], READ_ONLY, TEXT_FILE)
	    if (clplen (list) > 1 && strne (Memc[in_file], "STDIN")) {
		call strcpy (Memc[in_file], Memc[out_file], SZ_FNAME)
		call strcat (".uc", Memc[out_file], SZ_FNAME)
	    } else
		call strcpy ("STDOUT", Memc[out_file], SZ_FNAME)
	    out = open (Memc[out_file], NEW_FILE, TEXT_FILE)

	    while (getline (in, Memc[line]) != EOF) {
		call strupr (Memc[line])
		call putline (out, Memc[line])
	    }
	    call close (in)
	    call close (out)
	}

	call clpcls (list)
	call sfree (sp)
end
