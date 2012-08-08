# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<error.h>

# COPY -- Copy a file, or copy a list of files to another directory.
# The number of input files is variable, but there can be only a single
# output file.  If the output file is a directory, we copy the input files
# to that directory without changing their names.  Otherwise there can
# be only one input file, and a copy is made with the new name.

procedure t_copy()

int	list, root_len
bool	verbose
pointer	sp, infile, destination, outfile, junkstr, dirname
bool	clgetb()
int	clpopni(), clgfil(), clplen(), fnldir(), isdirectory()

begin
	call smark (sp)
	call salloc (infile, SZ_FNAME, TY_CHAR)
	call salloc (outfile, SZ_FNAME, TY_CHAR)
	call salloc (destination, SZ_FNAME, TY_CHAR)
	call salloc (dirname, SZ_PATHNAME, TY_CHAR)
	call salloc (junkstr, SZ_FNAME, TY_CHAR)

	list = clpopni ("input")
	call clgstr ("output", Memc[destination], SZ_FNAME)
	verbose = clgetb ("verbose")

	# If the destination file is a directory, copy each file in the input
	# list to the destination directory, else just copy the file and check
	# that there is only one file in the input list.

	if (isdirectory (Memc[destination], Memc[dirname], SZ_PATHNAME) > 0) {

	    while (clgfil (list, Memc[infile], SZ_FNAME) != EOF) {
		call strcpy (Memc[dirname], Memc[outfile], SZ_PATHNAME)
		root_len = fnldir (Memc[infile], Memc[junkstr], SZ_FNAME)
		call strcat (Memc[infile+root_len], Memc[outfile], SZ_PATHNAME)

		if (verbose) {
		    call eprintf ("%s -> %s\n")
			call pargstr (Memc[infile])
			call pargstr (Memc[outfile])
		}

		iferr (call fcopy (Memc[infile], Memc[outfile]))
		    call erract (EA_WARN)
	    }

	} else if (clgfil (list, Memc[infile], SZ_FNAME) != EOF) {
	    if (clplen (list) > 1) {
		call clpcls (list)
		call error (2, "cannot copy several files to a single file")
	    }
	    call fcopy (Memc[infile], Memc[destination])
	}

	call clpcls (list)
	call sfree (sp)
end
