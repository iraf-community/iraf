# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

define	MAX_FILES	50

# COLUMNS -- Convert a multicolumn file into a series of single column files.
# One output file `columns.n' is produced for each column in the input file.
#
#	usage: COLUMNS  input_file  ncols
#
# This routine was originally written to allow SDAS to treat multicolumn
# tables as simple CL lists.  Each column in the table is referenced in
# SDAS by a different parameter, pointing in the .par file to 
# a different list. 

procedure t_columns()

int	numcols, infd, nchar, nfile, ip
pointer	sp, fname, ofile, oroot, line, word, fnum, ofd
size_t	sz_val

int	clgeti(), open(), getline(), itoc(), ctowrd()

begin
	call smark (sp)
	sz_val = SZ_FNAME
	call salloc (fname, sz_val, TY_CHAR)
	call salloc (ofile, sz_val, TY_CHAR)
	call salloc (oroot, sz_val, TY_CHAR)
	sz_val = SZ_LINE
	call salloc (line, sz_val, TY_CHAR)
	call salloc (word, sz_val, TY_CHAR)
	sz_val = SZ_FNAME
	call salloc (fnum, sz_val, TY_CHAR)
	sz_val = MAX_FILES
	call salloc (ofd, sz_val, TY_INT)

	# Get the number of columns and the input file name
	call clgstr ("filename", Memc[fname], SZ_FNAME)
	numcols = clgeti ("numcols")
	if (numcols > MAX_FILES)
	    call error (1, "too many files (columns)")
	call clgstr ("outroot", Memc[oroot], SZ_FNAME)

	# Open all the files
	infd = open (Memc[fname], READ_ONLY, TEXT_FILE)
	for (nfile=1;  nfile <= numcols;  nfile=nfile+1)  {
	    nchar = itoc (nfile, Memc[fnum], 2)
	    call strcpy (Memc[oroot], Memc[ofile], SZ_FNAME)
	    call strcat (Memc[fnum], Memc[ofile], SZ_FNAME)
	    Memi[ofd+nfile-1] = open (Memc[ofile], NEW_FILE, TEXT_FILE)
	}

	# Separate each line of the input file
	while (getline (infd, Memc[line]) != EOF) {
	    if ((Memc[line] != '#') && (Memc[line] != '\n'))  {
		ip = 1
		for (nfile=1;  nfile <= numcols;  nfile=nfile+1)  {
		    nchar = ctowrd (Memc[line], ip, Memc[word], SZ_LINE)
		    call strcat ("\n", Memc[word], SZ_LINE)
		    call putline (Memi[ofd+nfile-1], Memc[word])
		}
	    }
	}

	# Close the files.
	call close (infd)
	for (nfile=1;  nfile <= numcols;  nfile=nfile+1)
	    call close (Memi[ofd+nfile-1])

	call sfree (sp)
end
