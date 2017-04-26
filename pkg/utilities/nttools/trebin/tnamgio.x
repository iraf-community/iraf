# tnam_gio -- get input & output names
# Get the next input and output table names.
#
# Phil Hodge, 15-Apr-1988  Task created.
# Phil Hodge,  4-Oct-1995  Modify to use tbn instead of fnt; use tbparse.
# Phil hodge, 16-Apr-1999  Remove ttype from calling sequence of tbparse.
# Phil Hodge, 25-Apr-2000  Add xin_t, xtable, maxch to calling sequence,
#			and remove dir_only.

int procedure tnam_gio (in_t, xin_t, out_t, outdir,
			intable, xtable, outtable, maxch)

pointer in_t			# i: fnt pointer for input tables
pointer xin_t			# i: fnt pointer for tables of output X
pointer out_t			# i: fnt pointer for output tables
char	outdir[ARB]		# i: name of output directory
char	intable[ARB]		# o: name of next input table
char	xtable[ARB]		# o: name of next table for output X
char	outtable[ARB]		# o: name of next output table
int	maxch			# i: size of table name strings
#--
pointer sp
pointer filename		# name of table file, without brackets
pointer scratch
int	nchar			# value of tbnget for intable
int	junk, hdu, tbparse()
int	dir_len			# length of root portion of table name
int	tbnget(), tbnlen(), fnldir()
errchk	tbparse

begin
	# Get the next input table name.
	nchar = tbnget (in_t, intable, SZ_LINE)
	if (nchar == EOF)
	    return (EOF)

	# Get the next table for output independent variable values.
	if (xin_t != NULL) {
	    if (tbnlen (xin_t) == 1)		# only one xtable?
		call tbnrew (xin_t)
	    if (tbnget (xin_t, xtable, SZ_LINE) == EOF)
		return (EOF)
	} else {
	    xtable[1] = EOS
	}

	if (out_t != NULL) {

	    # Get the next output table name.
	    if (tbnget (out_t, outtable, SZ_LINE) == EOF)
		return (EOF)

	} else {		# output is a directory name

	    call smark (sp)
	    call salloc (filename, SZ_LINE, TY_CHAR)
	    call salloc (scratch, SZ_LINE, TY_CHAR)

	    # Copy the portion of the table name without brackets to
	    # Memc[filename]; we need to get rid of the brackets because
	    # they confuse fnldir.
	    junk = tbparse (intable, Memc[filename], Memc[scratch],
			SZ_LINE, hdu)

	    # Get the length of the directory prefix.
	    dir_len = fnldir (Memc[filename], Memc[scratch], SZ_LINE)

	    # Copy the output directory name to outtable.
	    call strcpy (outdir, outtable, SZ_LINE)

	    # Append the name of the input file (without directory prefix
	    # and without any bracket suffix) to the output directory.
	    call strcat (Memc[filename+dir_len], outtable, SZ_LINE)

	    call sfree (sp)
	}

	return (nchar)
end
