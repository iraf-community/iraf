# This file contains tu_open and tu_close, which are used to open
# and close a temporary table.
#
# Phil Hodge, 28-Jun-1995  Subroutines created based on Bernie's tedit code.
# Phil hodge, 16-Apr-1999  Remove ttype from calling sequence of tbparse.

# tu_open -- open a temporary table
# If the table is to be opened in-place, then it's just opened.
# Otherwise, a copy of the table is made, and that table is opened
# read-write.  The name of the original table will be returned as
# 'tabname' so it can be passed to tu_close, which needs the name of
# the original file.  If the filename extension is ".tab", then 'table'
# might not include the extension.  This is the reason we need a
# separate output argument 'tabname'.  Note, however, that text tables
# need not have an extension.  If 'table' does not include an extension,
# and a file of that name exists, then ".tab" will not be appended when
# copying to 'tabname'.  'tabname' can differ from the actual file name
# by including a name or number in brackets after the file name.
#
# Note that it is an error if readonly=true but inplace=false.

procedure tu_open (table, root, readonly, inplace, tp, tabname, maxch)

char	table[ARB]	# i: name of table
char	root[ARB]	# i: beginning of name for scratch file
bool	readonly	# i: true if the table is to be opened read-only
bool	inplace		# i: true if the table is to be opened in-place
pointer tp		# o: pointer to table struct
char	tabname[maxch]	# o: full name of original table (incl extension, etc)
int	maxch		# i: size of filename string
#--
pointer sp
pointer temp		# name of temporary table
pointer tname, fname	# full table and file names
pointer extn		# file extension, or EOS
pointer tempdir		# name of directory for temporary copy
pointer errmess		# scratch for error message
int	tlen, flen	# length of table and file names
int	try		# loop index
int	junk, fnldir()
pointer tbtopn()
int	strlen()
errchk	tbtopn, tbtnam, tbfile, fcopy

begin
	if (readonly && !inplace)
	    call error (1, "readonly = yes, but inplace = no")

	if (inplace) {

	    if (readonly) {
		tp = tbtopn (table, READ_ONLY, NULL)
	    } else {
		tp = tbtopn (table, READ_WRITE, NULL)
	    }

	    call tbtnam (tp, tabname, maxch)	# get the full table name

	} else {

	    call smark (sp)
	    call salloc (temp, SZ_LINE, TY_CHAR)
	    call salloc (tname, SZ_LINE, TY_CHAR)
	    call salloc (fname, SZ_LINE, TY_CHAR)
	    call salloc (extn, SZ_LINE, TY_CHAR)
	    call salloc (tempdir, SZ_LINE, TY_CHAR)

	    # Get the full table name, full file name, and extension (if any)
	    # of the original file.  Copy the table name to output.
	    call tbfile (table, Memc[tname], Memc[fname], Memc[extn], SZ_LINE)
	    call strcpy (Memc[tname], tabname, maxch)

	    # Get the name of the directory containing the original file.
	    junk = fnldir (Memc[fname], Memc[tempdir], SZ_LINE)

	    # Copy the original file to a temporary file.  First try to
	    # make the copy in the directory containing the original file.
	    # If that fails then copy the file to tmp$.
	    do try = 1, 2 {

		# Construct the name of a temporary file by concatenating
		# the directory, root, a random number, and the extension
		# of the original file name.
		call strcat (root, Memc[tempdir], SZ_LINE)
		call mktemp (Memc[tempdir], Memc[temp], SZ_LINE)
		call strcat (Memc[extn], Memc[temp], SZ_LINE)

		# Copy the file.
		ifnoerr (call fcopy (Memc[fname], Memc[temp]))
		    break

		if (try == 1) {
		    # The first try failed.  Copy the file to tmp$.
		    call strcpy ("tmp$", Memc[tempdir], SZ_LINE)
		} else {
		    # The second try failed as well.
		    call salloc (errmess, SZ_LINE, TY_CHAR)
		    call sprintf (Memc[errmess], SZ_LINE,
			"unable to make a temporary copy of %s")
			call pargstr (Memc[fname])
		    call error (1, Memc[errmess])
		}
	    }

	    # If there was a bracketed expression (e.g. EXTNAME) in the
	    # input table name, append it to the name of the temp file
	    # to convert the file name to a complete table name.
	    tlen = strlen (Memc[tname])
	    flen = strlen (Memc[fname])
	    if (tlen > flen)
		call strcat (Memc[tname+flen], Memc[temp], SZ_LINE)

	    tp = tbtopn (Memc[temp], READ_WRITE, NULL)

	    call sfree (sp)
	}
end

# tu_close -- close a temporary table
# This routine first closes the table that was edited.  If it was opened
# inplace, then we have nothing further to do.  Otherwise, we were editing
# a temporary copy of the original.  If the command was to quit without
# saving changes, we delete the temporary file.  If the command was to
# exit, saving changes, we rename the copy back to the original.
# The quit and tabname arguments will be ignored if inplace is true.

procedure tu_close (tp, inplace, quit, tabname)

pointer tp		# i: pointer to table struct for edited table
bool	inplace		# i: true if the table was edited inplace
bool	quit		# i: true if we should quit without saving changes
char	tabname[ARB]	# i: name of original table (not temp copy)
#--
pointer sp
pointer temp		# name of temporary file
pointer tname		# name of temporary table
pointer filename	# name of original file
pointer cdfname		# scratch
pointer errmess		# for error message
int	hdu		# ignored
int	junk
int	errget()
int	tbparse()
errchk	tbparse, delete, rename

begin
	if (tp == NULL)
	    return

	call smark (sp)
	call salloc (tname, SZ_LINE, TY_CHAR)

	# Get the name of the table that we edited, then close it.
	call tbtnam (tp, Memc[tname], SZ_LINE)
	call tbtclo (tp)

	if (!inplace) {

	    call salloc (temp, SZ_LINE, TY_CHAR)
	    call salloc (filename, SZ_LINE, TY_CHAR)
	    call salloc (cdfname, SZ_LINE, TY_CHAR)

	    # Strip off brackets (if present) to get the file name
	    # for the table that we edited.
	    junk = tbparse (Memc[tname], Memc[temp],
			Memc[cdfname], SZ_LINE, hdu)

	    # Strip off brackets (if present) to get the file name
	    # of the original table.
	    junk = tbparse (tabname, Memc[filename],
			Memc[cdfname], SZ_LINE, hdu)

	    if (quit) {

		call delete (Memc[temp])		# delete temp copy

	    } else {

		iferr {
		    call delete (Memc[filename])	# delete original file
		    call rename (Memc[temp], Memc[filename])
		} then {
		    call salloc (errmess, SZ_LINE, TY_CHAR)
		    junk = errget (Memc[errmess], SZ_LINE)
		    call eprintf ("%s\n")
			call pargstr (Memc[errmess])
		    call sprintf (Memc[errmess], SZ_LINE,
			"couldn't rename edited file %s to original %s\n")
			call pargstr (Memc[temp])
			call pargstr (Memc[filename])
		    call error (1, Memc[errmess])
		}
	    }
	}

	call sfree (sp)
end
