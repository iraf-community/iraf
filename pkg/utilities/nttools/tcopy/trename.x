include <error.h>

# trename -- Rename table(s)

# The input tables are given by an filename template list.  The output
# is either a matching list of tables or a directory.  The number of
# input tables may be either one or match the number of output tables.
# This is based on the t_imcopy procedure.
#
# Phil Hodge, 21-Aug-1987  Task created.
# Phil Hodge,  7-Sep-1988  Change parameter names for tables.
# Phil Hodge, 28-Dec-1989  Use iferr with call to tbtren.
# Phil Hodge, 16-Mar-1992  Include check to prevent renaming text files.
# Phil Hodge,  1-Jul-1995  Modify for FITS tables.
# Phil Hodge, 19-Jul-1995  Replace fnt calls with tbn.
# B.Simon      5-May-1997  Call rename if renaming entire table
# B.Simon      9-May-1997  Add table type check

procedure trename()

pointer tablist1		# input table list
pointer tablist2		# output table list
bool	verbose			# print operations?
#--
pointer sp
pointer table1			# input table name
pointer fname1			# input file name (i.e. without brackets)
pointer cdfname			# input CDF name or EXTNAME
pointer table2			# output table name
pointer dir1			# input directory name
pointer dir2			# output directory name

pointer list1, list2
int	root_len		# number of char in input directory name

pointer tbnopen()
int	tbnget(), tbnlen()
int	fnldir(), isdirectory()
int	junk, ttype, hdu, tbparse()
bool	clgetb(), streq()

begin
	call smark (sp)
	call salloc (tablist1, SZ_LINE, TY_CHAR)
	call salloc (tablist2, SZ_LINE, TY_CHAR)
	call salloc (table1, SZ_LINE, TY_CHAR)
	call salloc (fname1, SZ_LINE, TY_CHAR)
	call salloc (cdfname, SZ_LINE, TY_CHAR)
	call salloc (table2, SZ_LINE, TY_CHAR)
	call salloc (dir1, SZ_LINE, TY_CHAR)
	call salloc (dir2, SZ_LINE, TY_CHAR)

	# Get input and output table template lists.
	call clgstr ("intable", Memc[tablist1], SZ_LINE)
	call clgstr ("outtable", Memc[tablist2], SZ_LINE)

	verbose = clgetb ("verbose")

	# Check if the output string is a directory.

	if (isdirectory (Memc[tablist2], Memc[dir2], SZ_LINE) > 0 &&
		!streq (Memc[tablist2], "STDOUT")) {

	    list1 = tbnopen (Memc[tablist1])

	    while (tbnget (list1, Memc[table1], SZ_LINE) != EOF) {

		# Memc[fname1] is the name without any brackets.  We need to
		# remove brackets because they confuse fnldir, which we use
		# to get the length of any directory prefix.

		junk = tbparse (Memc[table1], Memc[fname1], Memc[cdfname],
			SZ_LINE, ttype, hdu)
		root_len = fnldir (Memc[fname1], Memc[dir1], SZ_LINE)

		# Copy the output directory name to table2, and concatenate
		# the input file name (without directory prefix and without
		# the bracket suffix).

		call strcpy (Memc[dir2], Memc[table2], SZ_LINE)
		call strcat (Memc[fname1+root_len], Memc[table2], SZ_LINE)

		call one_rename (Memc[table1], Memc[table2], verbose)
	    }

	    call tbnclose (list1)

	} else {

	    # Expand the input and output table lists.
	    list1 = tbnopen (Memc[tablist1])
	    list2 = tbnopen (Memc[tablist2])

	    if (tbnlen (list1) != tbnlen (list2)) {
	        call tbnclose (list1)
	        call tbnclose (list2)
	        call error (1, "Number of input and output tables not the same")
	    }

	    # Rename each table.

	    while ((tbnget (list1, Memc[table1], SZ_LINE) != EOF) &&
		   (tbnget (list2, Memc[table2], SZ_LINE) != EOF)) {

		call one_rename (Memc[table1], Memc[table2], verbose)
	    }

	    call tbnclose (list1)
	    call tbnclose (list2)
	}
end

# ONE_RENAME -- Rename a single table

procedure one_rename (oldfile, newfile, verbose)

char	oldfile[ARB]	# i: current file name
char	newfile[ARB]	# i: new file name
bool	verbose		# i: print informational message
#--
bool	done
pointer	sp, oldname, newname

bool	streq(), is_wholetab()
int	access(), tbtacc(), ext_type()

begin
	call smark (sp)
	call salloc (oldname, SZ_FNAME, TY_CHAR)
	call salloc (newname, SZ_FNAME, TY_CHAR)

	# Check to make sure the copy is legal

	done = false
	if (streq (oldfile, newfile)) {
	    call eprintf ("Cannot rename table to itself:  %s\n")
	    call pargstr (oldfile)

	} else if (access (oldfile, 0, TEXT_FILE) == YES) {
	    call eprintf ("Cannot rename text file with trename: `%s'\n")
	    call pargstr (oldfile)

	} else if (is_wholetab (oldfile) && is_wholetab (newfile) &&
		   ext_type (oldfile) == ext_type (newfile)) {

	    # Entire files of the same type are renamed with the fio rename

	    if (tbtacc (oldfile) == NO) {
		call eprintf ("Can only rename tables with trename: `%s'\n")
		call pargstr (oldfile)

	    } else {
		call tbtext (oldfile, Memc[oldname], SZ_FNAME)
		call tbtext (newfile, Memc[newname], SZ_FNAME)

		iferr (call rename (Memc[oldname], Memc[newname])) {
		    call erract (EA_WARN)
		} else {
		    done = true
		}
	    }

	} else {
	    # Table extensions are renamed by the table 
	    # library function tbtren

	    iferr (call tbtren (oldfile, newfile)) {
		call erract (EA_WARN)
	    } else {
		done = true
	    }
	}

	# Print verbose message

	if (done && verbose) {
	    call printf ("%s -> %s\n")
	    call pargstr (oldfile)
	    call pargstr (newfile)
	    call flush (STDOUT)
	}

	call sfree (sp)
	return
end
