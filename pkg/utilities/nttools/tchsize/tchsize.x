include <tbset.h>

# tchsize -- Change size of table(s)

# The input tables are given by an filename template list.  The output
# is either a matching list of tables or a directory.  The number of
# input tables may be either one or match the number of output tables.
# This is based on the t_imcopy procedure.
#
# NOTE:  If task is used on a FITS table, it will be skipped.
#
# Phil Hodge, 28-Aug-1987  Task created.
# Phil Hodge,  7-Sep-1988  Change parameter names for tables.
# Phil Hodge, 26-Mar-1992  Remove calls to tbtext.
# Phil Hodge,  3-Oct-1995  Modify to use tbn instead of fnt; check for FITS.
# Phil hodge, 16-Apr-1999  Call tbttyp instead of tbparse.

procedure tchsize()

char	tablist1[SZ_LINE]	# Input table list
char	tablist2[SZ_LINE]	# Output table list
bool	verbose			# Print operations?
int	maxpar			# new max number of user (header) parameters
int	maxcols			# new max space for column descriptors
int	rowlen			# new row length (for row-ordered tables)
int	allrows			# new allocated number of rows (for col-ordered)
#--
int	ttype, exists, tbttyp()	# to check for a FITS table (exists is ignored)

pointer tp			# Pointer to table descriptor
char	table1[SZ_PATHNAME]	# Input table name
char	table2[SZ_PATHNAME]	# Output table name
char	dirname1[SZ_PATHNAME]	# Directory name
char	dirname2[SZ_PATHNAME]	# Directory name

pointer list1, list2
int	root_len

pointer tbtopn()
pointer tbnopen()
int	tbnget(), tbnlen()
int	fnldir(), isdirectory()
int	clgeti()
bool	clgetb(), streq()

begin
	# Get input and output table template lists.
	call clgstr ("intable", tablist1, SZ_LINE)
	call clgstr ("outtable", tablist2, SZ_LINE)

	# Get new values of table size parameters.
	maxpar  = clgeti ("maxpar")
	maxcols = clgeti ("maxcols")
	rowlen  = clgeti ("rowlen") * SZ_REAL
	allrows = clgeti ("allrows")

	verbose = clgetb ("verbose")

	# Check whether the output string is blank.
	if (tablist2[1] == EOS) {

	    # Expand the input table list, and change the sizes in-place.
	    list1 = tbnopen (tablist1)

	    while (tbnget (list1, table1, SZ_PATHNAME) != EOF) {

		# Check for a FITS table.
		ttype = tbttyp (table1, exists)
		if (ttype == TBL_TYPE_FITS) {
		    call eprintf ("Skipping FITS table %s\n")
			call pargstr (table1)
		    next
		}

		if (verbose) {
		    call printf ("%s\n")
			call pargstr (table1)
		    call flush (STDOUT)
		}
		# Open the table, change its size, and close it.
		tp = tbtopn (table1, READ_WRITE, 0)
		call tbtchs (tp, maxpar, maxcols, rowlen, allrows)
		call tbtclo (tp)
	    }
	    call tbnclose (list1)

	# Check whether the output string is a directory.
	} else if (isdirectory (tablist2, dirname2, SZ_PATHNAME) > 0) {

	    list1 = tbnopen (tablist1)
	    while (tbnget (list1, table1, SZ_PATHNAME) != EOF) {

		ttype = tbttyp (table1, exists)
		if (ttype == TBL_TYPE_FITS) {
		    call eprintf ("Skipping FITS table %s\n")
			call pargstr (table1)
		    next
		}

		# Place the input table name without a directory in
		# string dirname1.

		call get_root (table1, table2, SZ_PATHNAME)
		root_len = fnldir (table2, dirname1, SZ_PATHNAME)
		call strcpy (table2[root_len + 1], dirname1, SZ_PATHNAME)

		call strcpy (dirname2, table2, SZ_PATHNAME)
		call strcat (dirname1, table2, SZ_PATHNAME)

		if (verbose) {
		    call printf ("%s -> %s\n")
			call pargstr (table1)
			call pargstr (table2)
		    call flush (STDOUT)
		}
		# Copy the table, open it, change its size, and close it.
		call tbtcpy (table1, table2)
		tp = tbtopn (table2, READ_WRITE, 0)
		call tbtchs (tp, maxpar, maxcols, rowlen, allrows)
		call tbtclo (tp)
	    }
	    call tbnclose (list1)

	} else {

	    # Expand the input and output table lists.

	    list1 = tbnopen (tablist1)
	    list2 = tbnopen (tablist2)

	    if (tbnlen (list1) != tbnlen (list2)) {
	        call tbnclose (list1)
	        call tbnclose (list2)
	        call error (1, "Number of input and output tables not the same")
	    }

	    # Change each table in the list.

	    while ((tbnget (list1, table1, SZ_PATHNAME) != EOF) &&
		   (tbnget (list2, table2, SZ_PATHNAME) != EOF)) {

		ttype = tbttyp (table1, exists)
		if (ttype == TBL_TYPE_FITS) {
		    call eprintf ("Skipping FITS table:  %s --> %s\n")
			call pargstr (table1)
			call pargstr (table2)
		    next
		}

		if (streq (table1, table2)) {
		    # Same input and output names; no need to copy the table.
		    if (verbose) {
			call eprintf ("%s\n")
			    call pargstr (table1)
		    }
		} else {
		    # Different input & output names, so copy the table.
		    if (verbose) {
			call eprintf ("%s -> %s\n")
			    call pargstr (table1)
			    call pargstr (table2)
		    }
		    call tbtcpy (table1, table2)
		}
		# Open the table, change its size, and close it.
		tp = tbtopn (table2, READ_WRITE, 0)
		call tbtchs (tp, maxpar, maxcols, rowlen, allrows)
		call tbtclo (tp)
	    }
	    call tbnclose (list1)
	    call tbnclose (list2)
	}
end
