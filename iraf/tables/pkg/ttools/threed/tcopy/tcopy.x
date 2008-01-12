include	<error.h>

# tcopy -- Copy table(s)

# The input tables are given by an filename template list.  The output
# is either a matching list of tables or a directory.  The number of
# input tables may be either one or match the number of output tables.
# This is based on the t_imcopy procedure.
#
# Phil Hodge, 21-Aug-87  Task created.
# Phil Hodge,  7-Sep-88  Change parameter names for tables.
# Phil Hodge, 28-Dec-89  Use iferr with call to tbtcpy.
# Phil Hodge, 26-Mar-92  Remove calls to tbtext.
# B.Simon,    04-Nov-94  Replace call to tbtcpy with tcpyone
# I.Busko,    20-Nov-95  Add support for ASCII output.

procedure t_tcopy()

char	tablist1[SZ_LINE]		# Input table list
char	tablist2[SZ_LINE]		# Output table list
bool	verbose				# Print operations?

char	table1[SZ_PATHNAME]		# Input table name
char	table2[SZ_PATHNAME]		# Output table name
char	dirname1[SZ_PATHNAME]		# Directory name
char	dirname2[SZ_PATHNAME]		# Directory name

int	list1, list2, root_len
pointer	sp

int	imtopen(), imtgetim(), imtlen()
int	fnldir(), isdirectory()
bool	clgetb(), streq()

begin
	# Get input and output table template lists.

	call clgstr ("intable", tablist1, SZ_LINE)
	call clgstr ("outtable", tablist2, SZ_LINE)
	verbose = clgetb ("verbose")

	# Check if the output string is a directory.

	if (isdirectory (tablist2, dirname2, SZ_PATHNAME) > 0 &&
                !streq (tablist2, "STDOUT")) {
	    list1 = imtopen (tablist1)	
	    while (imtgetim (list1, table1, SZ_PATHNAME) != EOF) {
		call smark (sp)

		# Place the input table name without a directory in
		# string dirname1.

		call get_root (table1, table2, SZ_PATHNAME)
		root_len = fnldir (table2, dirname1, SZ_PATHNAME)
		call strcpy (table2[root_len + 1], dirname1, SZ_PATHNAME)

		call strcpy (dirname2, table2, SZ_PATHNAME)
		call strcat (dirname1, table2, SZ_PATHNAME)

		if (verbose) {
		    call eprintf ("%s -> %s\n")
			call pargstr (table1)
			call pargstr (table2)
		}
		iferr (call tcpyone (table1, table2))
		    call erract (EA_WARN)

		call sfree (sp)
	    }
	    call imtclose (list1)

	} else {
	    # Expand the input and output table lists.

	    list1 = imtopen (tablist1)
	    list2 = imtopen (tablist2)

	    if (imtlen (list1) != imtlen (list2)) {
	        call imtclose (list1)
	        call imtclose (list2)
	        call error (1, "Number of input and output tables not the same")
	    }

	    # Copy each table.

	    while ((imtgetim (list1, table1, SZ_PATHNAME) != EOF) &&
		   (imtgetim (list2, table2, SZ_PATHNAME) != EOF)) {

		call smark (sp)

		if (streq (table1, table2)) {
		    call eprintf ("can't copy table to itself:  %s\n")
			call pargstr (table1)
		    next
		}
		if (verbose) {
		    call eprintf ("%s -> %s\n")
			call pargstr (table1)
			call pargstr (table2)
		}
		iferr (call tcpyone (table1, table2))
		    call erract (EA_WARN)

		call sfree (sp)
	    }

	    call imtclose (list1)
	    call imtclose (list2)
	}
end
