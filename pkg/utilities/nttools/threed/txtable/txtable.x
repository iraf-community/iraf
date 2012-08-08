include	<error.h>

#  TXTABLE  --  Extract 2D tables from 3D table rows.

#  Input tables are given by a filename template list. All row/column 
#  selection on input tables is performed by bracket-enclosed selectors
#  appended to the file name. The output is either a matching list of 
#  tables or a directory. Output table names cannot have row/column
#  selectors. Since one input table specification can generate multiple 
#  output tables, a naming scheme for these is defined as follows:
#
#  -  if output name is a directory:
#        output table names are built from input table names appended with 
#        a _rXXX suffix, where XXX is the row number in the input file 
#        where the data comes from.
#
#  -  if output file name comes from a paired root file name list:
#        same suffixing scheme as above, but using the root file name 
#        extracted from the list.
#
#  -  if only one row is selected:
#        no suffixing takes place.
#
#
#  This code is a re-use of B.Simon's 04-Nov-94 version of tcopy.
#
#
#
#  Revision history:
#  ----------------
#
#  22-Nov-96  -  Task created (I.Busko)
#  03-Jan-97  -  Revised after code review (IB)


procedure t_txtable()

char	tablist1[SZ_LINE]		# Input table list
char	tablist2[SZ_LINE]		# Output table list
bool	compact				# Put scalars in header ?
bool	verbose				# Print operations ?

char	table1[SZ_PATHNAME]		# Input table name
char	table2[SZ_PATHNAME]		# Output table name
char	rootname[SZ_PATHNAME]		# Root name
char	dirname[SZ_PATHNAME]		# Directory name

int	list1, list2, root_len
pointer	sp

int	imtopen(), imtgetim(), imtlen()
int	fnldir(), isdirectory()
bool	clgetb(), streq()

begin
	# Get input and output table template lists.

	call clgstr ("intable", tablist1, SZ_LINE)
	call clgstr ("outtable", tablist2, SZ_LINE)
	compact = clgetb ("compact")
	verbose = clgetb ("verbose")

	# Check if the output string is a directory.

	if (isdirectory (tablist2, dirname, SZ_PATHNAME) > 0 &&
                !streq (tablist2, "STDOUT")) {
	    list1 = imtopen (tablist1)	
	    while (imtgetim (list1, table1, SZ_PATHNAME) != EOF) {
		call smark (sp)

		# Place the input table name without a directory in
		# string rootname.

		call get_root (table1, table2, SZ_PATHNAME)
		root_len = fnldir (table2, rootname, SZ_PATHNAME)
		call strcpy (table2[root_len + 1], rootname, SZ_PATHNAME)

		call strcpy (dirname, table2, SZ_PATHNAME)
		call strcat (rootname, table2, SZ_PATHNAME)

		iferr (call txtone (table1, table2, verbose, compact))
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

	    # Expand each table.

	    while ((imtgetim (list1, table1, SZ_PATHNAME) != EOF) &&
		   (imtgetim (list2, table2, SZ_PATHNAME) != EOF)) {

		call smark (sp)

		if (streq (table1, table2)) {
		    call eprintf ("can't expand table to itself:  %s\n")
			call pargstr (table1)
		    next
		}
		iferr (call txtone (table1, table2, verbose, compact))
		    call erract (EA_WARN)

		call sfree (sp)
	    }

	    call imtclose (list1)
	    call imtclose (list2)
	}
end
