include	<error.h>

#  TXIMAGE  --  Extract image from 3D table row.

#  Input tables are given by a filename template list. All row/column 
#  selection on input tables is performed by bracket-enclosed selectors
#  appended to the file name. The output is either a matching list of 
#  images or a directory. Since one input table specification can generate 
#  multiple output images, a naming scheme for these is defined as follows:
#
#  -  if output name is a directory:
#        output image names are built from input table names appended with 
#        a _rXXX suffix, where XXX is the row number in the input file 
#        where the data comes from.
#
#  -  if output image name comes from a paired root file name list:
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
#  26-Nov-96  -  Task created (I.Busko)
#  03-Jan-97  -  Revised after code review (IB)


procedure t_tximage()

char	tablist1[SZ_LINE]		# Input table list
char	imlist2[SZ_LINE]		# Output image list
bool	verbose				# Print operations ?

char	table1[SZ_PATHNAME]		# Input table name
char	image2[SZ_PATHNAME]		# Output table name
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
	call clgstr ("output",  imlist2, SZ_LINE)
	verbose = clgetb ("verbose")

	# Check if the output string is a directory.

	if (isdirectory (imlist2, dirname, SZ_PATHNAME) > 0) {
	    list1 = imtopen (tablist1)	
	    while (imtgetim (list1, table1, SZ_PATHNAME) != EOF) {
		call smark (sp)

		# Place the input table name without a directory in
		# string rootname.

		call get_root (table1, image2, SZ_PATHNAME)
		root_len = fnldir (image2, rootname, SZ_PATHNAME)
		call strcpy (image2[root_len + 1], rootname, SZ_PATHNAME)

		call strcpy (dirname, image2, SZ_PATHNAME)
		call strcat (rootname, image2, SZ_PATHNAME)

		iferr (call txione (table1, image2, verbose))
		    call erract (EA_WARN)

		call sfree (sp)
	    }
	    call imtclose (list1)

	} else {
	    # Expand the input and output table lists.

	    list1 = imtopen (tablist1)
	    list2 = imtopen (imlist2)

	    if (imtlen (list1) != imtlen (list2)) {
	        call imtclose (list1)
	        call imtclose (list2)
	        call error (1, "Number of input and output files not the same")
	    }

	    # Expand each table.

	    while ((imtgetim (list1, table1, SZ_PATHNAME) != EOF) &&
		   (imtgetim (list2, image2, SZ_PATHNAME) != EOF)) {

		call smark (sp)

		if (streq (table1, image2)) {
		    call eprintf ("can't expand table to itself:  %s\n")
			call pargstr (table1)
		    next
		}
		iferr (call txione (table1, image2, verbose))
		    call erract (EA_WARN)

		call sfree (sp)
	    }

	    call imtclose (list1)
	    call imtclose (list2)
	}
end
