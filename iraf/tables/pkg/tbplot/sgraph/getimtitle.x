include <imhdr.h>
include <tbset.h>
include "sgraph.h"

# GG_GET_IMTITLE -- Given the operand list as input, fetch the first operand
# and try to open it as an image.  If the operand is an image return the
# title string as an output argument, otherwise return the null string.
# If there are multiple operands only the first is used.

procedure gg_get_imtitle (oplist, title, xlabel, ylabel, maxch)

char	oplist[SZ_LINE]		# List of operands to be graphed
char	title[SZ_LINE]		# Image title string (output)
char	xlabel[SZ_LINE]		# Axis label strings (output)
char	ylabel[SZ_LINE]		# Axis label strings (output)
int	maxch

char	word[SZ_FNAME,4]
char	operand[SZ_FNAME], section[SZ_FNAME]
char	table[SZ_FNAME], column[SZ_COLNAME]
char	xtable[SZ_FNAME], xcolumn[SZ_COLNAME]
char	ytable[SZ_FNAME], ycolumn[SZ_COLNAME]
char	ftabnam[SZ_FNAME], xftabnam[SZ_FNAME], yftabnam[SZ_FNAME]
char	colunit[SZ_COLUNITS]
char	root[SZ_FNAME]		# Table root name
char	rowselect[SZ_FNAME]	# 3-D Table row selector
char	colselect[SZ_FNAME]	# 3-D Table column selector

pointer fd, xfd, yfd
pointer	td, xtd, ytd
pointer	cd, xcd, ycd
pointer	im
int	iw
char	errmsg[SZ_LINE]		# Error message

int	imtopen(), imtgetim(), gg_optype(), stridxs(), imtlen()
int	nscan(), strlen()
pointer	immap(), tbtopn()
pointer tbnopen()
int	tbnget()
bool	streq()


errchk	imtopen, imtgetim, tbtopn, immap

begin
	title[1] = EOS

	call sscan (oplist) 
	do iw = 1, 4
	    call gargwrd (word[1,iw], SZ_FNAME)
	
	switch (nscan ()) {
	# How many words in input line?

	case 1:
	    # Image or list file
	    # Get the first operand name.  
	    # If it is not an image we are all done.
	    fd = imtopen (word)

	    if (imtgetim (fd, operand, SZ_FNAME) == EOF)
		return
	    if (gg_optype (operand) != IMAGE_OP)
		return

	    # Open the image.
#	    iferr (im = immap (operand, READ_ONLY, 0))
#		return
	    im = immap (operand, READ_ONLY, 0)

	    # Get image title.
	    call strcpy (IM_TITLE(im), title, maxch)
	    if (stridxs ("\n", title) == 0)
		call strcat ("\n", title, maxch)

	    # If an image section was given and there was only one operand in
	    # the operand list, append the section to the title.

	    call imgsection (operand, section, SZ_FNAME)

	    if (section[1] != EOS) {
		if (imtlen(fd) == 1)
		    call strcat (section, title, maxch)
	    }

	    call imunmap  (im)
	    call imtclose (fd)

	case 2:
	    # Single table column
	    fd = tbnopen (word[1,1])

	    if (tbnget (fd, table, SZ_FNAME) == EOF) 
		return

	# Check to see if there are any row/col selectors 
	# appended to table name...
	    call rdselect (table, root, rowselect, colselect, SZ_PATHNAME)
	
	# Check to see if we are working with a 3-D table 
	    if (rowselect[1] != EOS || colselect[1] != EOS) {
		# We are working with a 3-D table, so strip off
		# descriptors from filename so 'fpathname' will work right...
		call strcpy(root, table, SZ_FNAME)
	    }

	    call fpathname (table, ftabnam, SZ_PATHNAME)

	    td = tbtopn (table, READ_ONLY, 0)

	    call sprintf (title, maxch, "%s\n")
		call pargstr (ftabnam)

	    if (xlabel[1] == EOS)
		call strcpy ("Row", xlabel, maxch)

	    call strcpy (word[1,2], column, SZ_COLNAME)

	    call tbcfnd (td, column, cd, 1)

	    if (cd <= 0) {
		call sprintf (errmsg, SZ_LINE, 
		    "Cannot find column %s in table %s\n")
		    call pargstr (column)
		    call pargstr (table)
		call error (0, errmsg)
	    }

	    if (ylabel[1] == EOS) {
		# Y label comes from column name
		call sprintf (ylabel, maxch, "%s")
		    call pargstr (column)

		# Find the column units 
		call tbcigt (cd, TBL_COL_UNITS, colunit, SZ_COLUNITS)
		if (colunit[1] != EOS) {
		    # Column units exist;  append to Y label
		    call sprintf (ylabel[strlen (ylabel)+1], maxch, " [%s]")
		    call pargstr (colunit)
		}
	    }

	    call tbtclo (td)
	    call tbnclose (fd)

	case 3:
	    # Single table;  X and Y column

	    fd = tbnopen (word[1,1])

	    if (tbnget (fd, table, SZ_FNAME) == EOF) 
		return

	# Check to see if there are any row/col selectors 
	# appended to table name...
	    call rdselect (table, root, rowselect, colselect, SZ_FNAME)
	
	# Check to see if we are working with a 3-D table 
	    if (rowselect[1] != EOS || colselect[1] != EOS) {
		# We are working with a 3-D table, so strip off
		# descriptors from filename so 'fpathname' will work right...
		call strcpy(root, table, SZ_FNAME)
	    }
	    call fpathname (table, ftabnam, SZ_FNAME)

	    td = tbtopn (table, READ_ONLY, 0)

	    call strcpy (word[1,2], xcolumn, SZ_COLNAME)
	    call tbcfnd (td, xcolumn, xcd, 1)
	    if (xcd <= 0) {
		call sprintf (errmsg, SZ_LINE, 
		    "Cannot find column %s in table %s\n")
		    call pargstr (xcolumn)
		    call pargstr (table)
		call error (0, errmsg)
	    }

	    if (xlabel[1] == EOS) {
		# X axis label comes from column name
		call sprintf (xlabel, maxch, "%s")
		    call pargstr (xcolumn)

		# Find the column units 
		call tbcigt (xcd, TBL_COL_UNITS, colunit, SZ_COLUNITS)
		if (colunit[1] != EOS) {
		    # Column units exist;  append to X label
		    call sprintf (xlabel[strlen (xlabel)+1], maxch, " [%s]")
		    call pargstr (colunit)
		}
	    }

	    call strcpy (word[1,3], ycolumn, SZ_COLNAME)
	    call tbcfnd (td, ycolumn, ycd, 1)
	    if (ycd <= 0) {
		call sprintf (errmsg, SZ_LINE, 
		    "Cannot find column %s in table %s\n")
		    call pargstr (ycolumn)
		    call pargstr (table)
		call error (0, errmsg)
	    }

	    if (ylabel[1] == EOS) {
		# Y label comes from column name
		call sprintf (ylabel, maxch, "%s")
		    call pargstr (ycolumn)

		# Find the column units 
		call tbcigt (ycd, TBL_COL_UNITS, colunit, SZ_COLUNITS)
		if (colunit[1] != EOS) {
		    # Column units exist;  append to Y label
		    call sprintf (ylabel[strlen (ylabel)+1], maxch, " [%s]")
		    call pargstr (colunit)
		}
	    }

	    call sprintf (title, maxch, "%s\n")
		call pargstr (ftabnam)

	    call tbtclo (td)
	    call tbnclose (fd)

	case 4:
	    # X table and column;  Y table and column

	    xfd = tbnopen (word[1,1])
	    if (tbnget (xfd, xtable, SZ_FNAME) == EOF) 
		return
	# Check to see if there are any row/col selectors 
	# appended to table name...
	    call rdselect (xtable, root, rowselect, colselect, SZ_FNAME)
	
	# Check to see if we are working with a 3-D table 
	    if (rowselect[1] != EOS || colselect[1] != EOS) {
		# We are working with a 3-D table, so strip off
		# descriptors from filename so 'fpathname' will work right...
		call strcpy(root, xtable, SZ_FNAME)
	    }

	    call fpathname (xtable, xftabnam, SZ_FNAME)
	    xtd = tbtopn (xtable, READ_ONLY, 0)
	    call strcpy (word[1,2], xcolumn, SZ_COLNAME)
	    call tbcfnd (xtd, xcolumn, xcd, 1)

	    if (xcd <= 0) {
		call sprintf (errmsg, SZ_LINE, 
		    "Cannot find column %s in table %s\n")
		    call pargstr (xcolumn)
		    call pargstr (xtable)
		call error (0, errmsg)
	    }

	    if (xlabel[1] == EOS) {
		# X axis label comes from column name
		call sprintf (xlabel, maxch, "%s")
		    call pargstr (xcolumn)

		# Find the column units 
		call tbcigt (xcd, TBL_COL_UNITS, colunit, SZ_COLUNITS)
		if (colunit[1] != EOS) {
		    # Column units exist;  append to Y label
		    call sprintf (xlabel[strlen (xlabel)+1], maxch, " [%s]")
		    call pargstr (colunit)
		}
	    }

	    yfd = tbnopen (word[1,3])
	    if (tbnget (yfd, ytable, SZ_FNAME) == EOF) 
		return

	# Check to see if there are any row/col selectors 
	# appended to table name...
	    call rdselect (ytable, root, rowselect, colselect, SZ_FNAME)
	
	# Check to see if we are working with a 3-D table 
	    if (rowselect[1] != EOS || colselect[1] != EOS) {
		# We are working with a 3-D table, so strip off
		# descriptors from filename so 'fpathname' will work right...
		call strcpy(root, ytable, SZ_FNAME)
	    }

	    if (streq (xtable, ytable))
		ytd = xtd
	    else
		ytd = tbtopn (ytable, READ_ONLY, 0)

	    call fpathname (ytable, yftabnam, SZ_FNAME)
	    ytd = tbtopn (ytable, READ_ONLY, 0)
	    call strcpy (word[1,4], ycolumn, SZ_COLNAME)
	    call tbcfnd (ytd, ycolumn, ycd, 1)
	    if (ycd <= 0) {
		call sprintf (errmsg, SZ_LINE, 
		    "Cannot find column %s in table %s\n")
		    call pargstr (ycolumn)
		    call pargstr (ytable)
		call error (0, errmsg)
	    }

	    if (ylabel[1] == EOS) {
		# Y label comes from column name
		call sprintf (ylabel, maxch, "%s")
		    call pargstr (ycolumn)

		# Find the column units 
		call tbcigt (ycd, TBL_COL_UNITS, colunit, SZ_COLUNITS)
		if (colunit[1] != EOS) {
		    # Column units exist;  append to Y label
		    call sprintf (ylabel[strlen (ylabel)+1], maxch, " [%s]")
		    call pargstr (colunit)
		}
	    }

	    call sprintf (title, maxch, "%s\n%s\n")
		call pargstr (xftabnam)
		call pargstr (yftabnam)

	    call tbtclo (xtd)
	    if (xtd != ytd)
		call tbtclo (ytd)

	    call tbnclose (xfd)
	    call tbnclose (yfd)
	}
end
