include	<error.h>
include	<ctype.h>
include	<fset.h>	# FIO
include <tbset.h>	# TBtables

# GT_HINFO -- Get the title and axes labels for the plot

procedure gt_hinfo (tp, xlabel, ylabel, xcolumn, ycolumn, maxch)

pointer	tp			# Table pointer
char	xlabel[SZ_LINE]		# Axis label strings (output)
char	ylabel[SZ_LINE]		# Axis label strings (output)
char	xcolumn[SZ_COLNAME]	# X column
char	ycolumn[SZ_COLNAME]	# Y column
int	maxch

char	colunit[SZ_COLUNITS]
char	errmsg[SZ_LINE]		# Error message
pointer	xcd, ycd

int	strlen()
bool	streq()

begin
	# Single table;  X and Y column

	if (!streq (xcolumn, NULL)) {
	    call tbcfnd (tp, xcolumn, xcd, 1)
	    if (xcd <= 0) {
	        call sprintf (errmsg, SZ_LINE, "Cannot find column %s")
	        call pargstr (xcolumn)
	    call error (0, errmsg)
	    }
	    # X axis label comes from column name
	    call sprintf (xlabel, maxch, "%s")
	        call pargstr (xcolumn)
	} else {
	    call sprintf (xlabel, maxch, "%s")
		call pargstr ("Number")
	}

	# Find the column units 
	call tbcigt (xcd, TBL_COL_UNITS, colunit, SZ_COLUNITS)
	if (colunit[1] != EOS) {
	    # Column units exist;  append to X label
	    call sprintf (xlabel[strlen (xlabel)+1], maxch, " [%s]")
		call pargstr (colunit)
	}

	call tbcfnd (tp, ycolumn, ycd, 1)
	if (ycd <= 0) {
	    call sprintf (errmsg, SZ_LINE, "Cannot find column %s")
	    call pargstr (ycolumn)
	    call error (0, errmsg)
	}

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

end
