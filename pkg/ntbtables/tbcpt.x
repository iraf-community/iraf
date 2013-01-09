include <tbset.h>
include "tbtables.h"
include "tblerr.h"

# Write values for one column to a range of rows.
#
# Phil Hodge, 28-Dec-1987  Different data types combined into one file.
# Phil Hodge,  3-Feb-1992  Add option for text table type.
# Phil Hodge, 31-Mar-1993  Include short datatype.
# Phil Hodge,  4-Nov-1993  Include check that row is > 0.
# Phil Hodge,  3-Apr-1995  Set TB_MODIFIED to true.
# Phil Hodge, 23-Jun-1995  Modify for FITS tables;
#			change declaration of buffer in tbcptt to 2-D array.
# Phil Hodge, 29-Jul-1997  Call tbtwer for fits tables.
# Phil Hodge,  3-Mar-1998  Modify to allow for row selector.

# tbcptd -- putcol double
# Write values for one column to a range of rows.  This is for data type
# double precision.

procedure tbcptd (tp, cp, buffer, sel_firstrow, sel_lastrow)

pointer tp		# i: pointer to table descriptor
pointer cp		# i: pointer to descriptor of the column
double	buffer[ARB]	# i: array of values to be put into column
int	sel_firstrow	# i: first row into which to put values
int	sel_lastrow	# i: last row into which to put values
#--
int	firstrow, lastrow	# range of actual row numbers
int	row, i
errchk	tbswer, tbxcpd, tbycpd, tbzcpd, tbfapd

begin
	if (TB_READONLY(tp))
	    call error (ER_TBREADONLY, "can't write to table; it's readonly")

	if (sel_lastrow < sel_firstrow)
	    call error (1, "tbcptd:  lastrow is less than firstrow")

	# If we're writing beyond EOF, write extra rows and update TB_NROWS.
	# Also convert to actual row numbers.
	call tbswer (tp, sel_lastrow, lastrow)
	call tbswer (tp, sel_firstrow, firstrow)

	if (TB_TYPE(tp) == TBL_TYPE_S_ROW) {
	    call tbxcpd (tp, cp, buffer, firstrow, lastrow)
	} else if (TB_TYPE(tp) == TBL_TYPE_S_COL) {
	    call tbycpd (tp, cp, buffer, firstrow, lastrow)
	} else if (TB_TYPE(tp) == TBL_TYPE_TEXT) {
	    call tbzcpd (tp, cp, buffer, firstrow, lastrow)
	} else if (TB_TYPE(tp) == TBL_TYPE_FITS) {
	    i = 1
	    do row = firstrow, lastrow {
		call tbfapd (tp, cp, row, buffer[i], 1, 1)
		i = i + 1
	    }
	} else {
	    call error (ER_TBCORRUPTED, "tbcptd:  table type is messed up")
	}

	TB_MODIFIED(tp) = true
end


# tbcptr -- putcol real
# Write values for one column to a range of rows.  This is for data type real.

procedure tbcptr (tp, cp, buffer, sel_firstrow, sel_lastrow)

pointer tp		# i: pointer to table descriptor
pointer cp		# i: pointer to descriptor of the column
real	buffer[ARB]	# i: array of values to be put into column
int	sel_firstrow	# i: first row into which to put values
int	sel_lastrow	# i: last row into which to put values
#--
int	firstrow, lastrow	# range of actual row numbers
int	row, i
errchk	tbswer, tbxcpr, tbycpr, tbzcpr, tbfapr

begin
	if (TB_READONLY(tp))
	    call error (ER_TBREADONLY, "can't write to table; it's readonly")

	if (sel_lastrow < sel_firstrow)
	    call error (1, "tbcptr:  lastrow is less than firstrow")

	# If we're writing beyond EOF, write extra rows and update TB_NROWS.
	# Also convert to actual row numbers.
	call tbswer (tp, sel_lastrow, lastrow)
	call tbswer (tp, sel_firstrow, firstrow)

	if (TB_TYPE(tp) == TBL_TYPE_S_ROW) {
	    call tbxcpr (tp, cp, buffer, firstrow, lastrow)
	} else if (TB_TYPE(tp) == TBL_TYPE_S_COL) {
	    call tbycpr (tp, cp, buffer, firstrow, lastrow)
	} else if (TB_TYPE(tp) == TBL_TYPE_TEXT) {
	    call tbzcpr (tp, cp, buffer, firstrow, lastrow)
	} else if (TB_TYPE(tp) == TBL_TYPE_FITS) {
	    i = 1
	    do row = firstrow, lastrow {
		call tbfapr (tp, cp, row, buffer[i], 1, 1)
		i = i + 1
	    }
	} else {
	    call error (ER_TBCORRUPTED, "tbcptr:  table type is messed up")
	}

	TB_MODIFIED(tp) = true
end


# tbcpti -- putcol integer
# Write values for one column to a range of rows.  This is for data type
# integer.

procedure tbcpti (tp, cp, buffer, sel_firstrow, sel_lastrow)

pointer tp		# i: pointer to table descriptor
pointer cp		# i: pointer to descriptor of the column
int	buffer[ARB]	# i: array of values to be put into column
int	sel_firstrow	# i: first row into which to put values
int	sel_lastrow	# i: last row into which to put values
#--
int	firstrow, lastrow	# range of actual row numbers
int	row, i
errchk	tbswer, tbxcpi, tbycpi, tbzcpi, tbfapi

begin
	if (TB_READONLY(tp))
	    call error (ER_TBREADONLY, "can't write to table; it's readonly")

	if (sel_lastrow < sel_firstrow)
	    call error (1, "tbcpti:  lastrow is less than firstrow")

	# If we're writing beyond EOF, write extra rows and update TB_NROWS.
	# Also convert to actual row numbers.
	call tbswer (tp, sel_lastrow, lastrow)
	call tbswer (tp, sel_firstrow, firstrow)

	if (TB_TYPE(tp) == TBL_TYPE_S_ROW) {
	    call tbxcpi (tp, cp, buffer, firstrow, lastrow)
	} else if (TB_TYPE(tp) == TBL_TYPE_S_COL) {
	    call tbycpi (tp, cp, buffer, firstrow, lastrow)
	} else if (TB_TYPE(tp) == TBL_TYPE_TEXT) {
	    call tbzcpi (tp, cp, buffer, firstrow, lastrow)
	} else if (TB_TYPE(tp) == TBL_TYPE_FITS) {
	    i = 1
	    do row = firstrow, lastrow {
		call tbfapi (tp, cp, row, buffer[i], 1, 1)
		i = i + 1
	    }
	} else {
	    call error (ER_TBCORRUPTED, "tbcpti:  table type is messed up")
	}

	TB_MODIFIED(tp) = true
end


# tbcpts -- putcol short
# Write values for one column to a range of rows.  This is for data type
# short integer.

procedure tbcpts (tp, cp, buffer, sel_firstrow, sel_lastrow)

pointer tp		# i: pointer to table descriptor
pointer cp		# i: pointer to descriptor of the column
short	buffer[ARB]	# i: array of values to be put into column
int	sel_firstrow	# i: first row into which to put values
int	sel_lastrow	# i: last row into which to put values
#--
int	firstrow, lastrow	# range of actual row numbers
int	row, i
errchk	tbswer, tbxcps, tbycps, tbzcps, tbfaps

begin
	if (TB_READONLY(tp))
	    call error (ER_TBREADONLY, "can't write to table; it's readonly")

	if (sel_lastrow < sel_firstrow)
	    call error (1, "tbcpts:  lastrow is less than firstrow")

	# If we're writing beyond EOF, write extra rows and update TB_NROWS.
	# Also convert to actual row numbers.
	call tbswer (tp, sel_lastrow, lastrow)
	call tbswer (tp, sel_firstrow, firstrow)

	if (TB_TYPE(tp) == TBL_TYPE_S_ROW) {
	    call tbxcps (tp, cp, buffer, firstrow, lastrow)
	} else if (TB_TYPE(tp) == TBL_TYPE_S_COL) {
	    call tbycps (tp, cp, buffer, firstrow, lastrow)
	} else if (TB_TYPE(tp) == TBL_TYPE_TEXT) {
	    call tbzcps (tp, cp, buffer, firstrow, lastrow)
	} else if (TB_TYPE(tp) == TBL_TYPE_FITS) {
	    i = 1
	    do row = firstrow, lastrow {
		call tbfaps (tp, cp, row, buffer[i], 1, 1)
		i = i + 1
	    }
	} else {
	    call error (ER_TBCORRUPTED, "tbcpts:  table type is messed up")
	}

	TB_MODIFIED(tp) = true
end


# tbcptb -- putcol Boolean
# This is for data type boolean.

procedure tbcptb (tp, cp, buffer, sel_firstrow, sel_lastrow)

pointer tp		# i: pointer to table descriptor
pointer cp		# i: pointer to descriptor of the column
bool	buffer[ARB]	# i: array of values to be put into column
int	sel_firstrow	# i: first row into which to put values
int	sel_lastrow	# i: last row into which to put values
#--
int	firstrow, lastrow	# range of actual row numbers
int	row, i
errchk	tbswer, tbxcpb, tbycpb, tbzcpb, tbfapb

begin
	if (TB_READONLY(tp))
	    call error (ER_TBREADONLY, "can't write to table; it's readonly")

	if (sel_lastrow < sel_firstrow)
	    call error (1, "tbcptb:  lastrow is less than firstrow")

	# If we're writing beyond EOF, write extra rows and update TB_NROWS.
	# Also convert to actual row numbers.
	call tbswer (tp, sel_lastrow, lastrow)
	call tbswer (tp, sel_firstrow, firstrow)

	if (TB_TYPE(tp) == TBL_TYPE_S_ROW) {
	    call tbxcpb (tp, cp, buffer, firstrow, lastrow)
	} else if (TB_TYPE(tp) == TBL_TYPE_S_COL) {
	    call tbycpb (tp, cp, buffer, firstrow, lastrow)
	} else if (TB_TYPE(tp) == TBL_TYPE_TEXT) {
	    call tbzcpb (tp, cp, buffer, firstrow, lastrow)
	} else if (TB_TYPE(tp) == TBL_TYPE_FITS) {
	    i = 1
	    do row = firstrow, lastrow {
		call tbfapb (tp, cp, row, buffer[i], 1, 1)
		i = i + 1
	    }
	} else {
	    call error (ER_TBCORRUPTED, "tbcptb:  table type is messed up")
	}

	TB_MODIFIED(tp) = true
end


# tbcptt -- putcol text
# Write values for one column to a range of rows.  This is for character
# strings.

procedure tbcptt (tp, cp, buffer, lenstr, sel_firstrow, sel_lastrow)

pointer tp			# i: pointer to table descriptor
pointer cp			# i: pointer to descriptor of the column
char	buffer[lenstr,ARB]	# i: array of values to be put into column
int	lenstr			# i: number of char in each element of buffer
int	sel_firstrow	# i: first row into which to put values
int	sel_lastrow	# i: last row into which to put values
#--
int	firstrow, lastrow	# range of actual row numbers
int	row, i
errchk	tbswer, tbxcpt, tbycpt, tbzcpt, tbfapt

begin
	if (TB_READONLY(tp))
	    call error (ER_TBREADONLY, "can't write to table; it's readonly")

	if (sel_lastrow < sel_firstrow)
	    call error (1, "tbcptt:  lastrow is less than firstrow")

	# If we're writing beyond EOF, write extra rows and update TB_NROWS.
	# Also convert to actual row numbers.
	call tbswer (tp, sel_lastrow, lastrow)
	call tbswer (tp, sel_firstrow, firstrow)

	if (TB_TYPE(tp) == TBL_TYPE_S_ROW) {
	    call tbxcpt (tp, cp, buffer, lenstr, firstrow, lastrow)
	} else if (TB_TYPE(tp) == TBL_TYPE_S_COL) {
	    call tbycpt (tp, cp, buffer, lenstr, firstrow, lastrow)
	} else if (TB_TYPE(tp) == TBL_TYPE_TEXT) {
	    call tbzcpt (tp, cp, buffer, lenstr, firstrow, lastrow)
	} else if (TB_TYPE(tp) == TBL_TYPE_FITS) {
	    i = 1
	    do row = firstrow, lastrow {
		call tbfapt (tp, cp, row, buffer[1,i], lenstr, 1, 1)
		i = i + 1
	    }
	} else {
	    call error (ER_TBCORRUPTED, "tbcptt:  table type is messed up")
	}

	TB_MODIFIED(tp) = true
end
