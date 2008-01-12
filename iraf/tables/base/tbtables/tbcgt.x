include <tbset.h>
include "tbtables.h"
include "tblerr.h"

# Read values for one column from a range of rows.
#
# Phil Hodge, 28-Dec-1987  Different data types combined into one file.
# Phil Hodge,  3-Feb-1992  Add option for text table type.
# Phil Hodge, 31-Mar-1993  Include short datatype.
# Phil Hodge,  4-Nov-1993  Include check on row number less than one.
# Phil Hodge, 17-May-1995  Change declaration of buffer in tbcgtt to 2-D array.
# Phil Hodge,  9-Apr-1995  Modify for FITS tables.
# Phil Hodge,  2-Mar-1998  Map selected row number to actual row number.
# Phil Hodge, 18-Jun-1998  Use tbfagi instead of tbfagb to get boolean.
# Phil Hodge, 28-Aug-2002  Use strsearch to check for INDEF in tbcgtt.

# tbcgtd -- getcol double
# Read values for one column from a range of rows.  This is for data type
# double precision.

procedure tbcgtd (tp, cp, buffer, nullflag, sel_firstrow, sel_lastrow)

pointer tp			# i: pointer to table descriptor
pointer cp			# i: pointer to descriptor of the column
double	buffer[ARB]		# o: buffer for values
bool	nullflag[ARB]		# o: true if element is undefined in table
int	sel_firstrow		# i: first row from which to get values
int	sel_lastrow		# i: last row from which to get values
#--
int	firstrow, lastrow	# actual range of row numbers
int	i, row			# loop indexes
int	nret			# for fits tables
int	tbfagd()
errchk	tbsirow, tbxcgd, tbycgd, tbzcgd, tbfagd

begin
	call tbsirow (tp, sel_firstrow, firstrow)
	call tbsirow (tp, sel_lastrow, lastrow)

	if (TB_TYPE(tp) == TBL_TYPE_S_ROW) {
	    call tbxcgd (tp, cp, buffer, nullflag, firstrow, lastrow)
	} else if (TB_TYPE(tp) == TBL_TYPE_S_COL) {
	    call tbycgd (tp, cp, buffer, nullflag, firstrow, lastrow)
	} else if (TB_TYPE(tp) == TBL_TYPE_TEXT) {
	    call tbzcgd (tp, cp, buffer, nullflag, firstrow, lastrow)
	} else if (TB_TYPE(tp) == TBL_TYPE_FITS) {
	    i = 1
	    do row = firstrow, lastrow {
		nret = tbfagd (tp, cp, row, buffer[i], 1, 1)
		nullflag[i] = (IS_INDEFD (buffer[i]))
		i = i + 1
	    }
	} else {
	    call error (ER_TBCORRUPTED, "tbcgtd:  table type is messed up")
	}
end

# tbcgtr -- getcol real
# Read values for one column from a range of rows.  This is for data type real.

procedure tbcgtr (tp, cp, buffer, nullflag, sel_firstrow, sel_lastrow)

pointer tp			# i: pointer to table descriptor
pointer cp			# i: pointer to descriptor of the column
real	buffer[ARB]		# o: buffer for values
bool	nullflag[ARB]		# o: true if element is undefined in table
int	sel_firstrow		# i: first row from which to get values
int	sel_lastrow		# i: last row from which to get values
#--
int	firstrow, lastrow	# actual range of row numbers
int	i, row			# loop indexes
int	nret			# for fits tables
int	tbfagr()
errchk	tbsirow, tbxcgr, tbycgr, tbzcgr, tbfagr

begin
	call tbsirow (tp, sel_firstrow, firstrow)
	call tbsirow (tp, sel_lastrow, lastrow)

	if (TB_TYPE(tp) == TBL_TYPE_S_ROW) {
	    call tbxcgr (tp, cp, buffer, nullflag, firstrow, lastrow)
	} else if (TB_TYPE(tp) == TBL_TYPE_S_COL) {
	    call tbycgr (tp, cp, buffer, nullflag, firstrow, lastrow)
	} else if (TB_TYPE(tp) == TBL_TYPE_TEXT) {
	    call tbzcgr (tp, cp, buffer, nullflag, firstrow, lastrow)
	} else if (TB_TYPE(tp) == TBL_TYPE_FITS) {
	    i = 1
	    do row = firstrow, lastrow {
		nret = tbfagr (tp, cp, row, buffer[i], 1, 1)
		nullflag[i] = (IS_INDEFR (buffer[i]))
		i = i + 1
	    }
	} else {
	    call error (ER_TBCORRUPTED, "tbcgtr:  table type is messed up")
	}
end

# tbcgti -- getcol integer
# Read values for one column from a range of rows.  This is for data type
# integer.

procedure tbcgti (tp, cp, buffer, nullflag, sel_firstrow, sel_lastrow)

pointer tp			# i: pointer to table descriptor
pointer cp			# i: pointer to descriptor of the column
int	buffer[ARB]		# o: buffer for values
bool	nullflag[ARB]		# o: true if element is undefined in table
int	sel_firstrow		# i: first row from which to get values
int	sel_lastrow		# i: last row from which to get values
#--
int	firstrow, lastrow	# actual range of row numbers
int	i, row			# loop indexes
int	nret			# for fits tables
int	tbfagi()
errchk	tbsirow, tbxcgi, tbycgi, tbzcgi, tbfagi

begin
	call tbsirow (tp, sel_firstrow, firstrow)
	call tbsirow (tp, sel_lastrow, lastrow)

	if (TB_TYPE(tp) == TBL_TYPE_S_ROW) {
	    call tbxcgi (tp, cp, buffer, nullflag, firstrow, lastrow)
	} else if (TB_TYPE(tp) == TBL_TYPE_S_COL) {
	    call tbycgi (tp, cp, buffer, nullflag, firstrow, lastrow)
	} else if (TB_TYPE(tp) == TBL_TYPE_TEXT) {
	    call tbzcgi (tp, cp, buffer, nullflag, firstrow, lastrow)
	} else if (TB_TYPE(tp) == TBL_TYPE_FITS) {
	    i = 1
	    do row = firstrow, lastrow {
		nret = tbfagi (tp, cp, row, buffer[i], 1, 1)
		nullflag[i] = (IS_INDEFI (buffer[i]))
		i = i + 1
	    }
	} else {
	    call error (ER_TBCORRUPTED, "tbcgti:  table type is messed up")
	}
end

# tbcgts -- getcol short
# Read values for one column from a range of rows.  This is for data type
# short integer.

procedure tbcgts (tp, cp, buffer, nullflag, sel_firstrow, sel_lastrow)

pointer tp			# i: pointer to table descriptor
pointer cp			# i: pointer to descriptor of the column
short	buffer[ARB]		# o: buffer for values
bool	nullflag[ARB]		# o: true if element is undefined in table
int	sel_firstrow		# i: first row from which to get values
int	sel_lastrow		# i: last row from which to get values
#--
int	firstrow, lastrow	# actual range of row numbers
int	i, row			# loop indexes
int	nret			# for fits tables
int	tbfags()
errchk	tbsirow, tbxcgs, tbycgs, tbzcgs, tbfags

begin
	call tbsirow (tp, sel_firstrow, firstrow)
	call tbsirow (tp, sel_lastrow, lastrow)

	if (TB_TYPE(tp) == TBL_TYPE_S_ROW) {
	    call tbxcgs (tp, cp, buffer, nullflag, firstrow, lastrow)
	} else if (TB_TYPE(tp) == TBL_TYPE_S_COL) {
	    call tbycgs (tp, cp, buffer, nullflag, firstrow, lastrow)
	} else if (TB_TYPE(tp) == TBL_TYPE_TEXT) {
	    call tbzcgs (tp, cp, buffer, nullflag, firstrow, lastrow)
	} else if (TB_TYPE(tp) == TBL_TYPE_FITS) {
	    i = 1
	    do row = firstrow, lastrow {
		nret = tbfags (tp, cp, row, buffer[i], 1, 1)
		nullflag[i] = (IS_INDEFS (buffer[i]))
		i = i + 1
	    }
	} else {
	    call error (ER_TBCORRUPTED, "tbcgts:  table type is messed up")
	}
end

# tbcgtb -- getcol Boolean
# This is for data type Boolean.

procedure tbcgtb (tp, cp, buffer, nullflag, sel_firstrow, sel_lastrow)

pointer tp			# i: pointer to table descriptor
pointer cp			# i: pointer to descriptor of the column
bool	buffer[ARB]		# o: buffer for values
bool	nullflag[ARB]		# o: true if element is undefined in table
int	sel_firstrow		# i: first row from which to get values
int	sel_lastrow		# i: last row from which to get values
#--
int	firstrow, lastrow	# actual range of row numbers
int	i, row			# loop indexes
int	nret			# for fits tables
int	ival			# for getting from a fits table
int	tbfagi()
errchk	tbsirow, tbxcgb, tbycgb, tbzcgb, tbfagi

begin
	call tbsirow (tp, sel_firstrow, firstrow)
	call tbsirow (tp, sel_lastrow, lastrow)

	if (TB_TYPE(tp) == TBL_TYPE_S_ROW) {
	    call tbxcgb (tp, cp, buffer, nullflag, firstrow, lastrow)
	} else if (TB_TYPE(tp) == TBL_TYPE_S_COL) {
	    call tbycgb (tp, cp, buffer, nullflag, firstrow, lastrow)
	} else if (TB_TYPE(tp) == TBL_TYPE_TEXT) {
	    call tbzcgb (tp, cp, buffer, nullflag, firstrow, lastrow)
	} else if (TB_TYPE(tp) == TBL_TYPE_FITS) {
	    i = 1
	    do row = firstrow, lastrow {
		nret = tbfagi (tp, cp, row, ival, 1, 1)
		if (IS_INDEFI(ival)) {
		    buffer[i] = false
		    nullflag[i] = true
		} else {
		    buffer[i] = (ival == 1)
		    nullflag[i] = false
		}
		i = i + 1
	    }
	} else {
	    call error (ER_TBCORRUPTED, "tbcgtb:  table type is messed up")
	}
end

# tbcgtt -- getcol text
# Read values for one column from a range of rows.  This is for character
# strings.

procedure tbcgtt (tp, cp, buffer, nullflag, lenstr, sel_firstrow, sel_lastrow)

pointer tp			# i: pointer to table descriptor
pointer cp			# i: pointer to descriptor of the column
char	buffer[lenstr,ARB]	# o: buffer for values
bool	nullflag[ARB]		# o: true if element is undefined in table
int	lenstr			# i: length of each element of buffer
int	sel_firstrow		# i: first row from which to get values
int	sel_lastrow		# i: last row from which to get values
#--
int	firstrow, lastrow	# actual range of row numbers
int	i, row			# loop indexes
int	nret			# for fits tables
int	tbfagt()
int	strsearch()
errchk	tbsirow, tbxcgt, tbycgt, tbzcgt, tbfagt

begin
	call tbsirow (tp, sel_firstrow, firstrow)
	call tbsirow (tp, sel_lastrow, lastrow)

	if (TB_TYPE(tp) == TBL_TYPE_S_ROW) {
	    call tbxcgt (tp, cp, buffer, nullflag, lenstr,
			firstrow, lastrow)
	} else if (TB_TYPE(tp) == TBL_TYPE_S_COL) {
	    call tbycgt (tp, cp, buffer, nullflag, lenstr,
			firstrow, lastrow)
	} else if (TB_TYPE(tp) == TBL_TYPE_TEXT) {
	    call tbzcgt (tp, cp, buffer, nullflag, lenstr,
			firstrow, lastrow)
	} else if (TB_TYPE(tp) == TBL_TYPE_FITS) {
	    i = 1
	    do row = firstrow, lastrow {
		nret = tbfagt (tp, cp, row, buffer[1,i], lenstr, 1, 1)
		nullflag[i] = (buffer[1,i] == EOS ||
			(strsearch (buffer[1,i], "INDEF") > 0))
		i = i + 1
	    }
	} else {
	    call error (ER_TBCORRUPTED, "tbcgtt:  table type is messed up")
	}
end
