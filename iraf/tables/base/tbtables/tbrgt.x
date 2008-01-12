include <tbset.h>
include "tbtables.h"
include "tblerr.h"

# Read column values from a row.
#
# Phil Hodge, 28-Dec-1987  Different data types combined into one file.
# Phil Hodge,  5-Feb-1992  Add option for text table type.
# Phil Hodge, 31-Mar-1993  Include short datatype.
# Phil Hodge,  4-Nov-1993  Include check on row number out of range.
# Phil Hodge,  9-Apr-1995  Modify for FITS tables.
# Phil Hodge,  2-Mar-1998  Map selected row number to actual row number.
# Phil Hodge, 18-Jun-1998  Use tbfagi instead of tbfagb to get boolean.
# Phil Hodge, 28-Aug-2002  Use strsearch to check for INDEF in tbrgtt.

# tbrgtd -- getrow double
# Read column values from a row.  This is for data type double.

procedure tbrgtd (tp, cp, buffer, nullflag, numcols, selrow)

pointer tp			# i: pointer to table descriptor
pointer cp[ARB]			# i: array of pointers to column descriptors
double	buffer[ARB]		# o: buffer for values
bool	nullflag[ARB]		# o: true if element is undefined in table
int	numcols			# i: number of columns from which to get values
int	selrow			# i: row number (or selected row number)
#--
int	rownum			# actual row number
int	k			# index into buffer & nullflag
int	nret			# for fits tables
int	tbfagd()
errchk	tbsirow, tbxrgd, tbyrgd, tbzgtd, tbfagd

begin
	call tbsirow (tp, selrow, rownum)

	if (TB_TYPE(tp) == TBL_TYPE_S_ROW) {
	    call tbxrgd (tp, cp, buffer, nullflag, numcols, rownum)
	} else if (TB_TYPE(tp) == TBL_TYPE_S_COL) {
	    call tbyrgd (tp, cp, buffer, nullflag, numcols, rownum)
	} else if (TB_TYPE(tp) == TBL_TYPE_TEXT) {
	    do k = 1, numcols {
		call tbzgtd (tp, cp[k], rownum, buffer[k])
		nullflag[k] = (IS_INDEFD (buffer[k]))
	    }
	} else if (TB_TYPE(tp) == TBL_TYPE_FITS) {
	    do k = 1, numcols {
		nret = tbfagd (tp, cp[k], rownum, buffer[k], 1, 1)
		nullflag[k] = (IS_INDEFD (buffer[k]))
	    }
	} else {
	    call error (ER_TBCORRUPTED, "tbrgtd:  table type is messed up")
	}
end

# tbrgtr -- getrow real
# Read column values from a row.  This is for data type real.

procedure tbrgtr (tp, cp, buffer, nullflag, numcols, selrow)

pointer tp			# i: pointer to table descriptor
pointer cp[ARB]			# i: array of pointers to column descriptors
real	buffer[ARB]		# o: buffer for values
bool	nullflag[ARB]		# o: true if element is undefined in table
int	numcols			# i: number of columns from which to get values
int	selrow			# i: row number (or selected row number)
#--
int	rownum			# actual row number
int	k			# index into buffer & nullflag
int	nret			# for fits tables
int	tbfagr()
errchk	tbsirow, tbxrgr, tbyrgr, tbzgtr, tbfagr

begin
	call tbsirow (tp, selrow, rownum)

	if (TB_TYPE(tp) == TBL_TYPE_S_ROW) {
	    call tbxrgr (tp, cp, buffer, nullflag, numcols, rownum)
	} else if (TB_TYPE(tp) == TBL_TYPE_S_COL) {
	    call tbyrgr (tp, cp, buffer, nullflag, numcols, rownum)
	} else if (TB_TYPE(tp) == TBL_TYPE_TEXT) {
	    do k = 1, numcols {
		call tbzgtr (tp, cp[k], rownum, buffer[k])
		nullflag[k] = (IS_INDEFR (buffer[k]))
	    }
	} else if (TB_TYPE(tp) == TBL_TYPE_FITS) {
	    do k = 1, numcols {
		nret = tbfagr (tp, cp[k], rownum, buffer[k], 1, 1)
		nullflag[k] = (IS_INDEFR (buffer[k]))
	    }
	} else {
	    call error (ER_TBCORRUPTED, "tbrgtr:  table type is messed up")
	}
end

# tbrgti -- getrow integer
# Read column values from a row.  This is for data type integer.

procedure tbrgti (tp, cp, buffer, nullflag, numcols, selrow)

pointer tp			# i: pointer to table descriptor
pointer cp[ARB]			# i: array of pointers to column descriptors
int	buffer[ARB]		# o: buffer for values
bool	nullflag[ARB]		# o: true if element is undefined in table
int	numcols			# i: number of columns from which to get values
int	selrow			# i: row number (or selected row number)
#--
int	rownum			# actual row number
int	k			# index into buffer & nullflag
int	nret			# for fits tables
int	tbfagi()
errchk	tbsirow, tbxrgi, tbyrgi, tbzgti, tbfagi

begin
	call tbsirow (tp, selrow, rownum)

	if (TB_TYPE(tp) == TBL_TYPE_S_ROW) {
	    call tbxrgi (tp, cp, buffer, nullflag, numcols, rownum)
	} else if (TB_TYPE(tp) == TBL_TYPE_S_COL) {
	    call tbyrgi (tp, cp, buffer, nullflag, numcols, rownum)
	} else if (TB_TYPE(tp) == TBL_TYPE_TEXT) {
	    do k = 1, numcols {
		call tbzgti (tp, cp[k], rownum, buffer[k])
		nullflag[k] = (IS_INDEFI (buffer[k]))
	    }
	} else if (TB_TYPE(tp) == TBL_TYPE_FITS) {
	    do k = 1, numcols {
		nret = tbfagi (tp, cp[k], rownum, buffer[k], 1, 1)
		nullflag[k] = (IS_INDEFI (buffer[k]))
	    }
	} else {
	    call error (ER_TBCORRUPTED, "tbrgti:  table type is messed up")
	}
end

# tbrgts -- getrow short
# Read column values from a row.  This is for data type short integer.

procedure tbrgts (tp, cp, buffer, nullflag, numcols, selrow)

pointer tp			# i: pointer to table descriptor
pointer cp[ARB]			# i: array of pointers to column descriptors
short	buffer[ARB]		# o: buffer for values
bool	nullflag[ARB]		# o: true if element is undefined in table
int	numcols			# i: number of columns from which to get values
int	selrow			# i: row number (or selected row number)
#--
int	rownum			# actual row number
int	k			# index into buffer & nullflag
int	nret			# for fits tables
int	tbfags()
errchk	tbsirow, tbxrgs, tbyrgs, tbzgts, tbfags

begin
	call tbsirow (tp, selrow, rownum)

	if (TB_TYPE(tp) == TBL_TYPE_S_ROW) {
	    call tbxrgs (tp, cp, buffer, nullflag, numcols, rownum)
	} else if (TB_TYPE(tp) == TBL_TYPE_S_COL) {
	    call tbyrgs (tp, cp, buffer, nullflag, numcols, rownum)
	} else if (TB_TYPE(tp) == TBL_TYPE_TEXT) {
	    do k = 1, numcols {
		call tbzgts (tp, cp[k], rownum, buffer[k])
		nullflag[k] = (IS_INDEFS (buffer[k]))
	    }
	} else if (TB_TYPE(tp) == TBL_TYPE_FITS) {
	    do k = 1, numcols {
		nret = tbfags (tp, cp[k], rownum, buffer[k], 1, 1)
		nullflag[k] = (IS_INDEFS (buffer[k]))
	    }
	} else {
	    call error (ER_TBCORRUPTED, "tbrgts:  table type is messed up")
	}
end

# tbrgtb -- getrow Boolean
# This is for data type Boolean.

procedure tbrgtb (tp, cp, buffer, nullflag, numcols, selrow)

pointer tp			# i: pointer to table descriptor
pointer cp[ARB]			# i: array of pointers to column descriptors
bool	buffer[ARB]		# o: buffer for values
bool	nullflag[ARB]		# o: true if element is undefined in table
int	numcols			# i: number of columns from which to get values
int	selrow			# i: row number (or selected row number)
#--
int	rownum			# actual row number
int	k			# index into buffer & nullflag
int	nret			# for fits tables
int	ival			# for getting from a fits table
int	tbfagi()
errchk	tbsirow, tbxrgb, tbyrgb, tbzgtb, tbfagi

begin
	call tbsirow (tp, selrow, rownum)

	if (TB_TYPE(tp) == TBL_TYPE_S_ROW) {
	    call tbxrgb (tp, cp, buffer, nullflag, numcols, rownum)
	} else if (TB_TYPE(tp) == TBL_TYPE_S_COL) {
	    call tbyrgb (tp, cp, buffer, nullflag, numcols, rownum)
	} else if (TB_TYPE(tp) == TBL_TYPE_TEXT) {
	    do k = 1, numcols {
		call tbzgtb (tp, cp[k], rownum, buffer[k])
		nullflag[k] = false
	    }
	} else if (TB_TYPE(tp) == TBL_TYPE_FITS) {
	    do k = 1, numcols {
		nret = tbfagi (tp, cp[k], rownum, ival, 1, 1)
		if (IS_INDEFI(ival)) {
		    buffer[k] = false
		    nullflag[k] = true
		} else {
		    buffer[k] = (ival == 1)
		    nullflag[k] = false
		}
	    }
	} else {
	    call error (ER_TBCORRUPTED, "tbrgtb:  table type is messed up")
	}
end

# tbrgtt -- getrow text
# Read column values from a row.  This is for character strings.

procedure tbrgtt (tp, cp, buffer, nullflag, lenstr, numcols, selrow)

pointer tp			# i: pointer to table descriptor
pointer cp[ARB]			# i: array of pointers to column descriptors
char	buffer[lenstr,ARB]	# o: buffer for values
bool	nullflag[ARB]		# o: true if element is undefined in table
int	lenstr			# i: length of each string in array buffer
int	numcols			# i: number of columns from which to get values
int	selrow			# i: row number (or selected row number)
#--
int	rownum			# actual row number
int	k			# index into buffer & nullflag
int	nret			# for fits tables
int	tbfagt()
int	strsearch()
errchk	tbxrgt, tbyrgt, tbzgtt, tbfagt

begin
	call tbsirow (tp, selrow, rownum)

	if (TB_TYPE(tp) == TBL_TYPE_S_ROW) {
	    call tbxrgt (tp, cp, buffer, nullflag,
				lenstr, numcols, rownum)
	} else if (TB_TYPE(tp) == TBL_TYPE_S_COL) {
	    call tbyrgt (tp, cp, buffer, nullflag,
				lenstr, numcols, rownum)
	} else if (TB_TYPE(tp) == TBL_TYPE_TEXT) {
	    do k = 1, numcols {
		call tbzgtt (tp, cp[k], rownum, buffer[1,k], lenstr)
		nullflag[k] = (buffer[1,k] == EOS ||
			(strsearch (buffer[1,k], "INDEF") > 0))
	    }
	} else if (TB_TYPE(tp) == TBL_TYPE_FITS) {
	    do k = 1, numcols {
		nret = tbfagt (tp, cp[k], rownum, buffer[1,k], lenstr, 1, 1)
		nullflag[k] = (buffer[1,k] == EOS ||
			(strsearch (buffer[1,k], "INDEF") > 0))
	    }
	} else {
	    call error (ER_TBCORRUPTED, "tbrgtt:  table type is messed up")
	}
end
