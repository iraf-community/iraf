include <mach.h>		# defines SZB_CHAR
include <tbset.h>
include "tbtables.h"
include "tblerr.h"

# tbcigi -- get integer info about a column
# This function returns information of type integer about a column:
# either the column number, data type, length of its print format,
# or the number of elements if it's an array.
# The corresponding routine for text-string information is tbcigt.
#
# Phil Hodge,  2-Oct-1987  Subroutine created
# Phil Hodge,  7-Mar-1989  Check for TBL_TY_REAL, etc for data type.
# Phil Hodge, 30-Mar-1993  Include short datatype.
# Phil Hodge,  2-Aug-1994  Return actual array length using tbalen.
# Phil Hodge, 29-Apr-1997  TBL_COL_FMTLEN for format "%s" is string length.
# Phil Hodge, 14-Apr-1998  Use strcpy instead of strpak.
# Phil Hodge,  5-Aug-1999  For TBL_COL_LENDATA, use COL_NELEM to get the
#		array length instead of calling tbalen.

int procedure tbcigi (cptr, get_what)

pointer cptr			# i: pointer to column descriptor
int	get_what		# i: indicates what column info to get
#--
int	value			# value that will be returned
char	pformat[SZ_COLFMT]	# format for printing the column
int	ip, ctoi()		# for getting lenfmt

begin
	switch (get_what) {
	case TBL_COL_DATATYPE:			# get data type of column
	    switch (COL_DTYPE(cptr)) {
	    case TBL_TY_REAL:
		value = TY_REAL
	    case TBL_TY_DOUBLE:
		value = TY_DOUBLE
	    case TBL_TY_INT:
		value = TY_INT
	    case TBL_TY_SHORT:
		value = TY_SHORT
	    case TBL_TY_BOOL:
		value = TY_BOOL
	    case TBL_TY_CHAR:
		value = -COL_LEN(cptr) * SZB_CHAR
	    default:
		value = COL_DTYPE(cptr)
	    }
	case TBL_COL_FMTLEN:			# get length for printing
	    call strcpy (COL_FMT(cptr), pformat, SZ_COLFMT)
	    ip = 1
	    if (pformat[ip] == '%')
		ip = ip + 1		# skip over the leading '%'
	    if (pformat[ip] == '-')
		ip = ip + 1		# skip over the minus sign
	    if (ctoi (pformat, ip, value) <= 0) {	# no field width?
		if (COL_DTYPE(cptr) < 0)
		    value = COL_LEN(cptr) * SZB_CHAR	# max string length
		else
		    value = 25			# a default value
	    }
	case TBL_COL_NUMBER:			# get column number
	    value = COL_NUMBER(cptr)
	case TBL_COL_LENDATA:			# get length of data array
	    value = COL_NELEM(cptr)
	default:
	    call error (ER_TBUNKPARAM, "tbcigi:  unknown parameter")
	}
	return (value)
end
