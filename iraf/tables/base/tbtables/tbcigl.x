include <mach.h>		# defines SZB_CHAR
include <tbset.h>
include "tbtables.h"
include "tblerr.h"

# tbcigl -- get long integer info about a column
#           (based on tbcigi by Phil Hodge)

long procedure tbcigl (cptr, get_what)

pointer cptr			# i: pointer to column descriptor
int	get_what		# i: indicates what column info to get
#--
long	value			# value that will be returned
char	pformat[SZ_COLFMT]	# format for printing the column
int	ip, ctol()		# for getting lenfmt

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
	    if (ctol (pformat, ip, value) <= 0) {	# no field width?
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
	    call error (ER_TBUNKPARAM, "tbcigl:  unknown parameter")
	}
	return (value)
end
