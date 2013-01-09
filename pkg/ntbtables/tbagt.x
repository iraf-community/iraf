include <mach.h>		# for MAX_INT and MAX_SHORT
include <tbset.h>
include "tbtables.h"

# tbagt[tbirds] -- get an array of values
#
# Phil Hodge, 12-Sep-1994  Subroutines created.
# Phil Hodge,  9-Jun-1995  Modify for FITS tables.
# Phil Hodge,  2-Mar-1998  Map selected row number to actual row number.

int procedure tbagtd (tp, cp, selrow, buffer, first, nelem)

pointer tp		# i: pointer to table struct
pointer cp		# i: pointer to column struct
int	selrow		# i: row number (or selected row number)
double	buffer[ARB]	# o: values read from table
int	first		# i: number of first array element to read
int	nelem		# i: maximum number of elements to read
#--
int	row		# actual row number
int	nret		# actual number of elements read
int	tbxagd(), tbfagd()
errchk	tbsirow, tbegtd, tbfagd, tbxagd

begin
	if (selrow < 1 || first < 1)
	    call error (1, "tbagtd:  invalid row or element number")

	if (nelem < 1)
	    return (0)

	if (first == 1 && nelem == 1) {
	    call tbegtd (tp, cp, selrow, buffer)
	    return (1)
	}

	call tbsirow (tp, selrow, row)

	if (TB_TYPE(tp) == TBL_TYPE_S_ROW)
	    nret = tbxagd (tp, cp, row, buffer, first, nelem)
	else if (TB_TYPE(tp) == TBL_TYPE_FITS)
	    nret = tbfagd (tp, cp, row, buffer, first, nelem)
	else
	    call error (1, "can't read an array from this type of table")

	return (nret)
end

int procedure tbagtr (tp, cp, selrow, buffer, first, nelem)

pointer tp		# i: pointer to table struct
pointer cp		# i: pointer to column struct
int	selrow		# i: row number (or selected row number)
real	buffer[ARB]	# o: values read from table
int	first		# i: number of first array element to read
int	nelem		# i: maximum number of elements to read
#--
int	row		# actual row number
int	nret		# actual number of elements read
int	tbxagr(), tbfagr()
errchk	tbsirow, tbegtr, tbfagr, tbxagr, tbxagr

begin
	if (selrow < 1 || first < 1)
	    call error (1, "tbagtr:  invalid row or element number")

	if (nelem < 1)
	    return (0)

	if (first == 1 && nelem == 1) {
	    call tbegtr (tp, cp, selrow, buffer)
	    return (1)
	}

	call tbsirow (tp, selrow, row)

	if (TB_TYPE(tp) == TBL_TYPE_S_ROW)
	    nret = tbxagr (tp, cp, row, buffer, first, nelem)
	else if (TB_TYPE(tp) == TBL_TYPE_FITS)
	    nret = tbfagr (tp, cp, row, buffer, first, nelem)
	else
	    call error (1, "can't read an array from this type of table")

	return (nret)
end

int procedure tbagti (tp, cp, selrow, buffer, first, nelem)

pointer tp		# i: pointer to table struct
pointer cp		# i: pointer to column struct
int	selrow		# i: row number (or selected row number)
int	buffer[ARB]	# o: values read from table
int	first		# i: number of first array element to read
int	nelem		# i: maximum number of elements to read
#--
int	row		# actual row number
int	nret		# actual number of elements read
int	tbxagi(), tbfagi()
errchk	tbsirow, tbegti, tbfagi, tbxagi

begin
	if (selrow < 1 || first < 1)
	    call error (1, "tbagti:  invalid row or element number")

	if (nelem < 1)
	    return (0)

	if (first == 1 && nelem == 1) {
	    call tbegti (tp, cp, selrow, buffer)
	    return (1)
	}

	call tbsirow (tp, selrow, row)

	if (TB_TYPE(tp) == TBL_TYPE_S_ROW)
	    nret = tbxagi (tp, cp, row, buffer, first, nelem)
	else if (TB_TYPE(tp) == TBL_TYPE_FITS)
	    nret = tbfagi (tp, cp, row, buffer, first, nelem)
	else
	    call error (1, "can't read an array from this type of table")

	return (nret)
end

int procedure tbagts (tp, cp, selrow, buffer, first, nelem)

pointer tp		# i: pointer to table struct
pointer cp		# i: pointer to column struct
int	selrow		# i: row number (or selected row number)
short	buffer[ARB]	# o: values read from table
int	first		# i: number of first array element to read
int	nelem		# i: maximum number of elements to read
#--
int	row		# actual row number
int	nret		# actual number of elements read
int	tbxags(), tbfags()
errchk	tbsirow, tbegts, tbfags, tbxags

begin
	if (selrow < 1 || first < 1)
	    call error (1, "tbagts:  invalid row or element number")

	if (nelem < 1)
	    return (0)

	if (first == 1 && nelem == 1) {
	    call tbegts (tp, cp, selrow, buffer)
	    return (1)
	}

	call tbsirow (tp, selrow, row)

	if (TB_TYPE(tp) == TBL_TYPE_S_ROW)
	    nret = tbxags (tp, cp, row, buffer, first, nelem)
	else if (TB_TYPE(tp) == TBL_TYPE_FITS)
	    nret = tbfags (tp, cp, row, buffer, first, nelem)
	else
	    call error (1, "can't read an array from this type of table")

	return (nret)
end

int procedure tbagtb (tp, cp, selrow, buffer, first, nelem)

pointer tp		# i: pointer to table struct
pointer cp		# i: pointer to column struct
int	selrow		# i: row number (or selected row number)
bool	buffer[ARB]	# o: values read from table
int	first		# i: number of first array element to read
int	nelem		# i: maximum number of elements to read
#--
int	row		# actual row number
int	nret		# actual number of elements read
int	tbxagb(), tbfagb()
errchk	tbsirow, tbegtb, tbfagb, tbxagb

begin
	if (selrow < 1 || first < 1)
	    call error (1, "tbagtb:  invalid row or element number")

	if (nelem < 1)
	    return (0)

	if (first == 1 && nelem == 1) {
	    call tbegtb (tp, cp, selrow, buffer)
	    return (1)
	}

	call tbsirow (tp, selrow, row)

	if (TB_TYPE(tp) == TBL_TYPE_S_ROW)
	    nret = tbxagb (tp, cp, row, buffer, first, nelem)
	else if (TB_TYPE(tp) == TBL_TYPE_FITS)
	    nret = tbfagb (tp, cp, row, buffer, first, nelem)
	else
	    call error (1, "can't read an array from this type of table")

	return (nret)
end

int procedure tbagtt (tp, cp, selrow, cbuf, maxch, first, nelem)

pointer tp		# i: pointer to table struct
pointer cp		# i: pointer to column struct
int	selrow		# i: row number (or selected row number)
char	cbuf[maxch,ARB]	# o: values read from table
int	maxch		# i: size of first dimension of cbuf
int	first		# i: number of first array element to read
int	nelem		# i: maximum number of elements to read
#--
int	row		# actual row number
int	nret		# actual number of elements read
int	tbxagt(), tbfagt()
errchk	tbsirow, tbegtt, tbfagt, tbxagt

begin
	if (selrow < 1 || first < 1)
	    call error (1, "tbagtt:  invalid row or element number")

	if (nelem < 1)
	    return (0)

	if (first == 1 && nelem == 1) {
	    call tbegtt (tp, cp, selrow, cbuf, maxch)
	    return (1)
	}

	call tbsirow (tp, selrow, row)

	if (TB_TYPE(tp) == TBL_TYPE_S_ROW)
	    nret = tbxagt (tp, cp, row, cbuf, maxch, first, nelem)
	else if (TB_TYPE(tp) == TBL_TYPE_FITS)
	    nret = tbfagt (tp, cp, row, cbuf, maxch, first, nelem)
	else
	    call error (1, "can't read an array from this type of table")

	return (nret)
end
