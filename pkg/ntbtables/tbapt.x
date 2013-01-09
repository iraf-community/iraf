include <tbset.h>
include "tbtables.h"

# tbapt[tbirds] -- put an array of values
#
# Phil Hodge, 12-Sep-1994  Subroutines created.
# Phil Hodge,  3-Apr-1995  Set TB_MODIFIED to true.
# Phil Hodge, 14-Jun-1995  Modify for FITS tables.
# Phil Hodge,  3-Mar-1998  Call tbswer1, to allow for row selector.

procedure tbaptd (tp, cp, selrow, buffer, first, nelem)

pointer tp		# i: pointer to table struct
pointer cp		# i: pointer to column struct
int	selrow		# i: row number (or selected row number)
double	buffer[ARB]	# i: values to write to table
int	first		# i: number of first array element to write
int	nelem		# i: number of elements to write
#--
int	row		# actual row number
errchk	tbeptd, tbfapd, tbswer1, tbxapd, tbwapd

begin
	if (selrow < 1 || first < 1)
	    call error (1, "tbaptd:  invalid row or element number")

	if (nelem < 1)
	    return

	if (first == 1 && nelem == 1) {
	    call tbeptd (tp, cp, selrow, buffer)
	} else {
	    call tbswer1 (tp, selrow, row)
	    if (TB_TYPE(tp) == TBL_TYPE_S_ROW)
		call tbxapd (tp, cp, row, buffer, first, nelem)
	    else if (TB_TYPE(tp) == TBL_TYPE_FITS)
		call tbfapd (tp, cp, row, buffer, first, nelem)
	    else
		call error (1, "can't write an array to this type of table")
	}

	TB_MODIFIED(tp) = true
end

procedure tbaptr (tp, cp, selrow, buffer, first, nelem)

pointer tp		# i: pointer to table struct
pointer cp		# i: pointer to column struct
int	selrow		# i: row number (or selected row number)
real	buffer[ARB]	# i: values to write to table
int	first		# i: number of first array element to write
int	nelem		# i: number of elements to write
#--
int	row		# actual row number
errchk	tbeptr, tbfapr, tbswer1, tbxapr, tbwapr

begin
	if (selrow < 1 || first < 1)
	    call error (1, "tbaptr:  invalid row or element number")

	if (nelem < 1)
	    return

	if (first == 1 && nelem == 1) {
	    call tbeptr (tp, cp, selrow, buffer)
	} else {
	    call tbswer1 (tp, selrow, row)
	    if (TB_TYPE(tp) == TBL_TYPE_S_ROW)
		call tbxapr (tp, cp, row, buffer, first, nelem)
	    else if (TB_TYPE(tp) == TBL_TYPE_FITS)
		call tbfapr (tp, cp, row, buffer, first, nelem)
	    else
		call error (1, "can't write an array to this type of table")
	}

	TB_MODIFIED(tp) = true
end

procedure tbapti (tp, cp, selrow, buffer, first, nelem)

pointer tp		# i: pointer to table struct
pointer cp		# i: pointer to column struct
int	selrow		# i: row number (or selected row number)
int	buffer[ARB]	# i: values to write to table
int	first		# i: number of first array element to write
int	nelem		# i: number of elements to write
#--
int	row		# actual row number
errchk	tbepti, tbfapi, tbswer1, tbxapi, tbwapi

begin
	if (selrow < 1 || first < 1)
	    call error (1, "tbapti:  invalid row or element number")

	if (nelem < 1)
	    return

	if (first == 1 && nelem == 1) {
	    call tbepti (tp, cp, selrow, buffer)
	} else {
	    call tbswer1 (tp, selrow, row)
	    if (TB_TYPE(tp) == TBL_TYPE_S_ROW)
		call tbxapi (tp, cp, row, buffer, first, nelem)
	    else if (TB_TYPE(tp) == TBL_TYPE_FITS)
		call tbfapi (tp, cp, row, buffer, first, nelem)
	    else
		call error (1, "can't write an array to this type of table")
	}

	TB_MODIFIED(tp) = true
end

procedure tbapts (tp, cp, selrow, buffer, first, nelem)

pointer tp		# i: pointer to table struct
pointer cp		# i: pointer to column struct
int	selrow		# i: row number (or selected row number)
short	buffer[ARB]	# i: values to write to table
int	first		# i: number of first array element to write
int	nelem		# i: number of elements to write
#--
int	row		# actual row number
errchk	tbepts, tbfaps, tbswer1, tbxaps, tbwaps

begin
	if (selrow < 1 || first < 1)
	    call error (1, "tbapts:  invalid row or element number")

	if (nelem < 1)
	    return

	if (first == 1 && nelem == 1) {
	    call tbepts (tp, cp, selrow, buffer)
	} else {
	    call tbswer1 (tp, selrow, row)
	    if (TB_TYPE(tp) == TBL_TYPE_S_ROW)
		call tbxaps (tp, cp, row, buffer, first, nelem)
	    else if (TB_TYPE(tp) == TBL_TYPE_FITS)
		call tbfaps (tp, cp, row, buffer, first, nelem)
	    else
		call error (1, "can't write an array to this type of table")
	}

	TB_MODIFIED(tp) = true
end

procedure tbaptb (tp, cp, selrow, buffer, first, nelem)

pointer tp		# i: pointer to table struct
pointer cp		# i: pointer to column struct
int	selrow		# i: row number (or selected row number)
bool	buffer[ARB]	# i: values to write to table
int	first		# i: number of first array element to write
int	nelem		# i: number of elements to write
#--
int	row		# actual row number
errchk	tbeptb, tbfapb, tbswer1, tbxapb, tbwapb

begin
	if (selrow < 1 || first < 1)
	    call error (1, "tbaptb:  invalid row or element number")

	if (nelem < 1)
	    return

	if (first == 1 && nelem == 1) {
	    call tbeptb (tp, cp, selrow, buffer)
	} else {
	    call tbswer1 (tp, selrow, row)
	    if (TB_TYPE(tp) == TBL_TYPE_S_ROW)
		call tbxapb (tp, cp, row, buffer, first, nelem)
	    else if (TB_TYPE(tp) == TBL_TYPE_FITS)
		call tbfapb (tp, cp, row, buffer, first, nelem)
	    else
		call error (1, "can't write an array to this type of table")
	}

	TB_MODIFIED(tp) = true
end

procedure tbaptt (tp, cp, selrow, cbuf, maxch, first, nelem)

pointer tp		# i: pointer to table struct
pointer cp		# i: pointer to column struct
int	selrow		# i: row number (or selected row number)
char	cbuf[maxch,ARB]	# i: values to write to table
int	maxch		# i: size of first dimension of cbuf
int	first		# i: number of first array element to write
int	nelem		# i: number of elements to write
#--
int	row		# actual row number
errchk	tbeptt, tbfapt, tbswer1, tbxapt, tbwapt

begin
	if (selrow < 1 || first < 1)
	    call error (1, "tbaptt:  invalid row or element number")

	if (nelem < 1)
	    return

	if (first == 1 && nelem == 1) {
	    call tbeptt (tp, cp, selrow, cbuf)
	} else {
	    call tbswer1 (tp, selrow, row)
	    if (TB_TYPE(tp) == TBL_TYPE_S_ROW)
		call tbxapt (tp, cp, row, cbuf, maxch, first, nelem)
	    else if (TB_TYPE(tp) == TBL_TYPE_FITS)
		call tbfapt (tp, cp, row, cbuf, maxch, first, nelem)
	    else
		call error (1, "can't write an array to this type of table")
	}

	TB_MODIFIED(tp) = true
end
