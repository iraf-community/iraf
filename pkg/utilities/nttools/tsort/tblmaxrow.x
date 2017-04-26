include	<tbset.h>

# TBL_MAXROW -- Compute the number of rows that fit in a buffer
#
# B.Simon	06-Mar-91	First Code
# B.Simon	15-Mar-00	Revised calculation of maxrow
# B.Simon	24-May-00	Temporary patch to avoid fits problem
# B.Simon	26-May-00	Restored old version after fix to fits problem
# Phil Hodge	10-Sep-04	Set lower limit of 5 on maxrow, because it
#				could be unreasonably small (even zero, due
#				to truncation).
# Phil Hodge	15-Sep-04	Reduce buffer size (maxsize) for STSDAS-format
#				table, because actual buffer size appears to
#				be smaller than the value returned by tbpsta().

int procedure tbl_maxrow (tp)

pointer	tp		# i: table pointer
#--
int	tabtype, maxsize, maxrow
int	tbpsta()

begin
	tabtype = tbpsta (tp, TBL_WHTYPE)

	if (tabtype == TBL_TYPE_S_COL || tabtype == TBL_TYPE_TEXT) {

	    maxrow = tbpsta (tp, TBL_NROWS)

	} else {
	    maxsize = tbpsta (tp, TBL_BUFSIZE)
	    if (tabtype == TBL_TYPE_S_ROW)
		maxsize = maxsize / 2
	    maxrow = maxsize / tbpsta (tp, TBL_ROWLEN_CHAR)
	    maxrow = max (maxrow, 5)
	}

	return (maxrow)
end
