include "tbtables.h"

# tbcnum -- get column pointer from number
# This function returns the column pointer corresponding to a given
# column number, or NULL if the column number is out of range.
#
# Phil Hodge,  2-Mar-1998  Map selected column descriptor to actual descriptor.

pointer procedure tbcnum (tp, colnum)

pointer tp			# i: pointer to table descriptor
int	colnum			# i: column number (not pointer)
#--
pointer cp
pointer tcs_column()

begin
	# Value to be returned if column number is out of range.
	cp = NULL

	if (colnum < 1)
	    return (cp)

	if (TB_COLUMN_SELECT(tp) == YES) {	# column selector was used
	    if (colnum <= TB_NSEL_COLS(tp)) {
		cp = tcs_column (TB_SELCOL(tp,colnum))
	    }
	} else {				# column selector not used
	    if (colnum <= TB_NCOLS(tp)) {
		cp = TB_COLINFO(tp,colnum)
	    }
	}

	return (cp)
end
