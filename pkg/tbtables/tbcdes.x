include <tbset.h>
include "tbtables.h"

# tbcdes -- get the column selector descriptor
# This function returns the column selector descriptor corresponding to
# the input column pointer.  NULL will be returned if the column pointer
# does not match any selected column, or if there is no column selector.
#
# Phil Hodge,  3-Oct-1997  Function created.

pointer procedure tbcdes (tp, cp)

pointer tp		# i: pointer to table descriptor
pointer cp		# i: pointer to column descriptor
#--
pointer descrip		# column selector descriptor
int	colnum		# loop index for selected column number
pointer tcs_column()	# column pointer as function of descriptor

begin
	descrip = NULL

	if (TB_COLUMN_SELECT(tp) == YES) {	# column selector was used

	    do colnum = 1, TB_NSEL_COLS(tp) {	# all selected columns
		descrip = TB_SELCOL(tp,colnum)
		if (cp == tcs_column (descrip))
		    break			# found it
		else
		    descrip = NULL
	    }
	}

	return (descrip)
end
