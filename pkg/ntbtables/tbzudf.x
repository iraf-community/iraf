include <tbset.h>
include "tbtables.h"
include "tblerr.h"

# tbzudf -- set to undefined
# "Delete" entries in a table by setting each entry in the internal
# memory for the column to the INDEF value appropriate for its datatype.
# This version is for text tables.
#
# Phil Hodge,  3-Feb-1992  Subroutine created.

procedure tbzudf (tp, cp, numcols, rownum)

pointer tp			# i: pointer to table descriptor
pointer cp[numcols]		# i: array of pointers to column descriptors
int	numcols			# i: number of columns
int	rownum			# i: row number
#--
int	k			# Loop index
int	datatype		# Data type of a column
int	lenstr			# length of a string table element
int	ip			# offset to a string in Memc

begin
	do k = 1, numcols {

	    datatype = COL_DTYPE(cp[k])

	    if (datatype == TBL_TY_DOUBLE) {
		Memd[COL_OFFSET(cp[k]) + rownum - 1] = INDEFD

	    } else if (datatype == TBL_TY_INT) {
		Memi[COL_OFFSET(cp[k]) + rownum - 1] = INDEFI

	    } else if (datatype < 0) {
		lenstr = -COL_DTYPE(cp[k])		# not including EOS
		ip = (rownum - 1) * (lenstr + 1)	# including EOS
		Memc[COL_OFFSET(cp[k]) + ip] = EOS
	    } else {
		call error (ER_TBCOLBADTYP, "tbzudf:  bad datatype")
	    }
	}
end
