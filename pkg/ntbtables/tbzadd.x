include <mach.h>		# for SZB_CHAR
include <tbset.h>
include "tbtables.h"

# tbzadd -- add new column for text file
# Allocate a buffer for a new column for a text file, and assign indef values.
# If the data type is not valid for text tables, the type will be changed.
#
# Phil Hodge, 14-Jan-1992  Subroutine created.
# Phil Hodge, 31-Mar-1993  Include check for short datatype.

procedure tbzadd (tp, cp)

pointer tp		# i: pointer to table descriptor
pointer cp		# i: pointer to column descriptor
#--
int	allrows		# number of allocated rows
int	row_1		# row number minus one
int	lenstr		# length of each string in a string column
int	size		# total number of char in a string column
errchk	malloc, calloc

begin
	allrows = TB_ALLROWS(tp)

	# Make sure the data type is valid for text tables.
	if (COL_DTYPE(cp) == TBL_TY_REAL)
	    COL_DTYPE(cp) = TBL_TY_DOUBLE
	else if (COL_DTYPE(cp) == TBL_TY_SHORT)
	    COL_DTYPE(cp) = TBL_TY_INT
	else if (COL_DTYPE(cp) == TBL_TY_BOOL)
	    COL_DTYPE(cp) = -8
	else if (COL_DTYPE(cp) == TY_CHAR)
	    # Old notation:  TY_CHAR instead of -N.
	    COL_DTYPE(cp) = -COL_LEN(cp) * SZB_CHAR

	COL_OFFSET(cp) = NULL		# initial null pointer

	# Allocate memory for column values.
	if (COL_DTYPE(cp) == TBL_TY_DOUBLE) {

	    call malloc (COL_OFFSET(cp), allrows, TY_DOUBLE)
	    do row_1 = 0, allrows-1			# zero indexed
		Memd[COL_OFFSET(cp) + row_1] = INDEFD

	} else if (COL_DTYPE(cp) == TBL_TY_INT) {

	    call malloc (COL_OFFSET(cp), allrows, TY_INT)
	    do row_1 = 0, allrows-1
		Memi[COL_OFFSET(cp) + row_1] = INDEFI

	} else if (COL_DTYPE(cp) < 0) {			# string

	    lenstr = -COL_DTYPE(cp) + 1			# add one for EOS
	    size = lenstr * allrows
	    call calloc (COL_OFFSET(cp), size, TY_CHAR)

	} else {
	    call error (1, "tbzadd:  invalid data type")
	}
end
