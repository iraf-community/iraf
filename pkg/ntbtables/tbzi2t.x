include <mach.h>		# for SZB_CHAR
include "tbtables.h"

# tbzi2t -- convert integer column to text
# When reading a text table into memory, if non-numeric text is found in
# a column of type integer, this routine may be called to convert the
# data type to text.
#
# Phil Hodge,  7-Jun-1994  Subroutine created.
# Phil Hodge, 10-Aug-1994  Update COL_LEN.

procedure tbzi2t (tp, cp, width)

pointer tp		# i: pointer to table descriptor
pointer cp		# i: pointer to column descriptor
int	width		# i: the maximum width for this column
#--
pointer new		# pointer to new memory for column data
int	row		# row number
int	op		# offset in new char array
errchk	calloc

begin
	# Allocate memory for the array of strings.
	call calloc (new, (width+1) * TB_ALLROWS(tp), TY_CHAR)

	op = 0					# initial value

	# Copy each row.
	do row = 1, TB_NROWS(tp) {

	    if (IS_INDEFI(Memi[COL_OFFSET(cp)+row-1])) {
		Memc[new+op] = EOS
	    } else {
		call sprintf (Memc[new+op], width, "%d")
		    call pargi (Memi[COL_OFFSET(cp) + row - 1])
	    }

	    op = op + width + 1			# add one for EOS
	}

	# Free the old memory, and save the new pointer.
	call mfree (COL_OFFSET(cp), TY_INT)
	COL_OFFSET(cp) = new

	# Specify the new data type and width.
	COL_DTYPE(cp) = -width
	COL_LEN(cp) = (width + SZB_CHAR-1) / SZB_CHAR
end
