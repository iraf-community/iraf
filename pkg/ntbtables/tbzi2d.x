include "tbtables.h"

# tbzi2d -- convert integer column to double
# When reading a text table into memory, if a floating-point value is
# found in a column of type integer, this routine may be called to convert
# the data type to double precision.
#
# Phil Hodge,  7-Jun-1994  Subroutine created.
# Phil Hodge, 10-Aug-1994  Update COL_LEN.

procedure tbzi2d (tp, cp)

pointer tp		# i: pointer to table descriptor
pointer cp		# i: pointer to column descriptor
#--
pointer new		# pointer to new memory for column data
int	row		# row number
errchk	malloc

begin
	# Allocate memory for the array of doubles.
	call malloc (new, TB_ALLROWS(tp), TY_DOUBLE)

	# Copy each row.
	do row = 1, TB_NROWS(tp) {

	    if (IS_INDEFI(Memi[COL_OFFSET(cp)+row-1])) {
		Memd[new+row-1] = INDEFD
	    } else {
		Memd[new+row-1] = Memi[COL_OFFSET(cp) + row - 1]
	    }
	}

	# Free the old memory, and save the new pointer.
	call mfree (COL_OFFSET(cp), TY_INT)
	COL_OFFSET(cp) = new

	# Specify the new data type and length.
	COL_DTYPE(cp) = TBL_TY_DOUBLE
	COL_LEN(cp) = SZ_DOUBLE
end
