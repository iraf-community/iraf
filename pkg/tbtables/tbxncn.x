include "tbtables.h"

# tbxncn -- new column null
# Write INDEF values for each new column in each existing row of a table.
# This is called after defining new columns in an open table, but only if
# it contains some rows and the record length did not have to be increased.
#
# Phil Hodge, 30-Mar-1993  indef_rec is now TY_CHAR rather than TY_REAL.

procedure tbxncn (tp, old_colused, indef_rec)

pointer tp			# i: Pointer to table descriptor
int	old_colused		# i: Previous value of TB_COLUSED (unit=SZ_CHAR)
char	indef_rec[ARB]		# i: INDEF record buffer
#--
long	locn			# Location (chars) for writing in table
int	start			# Location in INDEF record of values to write
int	k			# Loop index
int	num_chars		# Number of chars to write as INDEF

begin
	num_chars = TB_COLUSED(tp) - old_colused

	start = old_colused + 1			# unit = SZ_CHAR
	locn = TB_BOD(tp) + old_colused		# incremented in loop
	do k = 1, TB_NROWS(tp) {
	    call seek (TB_FILE(tp), locn)
	    call write (TB_FILE(tp), indef_rec[start], num_chars)
	    locn = locn + TB_ROWLEN(tp)
	}
end
