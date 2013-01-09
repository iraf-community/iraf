include <tbset.h>
include "tbtables.h"

# tbcnit -- change column units
# This procedure replaces the units for a column.  The column descriptor
# is updated, and if the table is not read-only the descriptor is also
# written back into the table.
#
# Phil Hodge, 19-Oct-1989  allow changing units for a read-only table
# Phil Hodge,  3-Apr-1995  Set TB_MODIFIED to true.
# Phil Hodge,  9-Apr-1995  Modify for FITS tables.
# Phil Hodge, 14-Apr-1998  Change calling sequence of tbcwcd.

procedure tbcnit (tp, cp, colunits)

pointer tp		# i: pointer to table descriptor
pointer cp		# i: pointer to a column descriptor
char	colunits[ARB]	# i: new value of units for column
#--
errchk	tbcwcd, tbfnit

begin
	call strcpy (colunits, COL_UNITS(cp), SZ_COLUNITS)

	if (!TB_READONLY(tp)) {
	    # Save modified column descriptor in table file.
	    if (TB_TYPE(tp) == TBL_TYPE_FITS)
		call tbfnit (tp, cp, colunits)
	    else
		call tbcwcd (tp, cp)
	    TB_MODIFIED(tp) = true
	}
end
