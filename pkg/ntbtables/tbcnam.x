include <tbset.h>
include "tbtables.h"

# tbcnam -- change column name
# This procedure replaces the column name.  The column descriptor is
# updated, and if the table is not read-only the modified descriptor
# is also written back into the table.
#
# Phil Hodge, 19-Oct-1989  allow changing column name for a read-only table
# Phil Hodge,  3-Apr-1995  Set TB_MODIFIED to true.
# Phil Hodge,  9-Apr-1995  Modify for FITS tables.
# Phil Hodge, 14-Apr-1998  Change calling sequence of tbcwcd.

procedure tbcnam (tp, cp, colname)

pointer tp			# i: pointer to table descriptor
pointer cp			# i: pointer to a column descriptor
char	colname[ARB]		# i: column name
#--
errchk	tbcwcd, tbfnam

begin
	call strcpy (colname, COL_NAME(cp), SZ_COLNAME)

	if (!TB_READONLY(tp)) {
	    # Save modified column descriptor in table file.
	    if (TB_TYPE(tp) == TBL_TYPE_FITS)
		call tbfnam (tp, cp, colname)
	    else
		call tbcwcd (tp, cp)
	    TB_MODIFIED(tp) = true
	}
end
