include <mach.h>
include <tbset.h>
include "tbtables.h"
include "tblerr.h"

# tbcchg -- change allocated row length
# This procedure is for changing the number of allocated columns, i.e.
# the row length.
# The allocated space for column descriptors will also be increased, if
# necessary, to allow defining a single-precision column for each SZ_REAL
# added to the row length.
# For column-ordered tables this procedure does nothing.
#
# Phil Hodge,  6-Feb-1992  Include only section for row ordered.

procedure tbcchg (tp, rowlen)

pointer tp			# Pointer to table descriptor
long	rowlen			# The new value for the row length in SZ_CHAR

int	maxcols			# New value for TB_MAXCOLS
long	l_val
errchk	tbtchs

begin
	if (TB_TYPE(tp) == TBL_TYPE_S_ROW) {
	    if (TB_IS_OPEN(tp)) {
		l_val = TB_NCOLS(tp) + (rowlen - TB_COLUSED(tp)) / SZ_REAL
		if ( l_val > MAX_INT ) {
		    call error (1, "tbcchg: too large rowlen")
		}
		maxcols = l_val
		# maxcols < 0 means that tbtchs should not change TB_MAXCOLS.
		if (maxcols <= TB_MAXCOLS(tp))
		    maxcols = -1
		l_val = -1
		call tbtchs (tp, -1, maxcols, rowlen, l_val)
	    } else {
		TB_ROWLEN(tp) = rowlen
	    }
	}
end
