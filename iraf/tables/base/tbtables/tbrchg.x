include <tbset.h>
include "tbtables.h"
include "tblerr.h"

# tbrchg -- change allocated number of rows
# For row-ordered tables this procedure does nothing.
#
# Phil Hodge,  6-Feb-1992  Include only section for column ordered.

procedure tbrchg (tp, allrows)

pointer tp		# Pointer to table descriptor
long	allrows		# The new value for the allocated number of rows

long	l_val

errchk	tbtchs

begin
	if (TB_TYPE(tp) == TBL_TYPE_S_COL) {
	    if (TB_IS_OPEN(tp)) {
		l_val = -1
		call tbtchs (tp, -1, -1, l_val, allrows)
	    } else {
		TB_ALLROWS(tp) = allrows
	    }
	}
end
