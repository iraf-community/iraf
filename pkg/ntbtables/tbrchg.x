include <tbset.h>
include "tbtables.h"
include "tblerr.h"

# tbrchg -- change allocated number of rows
# For row-ordered tables this procedure does nothing.
#
# Phil Hodge,  6-Feb-1992  Include only section for column ordered.

procedure tbrchg (tp, allrows)

pointer tp			# Pointer to table descriptor
int	allrows			# The new value for the allocated number of rows

errchk	tbtchs

begin
	if (TB_TYPE(tp) == TBL_TYPE_S_COL) {
	    if (TB_IS_OPEN(tp)) {
		call tbtchs (tp, -1, -1, -1, allrows)
	    } else {
		TB_ALLROWS(tp) = allrows
	    }
	}
end
