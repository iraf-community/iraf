# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <plset.h>
include	<plio.h>

# PL_LINENOTEMPTY -- Test whether the indicated mask image line is empty.

bool procedure pl_linenotempty (pl, v)

pointer	pl			#I mask descriptor
long	v[PL_MAXDIM]		#I coordinates of desired line

int	pl_reference()

begin
	return (pl_reference(pl,v) != PL_EMPTYLINE)
end
