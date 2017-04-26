# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<plio.h>


# PL_EMPTYLINE -- Return a pointer to the empty line for a mask.

pointer procedure pl_emptyline (pl)

pointer	pl			#I mask descriptor

begin
	return (Ref (pl, PL_EMPTYLINE))
end
