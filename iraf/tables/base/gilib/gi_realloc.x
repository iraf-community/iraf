include <imio.h>
include "gi.h"

# GI_REALLOC -- Procedure to reallocate space to keep the necessary
# stf parameter descrptor in memory.

procedure gi_realloc (im)

pointer	im		# Image descriptor

size_t	sz_val
pointer	stf

begin

	stf = IM_KDES(im)

	if (STF_PCOUNT(stf) > 0) {
	   sz_val = LEN_STFBASE + STF_PCOUNT(stf) * LEN_PDES
	   call realloc (stf, sz_val, TY_STRUCT)
	   IM_KDES(im) = stf
	}

end
