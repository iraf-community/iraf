# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <plset.h>
include	<plio.h>

# PL_GSIZE -- Get the dimensionality and size of a mask.

procedure pl_gsize (pl, naxes, axlen, depth)

pointer	pl			#I mask descriptor
int	naxes			#O number of axes (dimensionality of mask)
long	axlen[ARB]		#O length of each axis
int	depth			#O mask depth, bits

size_t	sz_val
int	i

begin
	naxes = PL_NAXES(pl)
	sz_val = PL_MAXDIM
	call amovl (PL_AXLEN(pl,1), axlen, sz_val)

	do i = 0, ARB
	    if (2**i > min (I_PVMAX, PL_MAXVAL(pl))) {
		depth = i
		break
	    }
end
