# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <plset.h>
include	<plio.h>

# PL_OPEN -- Open a mask.  If the input buffer pointer is NULL an inactive
# mask descriptor is allocated, otherwise the pointer is taken to point to
# an encoded mask, which is decoded and loaded to create an active descriptor.

pointer procedure pl_open (smp)

pointer	smp			#I stored mask pointer or NULL

pointer	pl
errchk	calloc, pl_load

begin
	# Allocate and initialize an inactive descriptor.
	call calloc (pl, LEN_PLDES, TY_STRUCT)

	call amovki (1, PL_PLANE(pl,1), PL_MAXDIM)
	PL_MAGIC(pl) = PL_MAGICVAL
	PL_LLINC(pl) = PL_STARTINC

	# Load the saved mask, if any.
	if (smp != NULL)
	    call pl_load (pl, smp)

	return (pl)
end
