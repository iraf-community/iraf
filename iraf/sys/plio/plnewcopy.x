# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<plset.h>
include	<plio.h>

# PL_NEWCOPY -- Create a new, empty mask with the same size and depth
# attributes as the reference mask.

pointer procedure pl_newcopy (old_pl)

pointer	old_pl			#I mask descriptor

pointer	new_pl
int	naxes, depth
long	axlen[PL_MAXDIM]
pointer	pl_open()
errchk	pl_open

begin
	new_pl = pl_open (NULL)

	call pl_gsize (old_pl, naxes, axlen, depth)
	call pl_ssize (new_pl, naxes, axlen, depth)

	PL_PRIVATE1(new_pl) = PL_PRIVATE1(old_pl)
	PL_PRIVATE2(new_pl) = PL_PRIVATE2(old_pl)
	PL_MAXLINE(new_pl)  = PL_MAXLINE(old_pl)

	return (new_pl)
end
