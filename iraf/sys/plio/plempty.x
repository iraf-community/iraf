# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<plio.h>

# PL_EMPTY -- Test whether a mask is empty, i.e., contains no nonzero pixels.

bool procedure pl_empty (pl)

pointer	pl			#I mask descriptor
int	i

begin
	# Mask is empty if all lines are empty.
	do i = 1, PL_NLP(pl)
	    if (PL_LP(pl,i) != PL_EMPTYLINE)
		return (false)

	return (true)

	# The following also works, but the call to pl_compress is an
	# unintended side effect which it is best to avoid.

	# call pl_compress (pl)
	# return (PL_LLOP(pl) == LP_BLEN(Ref(pl,0)))
end
