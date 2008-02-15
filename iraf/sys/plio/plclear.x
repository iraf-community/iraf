# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<plio.h>

# PL_CLEAR -- Clear a mask.  The entire surface is cleared.  This is equivalent
# to a full surface pl_rop with rop=PIX_CLR, but is more convenient and can be
# implemented more efficiently since the entire surface is cleared.

procedure pl_clear (pl)

pointer	pl			#I mask descriptor

pointer	lp
int	n_len, i
errchk	realloc

begin
	# Clear the line list buffer.
	lp = Ref (pl, PL_EMPTYLINE)
	PL_LLOP(pl) = LP_BLEN(lp)
	LP_NREFS(lp) = PL_NLP(pl)

	do i = 1, PL_NLP(pl)
	    PL_LP(pl,i) = PL_EMPTYLINE
	    
	n_len = PL_LLBUFLEN
	call realloc (PL_LLBP(pl), n_len, TY_SHORT)

	PL_LLLEN(pl) = n_len
	PL_LLFREE(pl) = 0
	PL_LLNUPDATES(pl) = 0
end
