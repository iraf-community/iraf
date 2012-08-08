# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"symtab.h"

# STFREE -- Free storage back to the last marker.  No storage is actually
# returned to the system, rather the storage is made available for reuse
# by SYMTAB (the stacks are pruned).

procedure stfree (stp, marker)

pointer	stp			# symtab descriptor
int	marker			# magic marker

int	el
pointer	index, stab, ep, mp

begin
	index = ST_INDEX(stp)
	stab  = ST_STABP(stp)
	mp    = stab + marker
	ep    = NULL

	# Ignore requests with out of range markers.
	if (marker <= 0 || marker >= ST_STABOP(stp))
	    return

	# Descend the global (time ordered) list, unlinking each symbol until
	# the marker position is reached.

	for (el = ST_LASTSYMBOL(stp);  el > marker;  el = E_NEXTGLOB(ep)) {
	    ep = stab + el
	    Memi[index+E_THREAD(ep)] = E_NEXTHASH(ep)
	}

	# Reset the stack pointers and set the head of the global list to
	# point to the symbol immediately preceding the marker.

	ST_NSYMBOLS(stp) = M_NSYMBOLS(mp)
	ST_SBUFOP(stp)   = M_SBUFOP(mp)
	ST_STABOP(stp)   = marker

	if (ep != NULL)
	    ST_LASTSYMBOL(stp) = E_NEXTGLOB(ep)
end
