# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"symtab.h"

# STMARK -- Mark the top of the STAB and SBUF stacks so that storage may
# later be returned with STFREE.  Since two integers of storage are required
# for the mark, the extra information is saved at the current position in STAB.
# The location of this entry in STAB marks the position to which STAB is later
# to be restored.

procedure stmark (stp, marker)

pointer	stp			# symtab descriptor
int	marker			# magic marker

pointer	mp
int	stalloc()

begin
	marker = stalloc (stp, LEN_MARKER)
	mp = ST_STABP(stp) + marker

	M_SBUFOP(mp)	= ST_SBUFOP(stp)
	M_NSYMBOLS(mp)	= ST_NSYMBOLS(stp)
end
