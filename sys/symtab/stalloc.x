# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"symtab.h"

# STALLOC -- Allocate a block of double aligned storage in the symbol table.
# Increase the size of STAB if overflow occurs.

int procedure stalloc (stp, blklen)

pointer	stp			# symtab descriptor
int	blklen			# number of integer units of storage
int	offset, buflen

begin
	offset = (ST_STABOP(stp) + 1) / 2 * 2
	buflen = ST_STABLEN(stp)

	if (offset + blklen > buflen) {
	    # Overflow has occurred.  Allocate a larger buffer; if overflow
	    # continues to occur the increments grow successively larger to
	    # minimize reallocation.

	    buflen = buflen + max (blklen, ST_STABINC(stp))
	    ST_STABINC(stp) = min (MAX_INCREMENT, ST_STABINC(stp) * INC_GROW)
	    ST_STABLEN(stp) = buflen
	    ST_STABNGROW(stp) = ST_STABNGROW(stp) + 1

	    call realloc (ST_STABP(stp), buflen, TY_STRUCT)
	}

	ST_STABOP(stp) = offset + ((blklen + 1) / 2 * 2)
	return (offset)
end
