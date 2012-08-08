# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"symtab.h"

define	SZ_BLOCK	2048


# STSAVE -- Save the symbol table in an external binary file in a machine
# independent format.  This works provided only integer and character data
# is stored in the symbol table.

procedure stsave (stp, fd)

pointer	stp			# symtab descriptor
int	fd			# output file

int	nelem
int	ip, itop
errchk	miiwritei, miiwritec

begin
	call miiwritei (fd, Memi[stp], LEN_SYMTAB)
	call miiwritei (fd, Memi[ST_INDEX(stp)], ST_INDEXLEN(stp))

	# Since the symbol table can be very large, write it out in chunks
	# of a reasonable size to avoid allocating large buffers.

	itop = ST_STABP(stp) + ST_STABLEN(stp)
	for (ip=ST_STABP(stp);  ip < itop;  ip=ip+nelem) {
	    nelem = min (SZ_BLOCK, itop - ip)
	    call miiwritei (fd, Memi[ip], nelem)
	}

	# Ditto for the string buffer.

	itop = ST_SBUFP(stp) + ST_SBUFLEN(stp)
	for (ip=ST_SBUFP(stp);  ip < itop;  ip=ip+nelem) {
	    nelem = min (SZ_BLOCK, itop - ip)
	    call miiwritec (fd, Memc[ip], nelem)
	}
end
