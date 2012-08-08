# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mii.h>
include	"symtab.h"

# STSIZE -- Compute the file storage space in chars required to store the
# symbol table, e.g., in a subsequent call to STSAVE.

int procedure stsize (stp)

pointer	stp			# symbol table descriptor

int	size
int	miipksize()

begin
	size = miipksize (LEN_SYMTAB + ST_INDEXLEN(stp) + ST_STABLEN(stp),
	    MII_LONG) + miipksize (ST_SBUFLEN(stp), MII_BYTE)

	return (size)
end
