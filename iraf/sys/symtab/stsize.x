# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mii.h>
include	"symtab.h"

# STSIZE -- Compute the file storage space in chars required to store the
# symbol table, e.g., in a subsequent call to STSAVE.

int procedure stsize (stp)

pointer	stp			# symbol table descriptor

size_t	sz1, sz2, sz3
int	size
size_t	miipksize()

begin
	sz1 = LEN_SYMTAB + ST_STABLEN(stp)
	sz2 = ST_INDEXLEN(stp)
	sz3 = ST_SBUFLEN(stp)
	size = miipksize (sz1, MII_LONGLONG) + 
		miipksize (sz2, MII_INT) + miipksize (sz3, MII_BYTE)

	return (size)
end
