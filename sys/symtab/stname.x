# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"symtab.h"

# STNAME -- Return a char pointer to the key string (symbol name) of a symbol
# table entry, given a pointer to the symbol structure.

pointer procedure stname (stp, sym)

pointer	stp			# symtab descriptor
pointer	sym			# pointer to 'current' symstruct

begin
	return (ST_SBUFP(stp) + E_KEY(E_BASE(sym)))
end
