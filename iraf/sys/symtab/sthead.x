# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"symtab.h"

# STHEAD -- Return a symstruct pointer to the last symbol entered into the
# table.  The NULL pointer is returned when the symbol table is empty.

pointer procedure sthead (stp)

pointer	stp			# symtab descriptor

begin
	if (ST_LASTSYMBOL(stp) == NULL)
	    return (NULL)
	else
	    return (E_USERFIELDS (ST_STABP(stp) + ST_LASTSYMBOL(stp)))
end
