# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"symtab.h"

# STNEXT -- Return a symstruct pointer to the next most recently entered
# symbol in the table, given a pointer to some symbol.  Use to walk down
# the global symbol table list in the reverse of the order in which symbols
# were entered.

pointer procedure stnext (stp, sym)

pointer	stp			# symtab descriptor
pointer	sym			# pointer to 'current' symstruct
int	el

begin
	if (sym == NULL)
	    return (NULL)
	else {
	    el = E_NEXTGLOB (E_BASE(sym))
	    if (el == NULL)
		return (NULL)
	    else
		return (E_USERFIELDS (ST_STABP(stp) + el))
	}
end
