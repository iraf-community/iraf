# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"symtab.h"

# STNSYMBOLS -- Return the number of symbols in the symbol table or in a
# marked segment, i.e., the number of symbols added to the table since the
# mark was made.

int procedure stnsymbols (stp, marker)

pointer	stp			# symbol table pointer
int	marker			# stmark marker or 0 for entire table

pointer	mp

begin
	if (marker <= 0)
	    return (ST_NSYMBOLS(stp))
	else {
	    mp = ST_STABP(stp) + marker
	    return (ST_NSYMBOLS(stp) - M_NSYMBOLS(mp))
	}
end
