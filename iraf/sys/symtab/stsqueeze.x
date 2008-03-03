# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"symtab.h"

# STSQUEEZE -- Return any unused storage in a symbol table.  This is useful
# when it is known that no more symbols will be entered in a table, or before
# saving a symbol table in a save file.

procedure stsqueeze (stp)

pointer	stp			# symtab descriptor
size_t	sz_val

begin
	if (ST_STABLEN(stp) > ST_STABOP(stp)) {
	    ST_STABLEN(stp) = ST_STABOP(stp)
	    ST_STABINC(stp) = min (MAX_INCREMENT, ST_STABLEN(stp))
	    sz_val = ST_STABLEN(stp)
	    call realloc (ST_STABP(stp), sz_val, TY_STRUCT)
	}

	if (ST_SBUFLEN(stp) > ST_SBUFOP(stp)) {
	    ST_SBUFLEN(stp) = ST_SBUFOP(stp)
	    ST_SBUFINC(stp) = min (MAX_INCREMENT, ST_SBUFLEN(stp))
	    sz_val = ST_SBUFLEN(stp)
	    call realloc (ST_SBUFP(stp), sz_val, TY_CHAR)
	}
end
