# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"symtab.h"

# STCLOSE -- Return all storage used by a symbol table.

procedure stclose (stp)

pointer	stp			# symbol table descriptor

begin
	call mfree (ST_STABP(stp), TY_STRUCT)
	call mfree (ST_SBUFP(stp), TY_CHAR)
	call mfree (ST_INDEX(stp), TY_INT)
	call mfree (stp, TY_STRUCT)
end
