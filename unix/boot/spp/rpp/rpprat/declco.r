include	defs

# DECLCO -- Process a declaration (xpp directive).  Look up directive in
# the symbol table.  If found, output the corresponding Fortran declaration,
# otherwise output the original string.

subroutine declco (id)

character id(MAXTOK)
character newid(MAXTOK), tok, tokbl
integer junk, ludef, equal, gettok
include COMMON_BLOCKS
string	xptyp XPOINTER
string	xpntr "x$pntr"
string	xfunc "x$func"
string	xsubr "x$subr"

	if (ludef (id, newid, xpptbl) == YES) {
	    if (equal (id, xpntr) == YES) {
		# Pointer declaration.
		tokbl = gettok (newid, MAXTOK)
		if (tokbl == BLANK)
		    tok = gettok (newid, MAXTOK)
		else
		    tok = tokbl

		if (tok == XPP_DIRECTIVE & equal (newid, xfunc) == YES) {
		    # Pointer function.
		    call outtab
		    call outstr (xptyp)
		    junk = ludef (newid, newid, xpptbl)
		    call outstr (newid)
		    call eatup
		    call outdon

		    call poicod (NO)

		} else {
		    # Pointer variable.
		    call pbstr (newid)
		    call poicod (YES)
		}

	    } else if (equal (id, xsubr) == YES) {
		# Subroutine declaration.
		call outtab
		call outstr (newid)
		call eatup
		call outdon

	    } else  {
		# Some other declaration.
		call outtab
		call outstr (newid)
		call outch (BLANK)
	    }

	} else
	    call synerr ("Invalid x$type type declaration.")
end
