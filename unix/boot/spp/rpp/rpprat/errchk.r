include defs

# ERRCHK -- Code called to process an ERRCHK declaration.

subroutine errchk

character tok, last_tok, gnbtok, token(MAXTOK)
integer	ntok
pointer	mktabl
include	COMMON_BLOCKS
string	serrcom1 "logical xerflg, xerpad(84)"
string	serrcom2 "common /xercom/ xerflg, xerpad"

	ntok = 0
	tok = 0

	repeat {
	    last_tok = tok
	    tok = gnbtok (token, MAXTOK)

	    switch (tok) {
	    case ALPHA:
		if (errtbl == NULL) {
		    errtbl = mktabl(0)			# make empty table
		    call outtab				# declare err flag
		    call outstr (serrcom1)
		    call outdon
		    call outtab				# declare err common
		    call outstr (serrcom2)
		    call outdon
		}
		call enter (token, 0, errtbl)		# enter keyw in table
	    case COMMA:
		# no action, but required by syntax
	    case NEWLINE:
		if (last_tok != COMMA)
		    break
	    default:
		call synerr ("Syntax error in ERRCHK declaration.")
	    }
	}
end
