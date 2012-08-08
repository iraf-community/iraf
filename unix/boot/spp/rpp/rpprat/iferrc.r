include  defs

# IFERRC - Generate initial code for an IFERR statement.  Used to provide
# error recovery for a statement or compound statement.

subroutine iferrc (lab, sense)	

integer lab, sense
integer labgen, nlpar
character t, gettok, gnbtok, token(MAXTOK)
include COMMON_BLOCKS
string	errpsh "call xerpsh"
string	siferr "if (.not.xerpop()) "
string	sifnoerr "if (xerpop()) "

	xfer = NO
	lab = labgen (3)

	call outtab					# "call errpsh"
	call outstr (errpsh)
	call outdon

	switch (gnbtok (token, MAXTOK)) {		# "iferr (" or "iferr {"
	case LPAREN:
	    call outtab
	case LBRACE:
	    call pbstr (token)
	    esp = esp + 1
	    if (esp >= MAXERRSTK)			# not likely
		call baderr ("Iferr statements nested too deeply.")
	    errstk(esp) = lab
	    return
	default:
	    call synerr ("Missing left paren.")
	    return
	}

	nlpar = 1					# process "iferr (.."
	token(1) = EOS

	# Push handler on error stack temporarily so that "iferr (call error.."
	# can be handled properly.
	esp = esp + 1
	if (esp >= MAXERRSTK)				# not likely
	    call baderr ("Iferr statements nested too deeply.")
	errstk(esp) = 0
	
	repeat {					# output the statement
	    call outstr (token)
	    t = gettok (token, MAXTOK)
	    if (t == SEMICOL | t == LBRACE | t == RBRACE | t == EOF) {
		call pbstr (token)
		break
	    }
	    if (t == NEWLINE)      			# delete newlines
		token (1) = EOS
	    else if (t == LPAREN)
		nlpar = nlpar + 1
	    else if (t == RPAREN)
		nlpar = nlpar - 1
	    else if (t == SEMICOL) {
		call outdon
		call outtab
	    } else if (t == ALPHA)
		call squash (token)
	    # else nothing special
	} until (nlpar <= 0)
	
	esp = esp - 1
	ername = NO					# ignore errchk
	if (nlpar != 0)
	    call synerr ("Missing parenthesis in condition.")
	else
	    call outdon
	
	call outtab					# "if (errpop())"
	if (sense == 1)
	    call outstr (siferr)
	else
	    call outstr (sifnoerr)
	call outgo (lab)				# "... goto lab"

	call indent (1)
	return
end
