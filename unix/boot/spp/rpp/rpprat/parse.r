include  defs

# PARSE - parse Ratfor source program

subroutine parse

include COMMON_BLOCKS
character lexstr(MAXTOK)
integer lab, labval(MAXSTACK), lextyp(MAXSTACK), sp, token, i, t
integer lex
logical	push_stack

	sp = 1
	lextyp(1) = EOF

	for (token = lex(lexstr);  token != EOF;  token = lex(lexstr)) {
	    push_stack = .false.

	    switch (token) {
	    case LEXIF:
		call ifcode (lab)
		push_stack = .true.
	    case LEXIFERR:
		call iferrc (lab, 1)
		push_stack = .true.
	    case LEXIFNOERR:
		call iferrc (lab, 0)
		push_stack = .true.
	    case LEXDO:
		call docode (lab)
		push_stack = .true.
	    case LEXWHILE:
		call whilec (lab)
		push_stack = .true.
	    case LEXFOR:
		call forcod (lab)
		push_stack = .true.
	    case LEXREPEAT:
		call repcod (lab)
		push_stack = .true.
	    case LEXSWITCH:
		call swcode (lab)
		push_stack = .true.
	    case LEXCASE, LEXDEFAULT:
		for (i=sp;  i > 0;  i=i-1)	# find for most recent switch
		    if (lextyp(i) == LEXSWITCH)
		       break
		if (i == 0)
		    call synerr ("illegal case or default.")
		else
		    call cascod (labval (i), token)
	    case LEXDIGITS:
		call labelc (lexstr)
		push_stack = .true.
	    case LEXELSE:
		t = lextyp(sp)
		if (t == LEXIF | t == LEXIFERR | t == LEXIFNOERR)
		    call elseif (labval(sp))
		else
		    call synerr ("Illegal else.")

		t = lex (lexstr)			# check for "else if"
		call pbstr (lexstr)
		if (t == LEXIF | t == LEXIFERR | t == LEXIFNOERR) {
		    call indent (-1)			# cancel out indent +1
		    token = LEXIFELSE			# prevent -indent at end
		}
		push_stack = .true.
	    case LEXTHEN:
		if (lextyp(sp) == LEXIFERR | lextyp(sp) == LEXIFNOERR) {
		    call thenco (lextyp(sp), labval(sp))
		    lab = labval(sp)
		    token = lextyp(sp)
		    sp = sp - 1			# cancel out subsequent push
		} else
		    call synerr ("Illegal 'then' clause in iferr statement.")
		push_stack = .true.
	    case LEXLITERAL:
		call litral
	    case LEXERRCHK:
		call errchk
	    case LEXBEGIN:
		call beginc
	    case LEXEND:
		call endcod (lexstr)
		if (sp != 1) {
		    call synerr ("Missing right brace or 'begin'.")
		    sp = 1
		}
	    default:
		if (token == LBRACE)
		    push_stack = .true.
		else if (token == LEXDECL)
		    call declco (lexstr)
	    }

	    if (push_stack) {
		if (body == NO) {
		    call synerr ("Missing 'begin' keyword.")
		    call beginc
		}
		sp = sp + 1				# beginning of statement
		if (sp > MAXSTACK)
		    call baderr ("Stack overflow in parser.")
		lextyp(sp) = token			# stack type and value
		labval(sp) = lab

	    } else if (token != LEXCASE & token != LEXDEFAULT) {
		if (token == RBRACE)
		    token = LEXRBRACE

		switch (token) {
		case LEXOTHER:
		    call otherc (lexstr)
		case LEXBREAK, LEXNEXT:
		    call brknxt (sp, lextyp, labval, token)
		case LEXRETURN:
		    call retcod
		case LEXGOTO:
		    call gocode
		case LEXSTRING:
		    if (body == NO)
			call strdcl
		    else
			call otherc (lexstr)
		case LEXRBRACE:
		    if (lextyp(sp) == LBRACE)
			sp = sp - 1
		    else if (lextyp(sp) == LEXSWITCH) {
			call swend (labval(sp))
			sp = sp - 1
		    } else
		       call synerr ("Illegal right brace.")
		}

		token = lex (lexstr)	   		# peek at next token
		call pbstr (lexstr)
		call unstak (sp, lextyp, labval, token)
	    }
	}

	if (sp != 1)
	    call synerr ("unexpected EOF.")
end
