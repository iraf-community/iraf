include  defs

# unstak - unstack at end of statement

define	IFSTMT		999


subroutine unstak (sp, lextyp, labval, token)

integer	labval(MAXSTACK), lextyp(MAXSTACK)
integer	sp, token, type

	for (;  sp > 1;  sp=sp-1) {
	    type = lextyp(sp)
	    if ((type == LEXIFERR | type == LEXIFNOERR) & token == LEXTHEN)
		break
	    if (type == LEXIF | type == LEXIFERR | type == LEXIFNOERR)
		type = IFSTMT
	    if (type == LBRACE | type == LEXSWITCH)
		break
	    if (type == IFSTMT & token == LEXELSE)
		break

	    if (type == IFSTMT) {
		call indent (-1)
		call outcon (labval(sp))
	    } else if (type == LEXELSE | type == LEXIFELSE) {
		if (sp > 2)
		    sp = sp - 1
		if (type != LEXIFELSE)
		    call indent (-1)
		call outcon (labval(sp) + 1)
	    } else if (type == LEXDO)
		call dostat (labval(sp))
	    else if (type == LEXWHILE)
		call whiles (labval(sp))
	    else if (type == LEXFOR)
		call fors (labval(sp))
	    else if (type == LEXREPEAT)
		call untils (labval(sp), token)
	}
end
