include  defs

# GOCODE - generate code for goto statement

subroutine gocode

character token (MAXTOK), t
character gnbtok
integer	ctoi, i
include	COMMON_BLOCKS

	t = gnbtok (token, MAXTOK)
	if (t != DIGIT)
	    call synerr ("Invalid label for goto.")
	else {
	    call outtab
	    i = 1
	    call ogotos (ctoi(token,i), NO)
	}
	xfer = YES

	for (t=gnbtok(token,MAXTOK);  t == NEWLINE;  t=gnbtok(token,MAXTOK))
	    ;
	call pbstr (token)
end
