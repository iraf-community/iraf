include  defs

# caslab - get one case label

integer function caslab (n, t)

integer n, t
character tok(MAXTOK)
integer i, s, lev
integer gnbtok, ctoi

	t = gnbtok (tok, MAXTOK)
	while (t == NEWLINE)
	    t = gnbtok (tok, MAXTOK)

	if (t == EOF)
	    return (t)

	for (lev=0;  t == LPAREN;  t = gnbtok (tok, MAXTOK))
	    lev = lev + 1

	if (t == MINUS)
	    s = -1
	else
	    s = +1
	if (t == MINUS | t == PLUS)
	    t = gnbtok (tok, MAXTOK)

	if (t != DIGIT)
	    goto 99
	else {
	    i = 1
	    n = s * ctoi (tok, i)
	}

	for (t=gnbtok(tok,MAXTOK);  t == RPAREN;  t=gnbtok(tok,MAXTOK))
	    lev = lev - 1
	if (lev != 0)
	    goto 99

	while (t == NEWLINE)
	    t = gnbtok (tok, MAXTOK)

	return 0

 99	call synerr ("Invalid case label.")
	n = 0
end
