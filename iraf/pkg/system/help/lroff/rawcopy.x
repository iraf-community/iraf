# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"lroff.h"

.help rawcopy
.nf ________________________________________________________________________
RAWCOPY -- Copy an unformatted help block without modification, except
for moving to the desired left margin.  Stop only when the .endhelp
directive is seen, or at EOF.  Ignore all other directives.
.endhelp ___________________________________________________________________

procedure rawcopy (in, out, linebuf)

extern	in(), out()
char	linebuf[ARB]
int	ip, in(), input(), nextcmd()
errchk	input, outline
include	"lroff.com"

begin
	while (input (in, linebuf) != EOF)
	    if (nextcmd (linebuf, ip) == ENDHELP)
		break
	    else
		call outline (out, linebuf)
end
