# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"lroff.h"

.help nofill
.nf __________________________________________________________________________
NOFILL -- Copy a block of text in ".nf" (nofill) mode, leaving the text
alone except for left justification.  The only directives recognized in
a nofill block are FI (resume filling) and RJ (right justify).
.endhelp _____________________________________________________________________

int procedure nofill (in, out, linebuf)

extern	in(), out()
char	linebuf[ARB]
int	ip, command
pointer	sp, rjbuf
int	in(), input(), nextcmd()
errchk	salloc, breakline, input, rjline, outline
include	"lroff.com"

begin
	call smark (sp)
	call salloc (rjbuf, SZ_IBUF, TY_CHAR)

	call breakline (out, NJ)

	while (input (in, linebuf) != EOF) {
	    command = nextcmd (linebuf, ip)
	    switch (command) {
	    case FI, ENDHELP:
		call sfree (sp)
		return (command)
	    case RJ:				# right justify text
		if (input (in, Memc[rjbuf]) == EOF)
		    break
		call rjline (out, Memc[rjbuf], linebuf[ip])
	    default:
		call outline (out, linebuf)
	    }
	}

	call sfree (sp)
	return (ENDHELP)
end
