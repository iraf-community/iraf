# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ctype.h>
include	"lroff.h"

.help nextcmd
.nf ________________________________________________________________________
NEXTCMD -- Examine the input line; if it is an Lroff directive, return
the integer code of the directive, otherwise NULL.  Leave IP pointing
to the argument field if a command, otherwise leave it pointing at the
first char of the text line.  Note that the "directives" string must
match the opcode definitions given in lroff.h
.endhelp ___________________________________________________________________

define	SZ_OPCODE	2

int procedure nextcmd (linebuf, ip)

char	linebuf[ARB]
int	ip, op
char	opcode[SZ_OPCODE]
int	command, kwp, strmatch(), strncmp()
string	directives "finfjunjrjshihnhbrcespinlslebptpkskehrhn"

begin
	if (linebuf[1] != '.')			# not a command line?
	    return (NULL)
	if (strmatch (linebuf, "^.endhelp") > 0)
	    return (ENDHELP)
	ip = 2					# skip the '.'

	# Directives may be either upper or lower case.
	for (op=1;  op <= SZ_OPCODE;  op=op+1) {
	    opcode[op] = linebuf[ip]
	    if (IS_UPPER (opcode[op]))
		opcode[op] = TO_LOWER (opcode[op])
	    ip = ip + 1
	}

	# Just in case a directive happens to be longer than 2 chars, make
	# sure IP points past the directive name.
	while (IS_ALPHA (linebuf[ip]))
	    ip = ip + 1

	# Lookup directive, return opcode number if found.
	command = NULL
	for (kwp=1;  directives[kwp] != EOS;  kwp=kwp+SZ_OPCODE)
	    if (strncmp (opcode, directives[kwp], SZ_OPCODE) == 0) {
		command = (kwp+1) / SZ_OPCODE
		break
	    }

	if (command == NULL)			# unrecognized directive
	    ip = 1		
	return (command)
end
