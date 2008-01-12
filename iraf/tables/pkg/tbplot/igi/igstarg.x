include <ctype.h>
include "igi.h"

procedure igstarg (igs, rline, maxch)

pointer	igs			# igi parameters structure
char	rline[ARB]
int	maxch

int	in			# Input command line
int	ip, first, last
int	ech
pointer	tokvals

int	getline()

begin
	rline[1] = EOS

	in = INPUT_SOURCE(igs)
	if (getline (in, rline) == EOF)
	    return

	# Strip white leading space
	for (ip = 1;  rline[ip] != EOS && IS_WHITE(rline[ip]);  ip = ip + 1)
	    ;

	if (rline[ip] == EOS) {
	    return
	} else if (rline[ip] == '\"' || rline[ip] == '\'') {
	    ech = rline[ip]
	    ip = ip + 1
	} else
	    ech = ';'

	first = ip

	# Search for the end of the string
	while (rline[ip] != EOS && rline[ip] != ech && rline[ip] != '\n')
	    ip = ip + 1

	last = ip

	if (ech != ';' && rline[ip] == '\n')
	    call eprintf ("Missing close quote in string constant ")

	# Pushback any possible commands at the end of the string.
	if (rline[ip] != EOS && rline[ip+1] != EOS)
	    call ungetline (in, rline[ip+1])
	
	# Strip off delimiters
	for (ip = first;  rline[ip] != EOS;  ip = ip + 1)
	    rline[ip-first+1] = rline[ip]
	rline[last-first+1] = EOS

	# Stuff the argument in the token value buffer
	tokvals = TOKEN_VALUE(igs)
	call tkalop (tokvals, maxch)
	call strcpy (rline, LOP_VALC(tokvals), maxch)

	# Update the last command buffer
	call strcat (" \"", LAST_COMMAND(igs), SZ_LINE)
	call strcat (rline, LAST_COMMAND(igs), SZ_LINE)
	call strcat ("\"",  LAST_COMMAND(igs), SZ_LINE)
end
