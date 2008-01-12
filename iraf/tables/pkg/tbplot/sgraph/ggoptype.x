include <ctype.h>
include "sgraph.h"

# GG_OPTYPE -- Determine whether the operand argument is an image section
# or a list.  If the string is STDIN, it is a list; if a subscript is
# present, it is an image; otherwise we must test whether or not it is a
# binary file and make the decision based on that.

int procedure gg_optype (operand)

char	operand[SZ_LINE]	# Operand to be plotted

int	first, last, ip
char	imoper[SZ_FNAME]

int	access(), imaccess(), strncmp()

begin
	# Strip off any whitespace at the beginning or end of the string.
	for (ip=1;  IS_WHITE(operand[ip]);  ip=ip+1)
	    ;

	first = ip
	for (last=ip;  operand[ip] != EOS;  ip=ip+1)
	    if (!IS_WHITE(operand[ip]))
		last = ip

	# Tack on a dummy cluster (group) number to force imaccess() to 
	# treat it as an image (!??)
	call strcpy (operand[first], imoper, last-first+1)
	call strcat ("[1]", imoper, SZ_FNAME)

	if (strncmp (operand[first], "STDIN", 5) == 0)
	    return (LIST_OP)

	else if (operand[last] == ']')
	    return (IMAGE_OP)

	else if (imaccess (imoper, 0) == YES)
	    return (IMAGE_OP)

	else if (access (operand, 0, TEXT_FILE) == YES)
	    return (LIST_OP)

	else
	    return (IMAGE_OP)
end
