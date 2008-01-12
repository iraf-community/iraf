# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<chars.h>
include	"lroff.h"

.help lgetarg
.nf _________________________________________________________________________
LGETARG -- Get an integer argument to a directive.  If no argument is found,
return the default value.  We are called with IP pointing to the start of
the argument field to be searched.  Leave IP pointing to the next argument
field.
.endhelp ____________________________________________________________________

int procedure lgetarg (input_line, ip, default_value)

char	input_line[ARB]
int	ip, default_value
int	argument
int	ctoi()

begin
	if (ctoi (input_line, ip, argument) == 0)
	    argument = default_value

	# Eat comma argument delimiter, if multiple arguments.  Also eat
	# trailing whitespace, in case a string argument follows.
	while (input_line[ip] == BLANK)
	    ip = ip + 1
	if (input_line[ip] == ',')
	    ip = ip + 1
	while (input_line[ip] == BLANK)
	    ip = ip + 1

	return (argument)
end
